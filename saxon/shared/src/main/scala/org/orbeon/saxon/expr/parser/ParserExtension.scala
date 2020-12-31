package org.orbeon.saxon.expr.parser

import java.{util => ju}

import org.orbeon.saxon.expr._
import org.orbeon.saxon.expr.instruct.{SlotManager, UserFunction, UserFunctionParameter}
import org.orbeon.saxon.expr.parser.ParserExtension._
import org.orbeon.saxon.functions.hof.{FunctionLiteral, PartialApply, UnresolvedXQueryFunctionItem, UserFunctionReference}
import org.orbeon.saxon.functions.registry.{BuiltInFunctionSet, XPath31FunctionSet}
import org.orbeon.saxon.functions.{FunctionLibrary, SystemFunction}
import org.orbeon.saxon.lib.NamespaceConstant
import org.orbeon.saxon.model._
import org.orbeon.saxon.om.{Function, Sequence, StructuredQName}
import org.orbeon.saxon.query.{AnnotationList, XQueryFunction, XQueryParser}
import org.orbeon.saxon.style.StylesheetPackage
import org.orbeon.saxon.trans._
import org.orbeon.saxon.value._
import org.orbeon.saxon.z.IntSet

import scala.collection.mutable
//import scala.collection.compat._
import scala.jdk.CollectionConverters._
import scala.util.control.Breaks._

object ParserExtension {

  private def makeNamedFunctionReference(functionName: StructuredQName, fcf: Function): Expression =
    fcf match {
      case function: UserFunction if !functionName.hasURI(
        NamespaceConstant.XSLT) =>
        new UserFunctionReference(function)
      case item: UnresolvedXQueryFunctionItem =>
        item.getFunctionReference
      case _ =>
        new FunctionLiteral(fcf)
    }

  class InlineFunctionDetails {

    var outerVariables: mutable.Stack[LocalBinding] = _

    var outerVariablesUsed: ju.List[LocalBinding] = _

    var implicitParams: ju.List[UserFunctionParameter] = _

  }

  def makeInlineFunctionValue(p: XPathParser,
                              annotations: AnnotationList,
                              details: InlineFunctionDetails,
                              params: ju.List[UserFunctionParameter],
                              resultType: SequenceType,
                              body: Expression): Expression = {
    val arity: Int = params.size
    val uf: UserFunction = new UserFunction()
    uf.setFunctionName(
      new StructuredQName("anon",
        NamespaceConstant.ANONYMOUS,
        "f_" + uf.hashCode))
    uf.setPackageData(p.getStaticContext.getPackageData)
    uf.setBody(body)
    uf.setAnnotations(annotations)
    uf.setResultType(resultType)
    uf.incrementReferenceCount()
    uf.getPackageData match {
      case pack: StylesheetPackage =>
        val comp: Component = Component.makeComponent(
          uf,
          Visibility.PRIVATE,
          VisibilityProvenance.DEFAULTED,
          pack,
          pack)
        uf.setDeclaringComponent(comp)
      case _ =>
    }
    var result: Expression = null
    val implicitParams = details.implicitParams
    if (!implicitParams.isEmpty) {
      val extraParams: Int = implicitParams.size
      val expandedArity: Int = params.size + extraParams
      val paramArray = Array.ofDim[UserFunctionParameter](expandedArity)
      for (i <- 0 until params.size) {
        paramArray(i) = params.get(i)
      }
      var k: Int = params.size
      for (implicitParam <- implicitParams.asScala) {
        paramArray({
          k += 1; k - 1
        }) = implicitParam
      }
      uf.setParameterDefinitions(paramArray)
      val stackFrame: SlotManager =
        p.getStaticContext.getConfiguration.makeSlotManager
      for (i <- 0 until expandedArity) {
        val slot: Int =
          stackFrame.allocateSlotNumber(paramArray(i).getVariableQName)
        paramArray(i).setSlotNumber(slot)
      }
      ExpressionTool.allocateSlots(body, expandedArity, stackFrame)
      uf.setStackFrameMap(stackFrame)
      result = new UserFunctionReference(uf)
      val partialArgs = Array.ofDim[Expression](expandedArity)
      for (i <- 0 until arity) {
        partialArgs(i) = null
      }
      for (ip <- 0 until implicitParams.size) {
        val ufp: UserFunctionParameter = implicitParams.get(ip)
        val binding: LocalBinding = details.outerVariablesUsed.get(ip)
        var `var`: VariableReference = null
        if (binding.isInstanceOf[TemporaryXSLTVariableBinding]) {
          `var` = new LocalVariableReference(binding)
          /* binding
             .asInstanceOf[TemporaryXSLTVariableBinding]
             .declaration
             .registerReference(`var`)*/
          // no class found
        } else {
          `var` = new LocalVariableReference(binding)
        }
        `var`.setStaticType(binding.getRequiredType, null, 0)
        ufp.setRequiredType(binding.getRequiredType)
        partialArgs(ip + arity) = `var`
      }
      result = new PartialApply(result, partialArgs)
    } else {
      val paramArray = params.toArray(Array.ofDim[UserFunctionParameter](0))
      uf.setParameterDefinitions(paramArray)
      val stackFrame: SlotManager =
        p.getStaticContext.getConfiguration.makeSlotManager
      for (param <- paramArray) {
        stackFrame.allocateSlotNumber(param.getVariableQName)
      }
      ExpressionTool.allocateSlots(body, params.size, stackFrame)
      uf.setStackFrameMap(stackFrame)
      result = new UserFunctionReference(uf)
    }
    uf.getPackageData match {
      case stylesheetPackage: StylesheetPackage =>
        stylesheetPackage.addComponent(uf.getDeclaringComponent)
      case _ =>
    }
    result
  }

  class TemporaryXSLTVariableBinding(/*decl: SourceBinding*/) extends LocalBinding {

    //var declaration: SourceBinding = decl // no scala class found

    def getRequiredType: SequenceType = new SequenceType()

    def evaluateVariable(context: XPathContext): Sequence =
      throw new UnsupportedOperationException

    def isGlobal: Boolean = false

    def isAssignable: Boolean = false

    def getLocalSlotNumber: Int = 0

    def getVariableQName: StructuredQName = new StructuredQName("", "", "")

    def addReference(ref: VariableReference,
                     isLoopingReference: Boolean): Unit = ()

    def getIntegerBoundsForVariable: Array[IntegerValue] = null

    override def setIndexedVariable(): Unit = ()

    override def isIndexedVariable: Boolean = false

  }

  def curryFunction(functionExp: Expression,
                    args: Array[Expression],
                    placeMarkers: IntSet): Expression = {
    val ii = placeMarkers.iterator
    while (ii.hasNext)
      args(ii.next()) = null
    new PartialApply(functionExp, args)
  }

  def findOuterRangeVariable(qName: StructuredQName,
                             inlineFunctionStack: List[InlineFunctionDetails],
                             env: StaticContext): LocalBinding = {
    var b2 = findOuterXPathRangeVariable(qName, inlineFunctionStack)
    if (b2 != null)
      return b2
    if ( /*env.isInstanceOf[ExpressionContext] &&*/ inlineFunctionStack.nonEmpty) // no class found
      b2 = findOuterXSLTVariable(qName, inlineFunctionStack, env)
    b2
  }

  private def findOuterXPathRangeVariable(qName: StructuredQName,
                                          inlineFunctionStack: List[InlineFunctionDetails]): LocalBinding = {
    var s: Int = inlineFunctionStack.size - 1
    while (s >= 0) {
      var details: InlineFunctionDetails = inlineFunctionStack(s)
      val outerVariables: mutable.Stack[LocalBinding] = details.outerVariables
      var v: Int = outerVariables.size - 1
      while (v >= 0) {
        var b2: LocalBinding = outerVariables(v)
        if (b2.getVariableQName == qName) {
          var bs: Int = s
          while (bs <= inlineFunctionStack.size - 1) {
            details = inlineFunctionStack(bs)
            var found = false
            breakable {
              for (p <- 0 until details.outerVariablesUsed.size - 1
                   if details.outerVariablesUsed.get(p) == b2) {
                b2 = details.implicitParams.get(p)
                found = true
                break()
              }
            }
            if (!found) {
              details.outerVariablesUsed.add(b2)
              val ufp: UserFunctionParameter = new UserFunctionParameter()
              ufp.setVariableQName(qName)
              ufp.setRequiredType(b2.getRequiredType)
              details.implicitParams.add(ufp)
              b2 = ufp
            }
            bs += 1
          }
          return b2
        }
        v -= 1
      }
      val b2: LocalBinding =
        bindParametersInNestedFunctions(qName, inlineFunctionStack, s)
      if (b2 != null) return b2
      s -= 1
    }
    null
  }

  private def bindParametersInNestedFunctions(qName: StructuredQName,
                                              inlineFunctionStack: List[InlineFunctionDetails],
                                              start: Int): LocalBinding = {
    var details = inlineFunctionStack(start)
    val params = details.implicitParams
    for (param <- params.asScala if param.getVariableQName == qName) {
      var b2: LocalBinding = param
      var bs: Int = start + 1
      while (bs <= inlineFunctionStack.size - 1) {
        details = inlineFunctionStack(bs)
        var found: Boolean = false
        breakable {
          for (p <- 0 until details.outerVariablesUsed.size - 1
               if details.outerVariablesUsed.get(p) == param) {
            b2 = details.implicitParams.get(p)
            found = true
            break()
          }
        }
        if (!found) {
          details.outerVariablesUsed.add(param)
          val ufp: UserFunctionParameter = new UserFunctionParameter()
          ufp.setVariableQName(qName)
          ufp.setRequiredType(param.getRequiredType)
          details.implicitParams.add(ufp)
          b2 = ufp
        }
          bs += 1
      }
      if (b2 != null) {
        return b2
      }
    }
    null
  }

  private def findOuterXSLTVariable(qName: StructuredQName,
                                    inlineFunctionStack: List[InlineFunctionDetails],
                                    env: StaticContext): LocalBinding = {
    /*val decl: SourceBinding = env
      .asInstanceOf[ExpressionContext]
      .getStyleElement
      .bindLocalVariable(qName)
    if (decl != null) {
      val details: InlineFunctionDetails = inlineFunctionStack.get(0)
      var innermostBinding: LocalBinding = null
      var found: Boolean = false
      for (p <- 0 until details.outerVariablesUsed.size
           if details.outerVariablesUsed.get(p).getVariableQName == qName) {
        found = true
        //break
      }
      if (!found) {
        details.outerVariablesUsed.add(new TemporaryXSLTVariableBinding(decl))
        val ufp: UserFunctionParameter = new UserFunctionParameter()
        ufp.setVariableQName(qName)
        ufp.setRequiredType(decl.getInferredType(true))
        details.implicitParams.add(ufp)
      }
      innermostBinding =
        bindParametersInNestedFunctions(qName, inlineFunctionStack, 0)
      if (innermostBinding != null) {
        innermostBinding
      }
    }*/
    null
  }

}

class ParserExtension {

  var inlineFunctionStack: List[InlineFunctionDetails] = Nil

  private def needExtension(p: XPathParser, what: String): Unit =
    p.grumble(what + " require support for Saxon extensions, available in Saxon-PE or higher")

  private def needUpdate(p: XPathParser, what: String): Unit =
    p.grumble(what + " requires support for XQuery Update, available in Saxon-EE or higher")

  def parseNamedFunctionReference(p: XPathParser): Expression = {
    val t: Tokenizer = p.getTokenizer
    val fname: String = t.currentTokenValue
    val offset: Int = t.currentTokenStartOffset
    val env: StaticContext = p.getStaticContext
    p.nextToken()
    p.expect(Token.NUMBER)

    val number = NumericValue.parseNumber(t.currentTokenValue)
    if (! number.isInstanceOf[IntegerValue])
      p.grumble("Number following '#' must be an integer")

    if (number.compareTo(0) < 0 || number.compareTo(java.lang.Integer.MAX_VALUE) > 0)
      p.grumble("Number following '#' is out of range", "FOAR0002")

    val arity = number.longValue.toInt
    p.nextToken()
    var functionName: StructuredQName = null
    try {
      functionName =
        p.getQNameParser.parse(fname, env.getDefaultFunctionNamespace)
      if (functionName.getPrefix.==("")) {
        if (XPathParser.isReservedFunctionName31(functionName.getLocalPart)) {
          p.grumble(
            "The unprefixed function name '" + functionName.getLocalPart +
              "' is reserved in XPath 3.1")
        }
      }
    } catch {
      case e: XPathException =>
        p.grumble(e.getMessage, e.getErrorCodeLocalPart)
        assert(functionName != null)
    }
    var fcf: Function = null
    try {
      val lib: FunctionLibrary = env.getFunctionLibrary
      val sn: SymbolicName.F = new SymbolicName.F(functionName, arity)
      fcf = lib.getFunctionItem(sn, env)
      if (fcf == null) {
        p.grumble(
          "Function " + functionName.getEQName + "#" + arity + " not found",
          "XPST0017",
          offset)
      }
    } catch {
      case e: XPathException => p.grumble(e.getMessage, "XPST0017", offset)

    }
    if (functionName.hasURI(NamespaceConstant.FN) && fcf.isInstanceOf[SystemFunction]) {
      val details: BuiltInFunctionSet.Entry = fcf.asInstanceOf[SystemFunction].getDetails
      if (details != null && (details.properties & (BuiltInFunctionSet.FOCUS | BuiltInFunctionSet.DEPENDS_ON_STATIC_CONTEXT)) != 0) {
        val lookup: SystemFunction =
          XPath31FunctionSet.getInstance.makeFunction("function-lookup", 2)
        lookup.setRetainedStaticContext(env.makeRetainedStaticContext())
        return lookup.makeFunctionCall(
          Literal.makeLiteral(
            new QNameValue(functionName, BuiltInAtomicType.QNAME)),
          Literal.makeLiteral(Int64Value.makeIntegerValue(arity)))
      }
    }
    val ref: Expression = makeNamedFunctionReference(functionName, fcf)
    p.setLocation(ref, offset)
    ref
  }

  def parseFunctionItemType(p: XPathParser,
                            annotations: AnnotationList): ItemType = {
    val t: Tokenizer = p.getTokenizer
    p.nextToken()
    val argTypes = new ju.ArrayList[SequenceType](3)
    var resultType: SequenceType = null
    if (t.currentToken == Token.STAR || t.currentToken == Token.MULT) {
      p.nextToken()
      p.expect(Token.RPAR)
      p.nextToken()
      if (annotations.isEmpty) {
        AnyFunctionType
      } else {
        new AnyFunctionTypeWithAssertions(annotations,
          p.getStaticContext.getConfiguration)
      }
    } else {
      breakable {
        while (t.currentToken != Token.RPAR) {
          val arg: SequenceType = p.parseSequenceType
          argTypes.add(arg)
          if (t.currentToken == Token.RPAR) {
            break()
          } else if (t.currentToken == Token.COMMA) {
            p.nextToken()
          } else {
            p.grumble(
              "Expected ',' or ')' after function argument type, found '" +
                Token.tokens(t.currentToken) +
                '\'')
          }
        }
      }
      p.nextToken()
      if (t.currentToken == Token.AS) {
        p.nextToken()
        resultType = p.parseSequenceType
        var argArray = Array.ofDim[SequenceType](argTypes.size)
        argArray = argTypes.toArray(argArray)
        new SpecificFunctionType(argArray, resultType, annotations)
      } else if (!argTypes.isEmpty) {
        p.grumble(
          "Result type must be given if an argument type is given: expected 'as (type)'")
        null
      } else {
        p.grumble(
          "function() is no longer allowed for a general function type: must be function(*)")
        null
      }
    }
  }

  def parseExtendedItemType(p: XPathParser): ItemType = {
    val t: Tokenizer = p.getTokenizer
    if (t.currentToken == Token.NODEKIND && t.currentTokenValue.==("tuple")) {
      needExtension(p, "Tuple types")
    } else if (t.currentToken == Token.NODEKIND && t.currentTokenValue.==(
      "union")) {
      needExtension(p, "Inline union types")
    }
    null
  }

  def parseTypePattern(p: XPathParser): Expression = {
    needExtension(p, "type-based patterns")
    null
  }

  def makeArgumentPlaceMarker(p: XPathParser): Expression = null

  def parseInlineFunction(p: XPathParser, annotations: AnnotationList): Expression = {
    val t = p.getTokenizer
    val offset = t.currentTokenStartOffset
    val details = new InlineFunctionDetails()

    details.outerVariables = new mutable.Stack[LocalBinding]
    for (lb <- p.getRangeVariables)
      details.outerVariables.push(lb)
    details.outerVariablesUsed = new ju.ArrayList(4)
    details.implicitParams = new ju.ArrayList(4)

    inlineFunctionStack ::= details

    p.setRangeVariables(new mutable.Stack[LocalBinding])
    p.nextToken()
    val paramNames = new ju.HashSet[StructuredQName](8)
    val params = new ju.ArrayList[UserFunctionParameter](8)
    var resultType = SequenceType.ANY_SEQUENCE
    var paramSlot = 0
    breakable {
      while (t.currentToken != Token.RPAR) {
        p.expect(Token.DOLLAR)
        p.nextToken()
        p.expect(Token.NAME)

        val argName: String = t.currentTokenValue
        val argQName = p.makeStructuredQName(argName, "")
        if (paramNames.contains(argQName))
          p.grumble("Duplicate parameter name " + Err.wrap(t.currentTokenValue, Err.VARIABLE), "XQST0039")
        paramNames.add(argQName)
        var paramType = SequenceType.ANY_SEQUENCE
        p.nextToken()
        if (t.currentToken == Token.AS) {
          p.nextToken()
          paramType = p.parseSequenceType
        }
        val arg = new UserFunctionParameter()
        arg.setRequiredType(paramType)
        arg.setVariableQName(argQName)
        arg.setSlotNumber(paramSlot)
        paramSlot += 1
        params.add(arg)
        p.declareRangeVariable(arg)
        if (t.currentToken == Token.RPAR) {
          break()
        } else if (t.currentToken == Token.COMMA) {
          p.nextToken()
        } else {
          p.grumble("Expected ',' or ')' after function argument, found '" + Token.tokens(t.currentToken) + '\'')
        }
      }
    }
    t.setState(Tokenizer.BARE_NAME_STATE)
    p.nextToken()
    if (t.currentToken == Token.AS) {
      t.setState(Tokenizer.SEQUENCE_TYPE_STATE)
      p.nextToken()
      resultType = p.parseSequenceType
    }
    p.expect(Token.LCURLY)
    t.setState(Tokenizer.DEFAULT_STATE)
    p.nextToken()
    val body =
      if (t.currentToken == Token.RCURLY && p.isAllowXPath31Syntax) {
        t.lookAhead()
        p.nextToken()
        Literal.makeEmptySequence
      } else {
        val body = p.parseExpression
        p.expect(Token.RCURLY)
        t.lookAhead()
        p.nextToken()
        body
      }

    ExpressionTool.setDeepRetainedStaticContext(body, p.getStaticContext.makeRetainedStaticContext())

    val arity = paramNames.size
    for (_ <- 0 until arity)
      p.undeclareRangeVariable()

    val result =
      makeInlineFunctionValue(p,
        annotations,
        details,
        params,
        resultType,
        body
      )
    p.setLocation(result, offset)
    p.setRangeVariables(details.outerVariables)

    inlineFunctionStack = inlineFunctionStack.tail
    result
  }

  def parseDotFunction(p: XPathParser): Expression = {
    needExtension(p, "Dot functions")
    null
  }

  def parseUnderscoreFunction(p: XPathParser): Expression = {
    needExtension(p, "Underscore functions")
    null
  }

  def bindNumericParameterReference(p: XPathParser): Expression = {
    needExtension(p, "Underscore functions")
    null
  }

  def makeCurriedFunction(parser: XPathParser,
                          offset: Int,
                          name: StructuredQName,
                          args: Array[Expression],
                          placeMarkers: IntSet): Expression = {

    val env = parser.getStaticContext
    val lib = env.getFunctionLibrary
    val sn = new SymbolicName.F(name, args.length)

    val target = lib.getFunctionItem(sn, env)
    if (target == null)
      return parser.reportMissingFunction(offset, name, args, new ju.ArrayList)

    val targetExp = makeNamedFunctionReference(name, target)
    parser.setLocation(targetExp, offset)
    curryFunction(targetExp, args, placeMarkers)
  }

  def findOuterRangeVariable(p: XPathParser, qName: StructuredQName): LocalBinding =
    ParserExtension.findOuterRangeVariable(qName, inlineFunctionStack, p.getStaticContext)

  def createDynamicCurriedFunction(p: XPathParser,
                                   functionItem: Expression,
                                   args: ju.ArrayList[Expression],
                                   placeMarkers: IntSet): Expression = {
    val arguments: Array[Expression] = Array.ofDim[Expression](args.size)
    args.toArray(arguments)
    val result: Expression =
      curryFunction(functionItem, arguments, placeMarkers)
    p.setLocation(result, p.getTokenizer.currentTokenStartOffset)
    result
  }

  def handleExternalFunctionDeclaration(p: XQueryParser, func: XQueryFunction): Unit =
    needExtension(p, "External function declarations")

  def parseTypeAliasDeclaration(p: XQueryParser): Unit =
    needExtension(p, "Type alias declarations")

  def parseRevalidationDeclaration(p: XQueryParser): Unit =
    needUpdate(p, "A revalidation declaration")

  def parseUpdatingFunctionDeclaration(p: XQueryParser): Unit =
    needUpdate(p, "An updating function")

  def parseExtendedExprSingle(p: XPathParser): Expression = null
  def parseForMemberExpression(p: XPathParser): Expression = null
}
