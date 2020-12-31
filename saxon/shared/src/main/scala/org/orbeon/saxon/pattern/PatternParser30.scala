package org.orbeon.saxon.pattern

import org.orbeon.saxon.expr._

import org.orbeon.saxon.expr.instruct.Choose

import org.orbeon.saxon.expr.parser._

import org.orbeon.saxon.functions.Doc

import org.orbeon.saxon.functions.KeyFn

import org.orbeon.saxon.functions.Root_1

import org.orbeon.saxon.functions.SuperId

import org.orbeon.saxon.lib.Feature

import org.orbeon.saxon.lib.NamespaceConstant

import org.orbeon.saxon.model._

import org.orbeon.saxon.om.AxisInfo

import org.orbeon.saxon.om.StructuredQName

import org.orbeon.saxon.trans.XPathException

import org.orbeon.saxon.value.BooleanValue

import org.orbeon.saxon.value.Int64Value

import org.orbeon.saxon.value.SequenceType

import org.orbeon.saxon.expr.parser.XPathParser.ParsedLanguage._

class PatternParser30 extends XPathParser with PatternParser {

  var inPredicate: Int = 0

  def parsePattern(pattern: String, env: StaticContext): Pattern = {
    this.env = env
    charChecker = env.getConfiguration.getValidCharacterChecker
    language = XSLT_PATTERN
    var trimmed: String = pattern.trim()
    if (trimmed.startsWith("(:")) {
      t = new Tokenizer()
      t.languageLevel = 30
      t.tokenize(trimmed, 0, -1)
      val start: Int = t.currentTokenStartOffset
      trimmed = trimmed.substring(start)
    }
    allowSaxonExtensions =
      env.getConfiguration.getBooleanProperty(Feature.ALLOW_SYNTAX_EXTENSIONS)
    if (isSelectionPattern(trimmed)) {
      var e: Expression = parse(pattern, 0, Token.EOF, env)
      if (e.isInstanceOf[Pattern]) {
        e.asInstanceOf[Pattern]
      } else if (e.isInstanceOf[ContextItemExpression]) {
        new UniversalPattern()
      } else if (e.isInstanceOf[FilterExpression]) {
        var predicate: Expression = null
        while (e.isInstanceOf[FilterExpression]) {
          var filter: Expression =
            e.asInstanceOf[FilterExpression].getActionExpression
          e = e.asInstanceOf[FilterExpression].getSelectExpression
          val filterType: ItemType = filter.getItemType
          val th = env.getConfiguration.getTypeHierarchy
          val rel: Affinity.Affinity =
            th.relationship(filterType, NumericType.getInstance)
          if (rel != Affinity.DISJOINT) {
            if (rel == Affinity.SAME_TYPE || rel == Affinity.SUBSUMED_BY) {
              filter = new ValueComparison(
                filter,
                Token.FEQ,
                Literal.makeLiteral(Int64Value.PLUS_ONE))
            } else {
              val let: LetExpression = new LetExpression()
              val varName: StructuredQName = new StructuredQName(
                "vv",
                NamespaceConstant.SAXON_GENERATED_VARIABLE,
                "v" + filter.hashCode)
              let.setVariableQName(varName)
              val condition: InstanceOfExpression = new InstanceOfExpression(
                new LocalVariableReference(let),
                SequenceType.SINGLE_NUMERIC)
              val ref: LocalVariableReference = new LocalVariableReference(let)
              ref.setStaticType(SequenceType.SINGLE_NUMERIC, null, 0)
              val comparison: ValueComparison = new ValueComparison(
                ref,
                Token.FEQ,
                Literal.makeLiteral(Int64Value.PLUS_ONE))
              val choice: Choose = new Choose(
                Array(condition, Literal.makeLiteral(BooleanValue.TRUE)),
                Array(comparison, new LocalVariableReference(let)))
              let.setSequence(filter)
              let.setAction(choice)
              let.setRequiredType(SequenceType.ANY_SEQUENCE)
              let.setRetainedStaticContext(env.makeRetainedStaticContext())
              filter = let
            }
          }
          predicate =
            if (predicate == null) filter
            else new AndExpression(filter, predicate)
        }
        if (e.isInstanceOf[ContextItemExpression]) {
          return new BooleanExpressionPattern(predicate)
        }
      }
      grumble(
        "Pattern starting with '.' must be followed by a sequence of predicates")
      null
    } else {
      val exp: Expression = parse(pattern, 0, Token.EOF, env)
      exp.setRetainedStaticContext(env.makeRetainedStaticContext())
      if (exp.isInstanceOf[VennExpression]) {
        checkNoPredicatePattern(
          exp.asInstanceOf[VennExpression].getLhsExpression)
        checkNoPredicatePattern(
          exp.asInstanceOf[VennExpression].getRhsExpression)
      }
      val visitor: ExpressionVisitor = ExpressionVisitor.make(env)
      visitor.setOptimizeForPatternMatching(true)
      val cit: ContextItemStaticInfo = visitor.getConfiguration
        .makeContextItemStaticInfo(AnyNodeTest, maybeUndefined = true)
      var pat: Pattern = null
      try pat = PatternMaker.fromExpression(
        exp.simplify().typeCheck(visitor, cit),
        env.getConfiguration,
        is30 = true)
      catch {
        case e: XPathException =>
          pat = PatternMaker.fromExpression(exp.simplify(),
            env.getConfiguration,
            is30 = true)

      }
      if (exp.isInstanceOf[FilterExpression] &&
        exp
          .asInstanceOf[FilterExpression]
          .getBase
          .isInstanceOf[ContextItemExpression]) {
        if (allowSaxonExtensions &&
          (pattern.startsWith("tuple") || pattern.startsWith("map") ||
            pattern.startsWith("array") ||
            pattern.startsWith("union"))) {} else {
          grumble(
            "A predicatePattern can appear only at the outermost level (parentheses not allowed)")
        }
      }
      if (exp.isInstanceOf[FilterExpression] && pat
        .isInstanceOf[NodeTestPattern]) {
        pat.setPriority(0.5)
      }
      pat
    }
  }

  private def isSelectionPattern(pattern: String): Boolean = {
    if (pattern.startsWith("."))
      return true
    if (pattern.matches("^(type|tuple|map|array|union|atomic)\\s*\\(.+")) {
      checkSyntaxExtensions(
        "Patterns matching " + pattern.replace("\\(.*$", "") +
          " types")
      return true
    }
    false
  }

  private def checkNoPredicatePattern(exp: Expression): Unit = {
    if (exp.isInstanceOf[ContextItemExpression]) {
      grumble(
        "A predicatePattern can appear only at the outermost level (union operator not allowed)")
    }
    if (exp.isInstanceOf[FilterExpression]) {
      checkNoPredicatePattern(exp.asInstanceOf[FilterExpression].getBase)
    }
    if (exp.isInstanceOf[VennExpression]) {
      checkNoPredicatePattern(
        exp.asInstanceOf[VennExpression].getLhsExpression)
      checkNoPredicatePattern(
        exp.asInstanceOf[VennExpression].getRhsExpression)
    }
  }

  override def customizeTokenizer(t: Tokenizer): Unit = ()

  override def parseExpression(): Expression = {
    val t: Tokenizer = getTokenizer
    if (inPredicate > 0) {
      super.parseExpression
    } else if (allowSaxonExtensions && t.currentToken == Token.NODEKIND &&
      (t.currentTokenValue.==("tuple") || t.currentTokenValue.==(
        "type") ||
        t.currentTokenValue.==("map") ||
        t.currentTokenValue.==("array"))) {
      val `type`: ItemType = parseItemType
      var expr: Expression = new ItemTypePattern(`type`)
      expr.setRetainedStaticContext(env.makeRetainedStaticContext())
      this.setLocation(expr)
      while (t.currentToken == Token.LSQB) expr =
        parsePredicate(expr).toPattern(env.getConfiguration)
      expr
    } else if (allowSaxonExtensions && t.currentToken == Token.NODEKIND &&
      (t.currentTokenValue.==("atomic"))) {
      nextToken()
      expect(Token.NAME)
      val typeName: StructuredQName = makeStructuredQName(
        t.currentTokenValue,
        env.getDefaultElementNamespace)
      nextToken()
      expect(Token.RPAR)
      nextToken()
      val `type` = env.getConfiguration.getSchemaType(typeName)
      if (`type` == null || !`type`.isAtomicType)
        grumble("Unknown atomic type " + typeName)
      val at: AtomicType = `type`.asInstanceOf[AtomicType]
      var expr: Expression = new ItemTypePattern(at)
      this.setLocation(expr)
      while (t.currentToken == Token.LSQB) expr = parsePredicate(expr)
      expr
    } else {
      parseBinaryExpression(parsePathExpression, 10)
    }
  }

  override def parseBasicStep(firstInPattern: Boolean): Expression =
    if (inPredicate > 0) {
      super.parseBasicStep(firstInPattern)
    } else {
      t.currentToken match {
        case Token.DOLLAR =>
          if (!firstInPattern) {
            grumble(
              "In an XSLT 3.0 pattern, a variable reference is allowed only as the first step in a path")
            null
          } else {
            super.parseBasicStep(firstInPattern)
          }
        case Token.STRING_LITERAL | Token.NUMBER | Token.KEYWORD_CURLY |
             Token.ELEMENT_QNAME | Token.ATTRIBUTE_QNAME |
             Token.NAMESPACE_QNAME | Token.PI_QNAME | Token.TAG |
             Token.NAMED_FUNCTION_REF | Token.DOTDOT =>
          grumble(
            "Token " + currentTokenDisplay + " not allowed here in an XSLT pattern")
          null
        case Token.FUNCTION =>
          if (!firstInPattern) {
            grumble(
              "In an XSLT pattern, a function call is allowed only as the first step in a path")
          }
          super.parseBasicStep(firstInPattern)
        case Token.NODEKIND =>
          t.currentTokenValue match {
            case "type" | "tuple" | "union" | "map" | "array" | "atomic" =>
              parserExtension.parseTypePattern(this)
            case _ => super.parseBasicStep(firstInPattern)

          }
        case _ => super.parseBasicStep(firstInPattern)

      }
    }

  override def testPermittedAxis(axis: Int,
                                 errorCode: String): Unit = {
    super.testPermittedAxis(axis, errorCode)
    if (inPredicate == 0) {
      if (!AxisInfo.isSubtreeAxis(axis)) {
        grumble(
          "The " + AxisInfo.axisName(axis) + " is not allowed in a pattern")
      }
    }
  }

  override def parsePredicate(): Expression = {
    val disallow: Boolean = t.disallowUnionKeyword
    t.disallowUnionKeyword = false

    val exp: Expression = parseExpression()

    t.disallowUnionKeyword = disallow
    exp
  }

  override def parseFunctionCall(prefixArgument: Expression): Expression = {
    val fn: Expression = super.parseFunctionCall(prefixArgument)
    if (inPredicate <= 0 && !fn.isCallOn(classOf[SuperId]) && !fn.isCallOn(
      classOf[KeyFn]) &&
      !fn.isCallOn(classOf[Doc]) &&
      !fn.isCallOn(classOf[Root_1])) {
      grumble(
        "The " + fn.toString +
          " function is not allowed at the head of a pattern")
    }
    fn
  }

  override def parseFunctionArgument(): Expression =
    if (inPredicate > 0) {
      super.parseFunctionArgument
    } else {
      t.currentToken match {
        case Token.DOLLAR => parseVariableReference
        case Token.STRING_LITERAL => parseStringLiteral(true)
        case Token.NUMBER => parseNumericLiteral(true)
        case _ =>
          grumble(
            "A function argument in an XSLT pattern must be a variable reference or literal")
          null

      }
    }

  override def makeTracer(exp: Expression, qName: StructuredQName): Expression = exp

}
