package org.orbeon.saxon.expr.parser

import java.net.{URI, URISyntaxException}
import java.util
import java.util.function.Predicate

import javax.xml.transform.SourceLocator
import org.orbeon.saxon.event.ComplexContentOutputter
import org.orbeon.saxon.expr._
import org.orbeon.saxon.expr.flwor.FLWORExpression
import org.orbeon.saxon.expr.instruct._
import org.orbeon.saxon.expr.sort.{ConditionalSorter, DocumentSorter}
import org.orbeon.saxon.functions._
import org.orbeon.saxon.lib.{Logger, NamespaceConstant, StandardLogger}
import org.orbeon.saxon.ma.arrays.ArrayItem
import org.orbeon.saxon.ma.map.MapItem
import org.orbeon.saxon.model.{AnyItemType, ItemType}
import org.orbeon.saxon.om._
import org.orbeon.saxon.pattern.Pattern
import org.orbeon.saxon.query.QueryModule
import org.orbeon.saxon.trans.{Err, NoDynamicContextException, SymbolicName, XPathException}
import org.orbeon.saxon.tree.util.FastStringBuffer
import org.orbeon.saxon.value._

import scala.annotation.tailrec

//import scala.collection.compat._
import scala.jdk.CollectionConverters._


object ExpressionTool {

  @throws[XPathException]
  def make(expression: String, env: StaticContext, start: Int, _terminator: Int, codeInjector: CodeInjector): Expression = {
    var terminator = _terminator
    val languageLevel = env.getXPathVersion
    val parser = env.getConfiguration.newExpressionParser("XP", updating = false, languageLevel)
    if (codeInjector != null)
      parser.setCodeInjector(codeInjector)
    if (terminator == -1)
      terminator = Token.EOF
    var exp = parser.parse(expression, start, terminator, env)
    setDeepRetainedStaticContext(exp, env.makeRetainedStaticContext())
    exp = exp.simplify()
    exp
  }

  def setDeepRetainedStaticContext(exp: Expression, _rsc: RetainedStaticContext): Unit = {

    var rsc = _rsc

    if (exp.getLocalRetainedStaticContext == null)
      exp.setRetainedStaticContextLocally(rsc)
    else
      rsc = exp.getLocalRetainedStaticContext

    for (o <- exp.operands.asScala)
      setDeepRetainedStaticContext(o.getChildExpression, rsc)
  }

  def copyLocationInfo(from: Expression, to: Expression): Unit =
    if (from != null && to != null) {
      if (to.getLocation == null || (to.getLocation eq Loc.NONE))
        to.setLocation(from.getLocation)
      if (to.getLocalRetainedStaticContext == null)
        to.setRetainedStaticContextLocally(from.getLocalRetainedStaticContext)
    }

  @throws[XPathException]
  def unsortedIfHomogeneous(exp: Expression, forStreaming: Boolean): Expression = {
    if (exp.isInstanceOf[Literal])
      exp
    else if (exp.getItemType eq AnyItemType)
      exp
    else
      exp.unordered(retainAllNodes = false, forStreaming = forStreaming)
  }

  def injectCode(exp: Expression, injector: CodeInjector): Expression = {
    exp match {
      case expression: FLWORExpression => expression.injectCode(injector)
      case _ =>
        if (!exp.isInstanceOf[TraceExpression])
          for (o <- exp.operands.asScala)
            o.setChildExpression(injectCode(o.getChildExpression, injector))
    }
    injector.inject(exp)
  }

  def lazyEvaluator(exp: Expression, repeatable: Boolean): Evaluator =
    exp match {
      case _: Literal => Evaluator.LITERAL
      case _: VariableReference => Evaluator.VARIABLE
      case _: SuppliedParameterReference => Evaluator.SUPPLIED_PARAMETER
      case _ =>
        if ((exp.getDependencies & (StaticProperty.DEPENDS_ON_POSITION | StaticProperty.DEPENDS_ON_LAST | StaticProperty.DEPENDS_ON_CURRENT_ITEM |
        StaticProperty.DEPENDS_ON_CURRENT_GROUP | StaticProperty.DEPENDS_ON_REGEX_GROUP)) != 0) {
          eagerEvaluator(exp)
        } else if (exp.isInstanceOf[ErrorExpression]) {
          Evaluator.SINGLE_ITEM
        } else if (!Cardinality.allowsMany(exp.getCardinality)) {
          eagerEvaluator(exp)
        } else exp match {
            case tail: TailExpression =>
              val base = tail.getBaseExpression
              if (base.isInstanceOf[VariableReference])
                Evaluator.LAZY_TAIL
              else if (repeatable)
                Evaluator.MEMO_CLOSURE
              else
                Evaluator.LAZY_SEQUENCE
            case block: Block if block.isCandidateForSharedAppend =>
              Evaluator.SHARED_APPEND
            case _ => if (repeatable) {
              Evaluator.MEMO_CLOSURE
            }
            else Evaluator.LAZY_SEQUENCE
          }
    }

  def eagerEvaluator(exp: Expression): Evaluator = {
    exp match {
      case literal: Literal if ! literal.getValue.isInstanceOf[Closure] =>
        return Evaluator.LITERAL
      case _ =>
    }

    if (exp.isInstanceOf[VariableReference])
      return Evaluator.VARIABLE

    val m = exp.getImplementationMethod
    if (((m & Expression.EVALUATE_METHOD)  != 0) && ! Cardinality.allowsMany(exp.getCardinality)) {
      if (Cardinality.allowsZero(exp.getCardinality))
        Evaluator.OPTIONAL_ITEM
      else
        Evaluator.SINGLE_ITEM
    } else if ((m & Expression.ITERATE_METHOD) != 0) {
      Evaluator.EAGER_SEQUENCE
    } else {
      Evaluator.PROCESS
    }
  }

  @throws[XPathException]
  def lazyEvaluate(exp: Expression, context: XPathContext, repeatable: Boolean): Sequence = {
    val evaluator = lazyEvaluator(exp, repeatable)
    evaluator.evaluate(exp, context)
  }

  @throws[XPathException]
  def eagerEvaluate(exp: Expression, context: XPathContext): GroundedValue = {
    val evaluator = eagerEvaluator(exp)
    evaluator.evaluate(exp, context).materialize
  }

  def markTailFunctionCalls(exp: Expression, qName: StructuredQName, arity: Int): Int =
    exp.markTailFunctionCalls(qName, arity)

  def indent(level: Int): String = {
    val fsb = new FastStringBuffer(level)
    for (_ <- 0 until level)
      fsb.append("  ")
    fsb.toString
  }

  def contains(a: Expression, b: Expression): Boolean = {
    var temp = b
    var exitLoop = false
    while (! exitLoop && temp != null)
      if (temp eq a)
        exitLoop = true
      else
        temp = temp.getParentExpression
    exitLoop
  }

  def containsLocalParam(exp: Expression): Boolean =
    contains(exp, sameFocusOnly = true, (e: Expression) => e.isInstanceOf[LocalParam])

  def containsLocalVariableReference(exp: Expression): Boolean =
    contains(exp, sameFocusOnly = false, {
      case vref: LocalVariableReference =>
        val binding = vref.getBinding
        ! (binding.isInstanceOf[Expression] && contains(exp, binding.asInstanceOf[Expression]))
      case _ =>
        false
    }: Predicate[Expression])

  def contains(exp: Expression, sameFocusOnly: Boolean, predicate: Predicate[Expression]): Boolean =
    predicate.test(exp) ||
      exp.operands.asScala.exists(info =>
        (info.hasSameFocus || ! sameFocusOnly) && contains(info.getChildExpression, sameFocusOnly, predicate))

  def changesXsltContext(exp: Expression): Boolean = {
    var expression = exp
    expression = exp.getInterpretedExpression
    if (exp.isInstanceOf[ResultDocument] || exp.isInstanceOf[CallTemplate] || exp.isInstanceOf[ApplyTemplates] || exp.isInstanceOf[NextMatch] || exp.isInstanceOf[ApplyImports] || exp.isCallOn(classOf[RegexGroup]) || exp.isCallOn(classOf[CurrentGroup])) return true

    for (o <- exp.operands.asScala)
      if (changesXsltContext(o.getChildExpression))
        return true
    false
  }

  def isLoopingSubexpression(child: Expression, ancestor: Expression): Boolean = {
    var childExp = child
    while (true) {
      val parent = childExp.getParentExpression
      if (parent == null)
        return false
      if (hasLoopingSubexpression(parent, childExp))
        return true
      if (parent eq ancestor)
        return false
      childExp = parent
    }
    false
  }

  def isLoopingReference(reference: VariableReference, binding: Binding): Boolean = {
    var child: Expression = reference
    var parent = child.getParentExpression

    while (true) {

      if (parent == null)
        return true

      parent match {
        case expr: FLWORExpression =>
          if (parent.hasVariableBinding(binding))
            return expr.hasLoopingVariableReference(binding)
          else if (hasLoopingSubexpression(parent, child))
            return true
        case _ =>
          if (parent.getExpressionName == "tryCatch")
            return true
          else {
            if (parent.isInstanceOf[ForEachGroup] && parent.hasVariableBinding(binding))
              return false
            if (hasLoopingSubexpression(parent, child))
              return true
            if (parent.hasVariableBinding(binding))
              return false
          }
      }
      child = parent
      parent = child.getParentExpression
    }
    false
  }

  def hasLoopingSubexpression(parent: Expression, child: Expression): Boolean =
    parent.operands.asScala.find(_.getChildExpression eq child).exists(_.isEvaluatedRepeatedly)

  def getFocusSettingContainer(exp: Expression): Expression = {
    var child = exp
    var parent = child.getParentExpression
    while (parent != null) {
      val o = findOperand(parent, child)
      assert(o ne null)
      if (! o.hasSameFocus)
        return parent
      child = parent
      parent = child.getParentExpression
    }
    null
  }

  def getContextDocumentSettingContainer(exp: Expression): Expression = {
    var child = exp
    var parent = child.getParentExpression
    while (parent != null) {
      parent match {
        case switcher: ContextSwitchingExpression =>
          if (child eq switcher.getActionExpression)
            if (switcher.getSelectExpression.hasSpecialProperty(StaticProperty.CONTEXT_DOCUMENT_NODESET)) {
              parent.resetLocalStaticProperties()
              parent.getSpecialProperties
              return getContextDocumentSettingContainer(parent)
            }
        case _ =>
      }
      val o = findOperand(parent, child)
      assert(o ne null)
      if (! o.hasSameFocus)
        return parent
      child = parent
      parent = child.getParentExpression
    }
    null
  }

  def resetStaticProperties(exp: Expression): Unit = {
    var expression = exp
    var i = 0
    while (expression != null) {
      expression.resetLocalStaticProperties()
      expression = expression.getParentExpression
      if (i > 100000)
        throw new IllegalStateException("Loop in parent expression chain")
      i += 1
    }
  }

  def equalOrNull(x: Any, y: Any): Boolean =
    if (x == null)
      y == null
    else
      x == y

  @throws[XPathException]
  def getIteratorFromProcessMethod(exp: Expression, context: XPathContext): SequenceIterator = {
    val controller = context.getController
    assert(controller != null)
    val seq = controller.allocateSequenceOutputter
    exp.process(new ComplexContentOutputter(seq), context)
    seq.close()
    seq.iterate()
  }

  @throws[XPathException]
  def getItemFromProcessMethod(exp: Expression, context: XPathContext): Item = {
    val controller = context.getController
    if (controller == null)
      throw new NoDynamicContextException("No controller available")
    val seq = controller.allocateSequenceOutputter(1)
    exp.process(new ComplexContentOutputter(seq), context)
    seq.close()
    val result = seq.getFirstItem
    seq.reset()
    result
  }


  def allocateSlots(exp: Expression, _nextFree: Int, frame: SlotManager): Int = {
    var nextFree = _nextFree
    exp match {
      case assignation: Assignation =>
        assignation.setSlotNumber(nextFree)
        val count = assignation.getRequiredSlots
        nextFree += count
        if (frame != null)
          frame.allocateSlotNumber(assignation.getVariableQName)
      case _ =>
    }
    exp match {
      case param: LocalParam if param.getSlotNumber < 0 =>
        param.setSlotNumber({
          nextFree += 1
          nextFree
        })
      case _ =>
    }
    exp match {
      case expr: FLWORExpression =>
        for (c <- expr.getClauseList.asScala) {
          for (b <- c.getRangeVariables) {
            b.setSlotNumber({
              nextFree += 1
              nextFree
            })
            frame.allocateSlotNumber(b.getVariableQName)
          }
        }
      case _ =>
    }
    exp match {
      case varRef: VariableReference =>
        val binding = varRef.getBinding

        varRef match {
          case reference: LocalVariableReference =>
            reference.setSlotNumber(binding.asInstanceOf[LocalBinding].getLocalSlotNumber)
          case _ =>
        }
        binding match {
          case decl: Assignation if binding.asInstanceOf[LocalBinding].getLocalSlotNumber < 0 =>
            // This indicates something badly wrong: we've found a variable reference on the tree, that's
            // bound to a variable declaration that is no longer on the tree. All we can do is print diagnostics.
            // The most common reason for this failure is that the declaration of the variable was removed
            // from the tree in the mistaken belief that there were no references to the variable. Variable
            // references are counted during the typeCheck phase, so this can happen if typeCheck() fails to
            // visit some branch of the expression tree.

            var err: Logger = null
            try err = varRef.getConfiguration.getLogger
            catch {
              case _: Exception =>
                err = new StandardLogger
            }
            val msg = "*** Internal Saxon error: local variable encountered whose binding has been deleted"
            err.error(msg)
            err.error("Variable name: " + decl.getVariableName)
            err.error("Line number of reference: " + varRef.getLocation.getLineNumber + " in " + varRef.getLocation.getSystemId)
            err.error("Line number of declaration: " + decl.getLocation.getLineNumber + " in " + decl.getLocation.getSystemId)
            err.error("DECLARATION:")
            try
              decl.explain(err)
            catch {
              case _: Exception =>
            }
            throw new IllegalStateException(msg)
          case _ =>
        }
      case _ =>
    }
    exp match {
      case pattern: Pattern =>
        nextFree = pattern.allocateSlots(frame, nextFree)
      case _ =>
        for (o <- exp.operands.asScala)
          nextFree = allocateSlots(o.getChildExpression, nextFree, frame)
    }
    nextFree
  }


  @throws[XPathException]
  def effectiveBooleanValue(iterator: SequenceIterator): Boolean = {
    val first = iterator.next
    if (first == null)
      return false
    first match {
      case _: NodeInfo =>
        iterator.close()
        true
      case value: AtomicValue => first match {
        case value1: BooleanValue =>
          if (iterator.next != null) {
            iterator.close()
            ebvError("a sequence of two or more items starting with a boolean")
          }
          iterator.close()
          value1.getBooleanValue
        case value1: StringValue =>
          if (iterator.next != null) {
            iterator.close()
            ebvError("a sequence of two or more items starting with a string")
          }
          ! value1.isZeroLength
        case n: NumericValue =>
          if (iterator.next != null) {
            iterator.close()
            ebvError("a sequence of two or more items starting with a numeric value")
          }
          (n.compareTo(0) != 0) && !n.isNaN
        case _ =>
          iterator.close()
          ebvError("a sequence starting with an atomic value of type " + value.getItemType.getTypeName.getDisplayName)
      }
      case _: Function =>
        iterator.close()
        first match {
          case _: ArrayItem =>
            ebvError("a sequence starting with an array item (" + first.toShortString + ")")
          case _: MapItem =>
            ebvError("a sequence starting with a map (" + first.toShortString + ")")
          case _ =>
            ebvError("a sequence starting with a function (" + first.toShortString + ")")
        }
      case _: ObjectValue[_] =>
        if (iterator.next != null) {
          iterator.close()
          ebvError("a sequence of two or more items starting with an external object value")
        }
        true
      case _ =>
        ebvError("a sequence starting with an item of unknown kind")
        false
    }
  }

  @throws[XPathException]
  def effectiveBooleanValue(item: Item): Boolean =
    item match {
      case null => false
      case _: NodeInfo => true
      case value: AtomicValue =>
        item match {
          case value1: BooleanValue => value1.getBooleanValue
          case value1: StringValue  => ! value1.isZeroLength
          case n: NumericValue      => (n.compareTo(0) != 0) && ! n.isNaN
          case _: ExternalObject[_] => true
          case _ =>
            ebvError("an atomic value of type " + value.getPrimitiveType.getDisplayName)
        }
      case _ =>
        ebvError(item.getGenre.toString)
    }

  @throws[XPathException]
  def ebvError(reason: String): Nothing = {
    val err = new XPathException("Effective boolean value is not defined for " + reason)
    err.setErrorCode("FORG0006")
    err.setIsTypeError(true)
    throw err
  }

  @throws[XPathException]
  def ebvError(reason: String, cause: Expression): Nothing = {
    val err = new XPathException("Effective boolean value is not defined for " + reason)
    err.setErrorCode("FORG0006")
    err.setIsTypeError(true)
    err.setFailingExpression(cause)
    throw err
  }

  def dependsOnFocus(exp: Expression): Boolean = (exp.getDependencies & StaticProperty.DEPENDS_ON_FOCUS) != 0

  def dependsOnVariable(exp: Expression, bindingList: Array[Binding]): Boolean =
    !(bindingList == null || bindingList.length == 0) && contains(exp, sameFocusOnly = false, (e: Expression) => {
    def foo(e: Expression): Boolean = {
      e match {
        case varRef: VariableReference =>
          for (binding <- bindingList)
            if (varRef.getBinding eq binding)
              return true
        case _ =>
      }
      false
    }

    foo(e)
  })

  def gatherReferencedVariables(e: Expression, list: util.List[Binding]): Unit =
    e match {
      case varRef: VariableReference =>
        val binding = varRef.getBinding
        if (! list.contains(binding))
          list.add(binding)
      case _ =>
        for (o <- e.operands.asScala)
          if (! o.getOperandRole.isInChoiceGroup)
            gatherReferencedVariables(o.getChildExpression, list)
    }

  def refersToVariableOrFunction(exp: Expression): Boolean =
    contains(exp, sameFocusOnly = false, (e: Expression) => e.isInstanceOf[VariableReference] ||
    e.isInstanceOf[UserFunctionCall] || e.isInstanceOf[Binding] || e.isInstanceOf[CallTemplate] || e.isInstanceOf[ApplyTemplates] || e.isInstanceOf[ApplyImports] ||
    isCallOnSystemFunction(e, "function-lookup") || e.isCallOn(classOf[ApplyFn]))

  def isCallOnSystemFunction(e: Expression, localName: String): Boolean =
    e.isInstanceOf[StaticFunctionCall] && localName == e.asInstanceOf[StaticFunctionCall].
    getFunctionName.getLocalPart

  def callsFunction(exp: Expression, qName: StructuredQName, sameFocusOnly: Boolean): Boolean =
    contains(exp, sameFocusOnly, (e: Expression) => e.isInstanceOf[FunctionCall] && qName == e.asInstanceOf[FunctionCall].getFunctionName)

  def containsSubexpression(exp: Expression, subClass: Class[_ <: Expression]): Boolean = contains(exp, sameFocusOnly = false,
    (e: Expression) => subClass.isAssignableFrom(e.getClass))

  def gatherCalledFunctions(e: Expression, list: util.List[UserFunction]): Unit =
    e match {
      case call: UserFunctionCall =>
        val function = call.getFunction
        if (!list.contains(function))
          list.add(function)
      case _ =>
        for (o <- e.operands.asScala)
          gatherCalledFunctions(o.getChildExpression, list)
    }

  def gatherCalledFunctionNames(e: Expression, list: util.List[SymbolicName]): Unit =
    e match {
      case call: UserFunctionCall => list.add(call.getSymbolicName)
      case _ =>
        for (o <- e.operands.asScala)
          gatherCalledFunctionNames(o.getChildExpression, list)
    }

  // Compilation,ExpressionContext scala class not exist
  // ORBEON: Not used by Saxon XPath.
  @throws[XPathException]
  def optimizeComponentBody(body: Expression/*, compilation: Compilation*/, visitor: ExpressionVisitor, cisi: ContextItemStaticInfo, extractGlobals: Boolean): Expression = { //
    val config = visitor.getConfiguration
    val opt = visitor.obtainOptimizer
    val env = visitor.getStaticContext
    var compileWithTracing = config.isCompileWithTracing
    var expBody = body
    if (!compileWithTracing) {
     /* if (compilation != null) compileWithTracing = compilation.getCompilerInfo.isCompileWithTracing
      else*/ if (env.isInstanceOf[QueryModule])
        compileWithTracing = env.asInstanceOf[QueryModule].getUserQueryContext.isCompileWithTracing
      //else if (env.isInstanceOf[ExpressionContext]) compileWithTracing = env.asInstanceOf[ExpressionContext].getStyleElement.getCompilation.getCompilerInfo.isCompileWithTracing
    }
    if (opt.isOptionSet(OptimizerOptions.MISCELLANEOUS) && ! compileWithTracing) {
      ExpressionTool.resetPropertiesWithinSubtree(expBody)
      if (opt.isOptionSet(OptimizerOptions.MISCELLANEOUS)) expBody = expBody.optimize(visitor, cisi)
      expBody.setParentExpression(null)
      /*if (extractGlobals && compilation != null) {
        val exp2 = opt.promoteExpressionsToGlobal(expBody, compilation.getPrincipalStylesheetModule, visitor)
        if (exp2 != null) {
          ExpressionTool.resetPropertiesWithinSubtree(exp2)
          expBody = exp2.optimize(visitor, cisi)
        }
      }*/
      if (opt.isOptionSet(OptimizerOptions.LOOP_LIFTING))
        expBody = LoopLifter.process(expBody, visitor, cisi)
    } else
      expBody = avoidDocumentSort(expBody)
    if (!visitor.isOptimizeForStreaming)
      expBody = opt.eliminateCommonSubexpressions(expBody)
    opt.injectByteCodeCandidates(expBody)
    opt.prepareForStreaming(expBody)
    computeEvaluationModesForUserFunctionCalls(expBody)
    expBody.restoreParentPointers()
    expBody
  }

  private def avoidDocumentSort(exp: Expression): Expression = {
    exp match {
      case sorter1: DocumentSorter =>
        val base = sorter1.getBaseExpression
        if (base.hasSpecialProperty(StaticProperty.ORDERED_NODESET))
          return base
        else
          return exp
      case sorter1: ConditionalSorter =>
        val sorter = sorter1.getDocumentSorter
        val eliminatedSorter = avoidDocumentSort(sorter)
        if (eliminatedSorter ne sorter)
          return eliminatedSorter
      case _ =>
    }

    for (o <- exp.operands.asScala)
      o.setChildExpression(avoidDocumentSort(o.getChildExpression))

    exp
  }

  @throws[XPathException]
  def computeEvaluationModesForUserFunctionCalls(exp: Expression): Boolean =
    ExpressionTool.processExpressionTree(exp, null, (expression: Expression, result: Any) => {
    def foo(expression: Expression, result: Any) = {
      expression match {
        case call: UserFunctionCall => call.allocateArgumentEvaluators()
        case _ =>
      }
      expression match {
        case param: LocalParam => param.computeEvaluationMode()
        case _ =>
      }
      false
    }

    foo(expression, result)
  })

  @throws[XPathException]
  def clearStreamabilityData(exp: Expression): Boolean =
    ExpressionTool.processExpressionTree(exp, null, (expression: Expression, result: Any) => {
    def foo(expression: Expression, result: Any) = {
      expression.setExtraProperty("P+S", null)
      expression.setExtraProperty("inversion", null)
      false
    }

    foo(expression, result)
  })

  def resetPropertiesWithinSubtree(exp: Expression): Unit = {
    exp.resetLocalStaticProperties()
    exp match {
      case ref: LocalVariableReference =>
        val binding = ref.getBinding
        if (binding.isInstanceOf[Assignation])
          binding.addReference(ref, ref.isInLoop)
      case _ =>
    }

    for (o <- exp.operands.asScala) {
      resetPropertiesWithinSubtree(o.getChildExpression)
      o.getChildExpression.setParentExpression(exp)
    }
  }

  def resolveCallsToCurrentFunction(exp: Expression): Expression =
    if (exp.isCallOn(classOf[Current])) {
      val cie = new ContextItemExpression
      copyLocationInfo(exp, cie)
      cie
    } else {
      if (callsFunction(exp, Current.FN_CURRENT, sameFocusOnly = true))
        replaceTrivialCallsToCurrent(exp)
      if (callsFunction(exp, Current.FN_CURRENT, sameFocusOnly = false)) {
        val let = new LetExpression
        let.setVariableQName(new StructuredQName("vv", NamespaceConstant.SAXON_GENERATED_VARIABLE, "current" + exp.hashCode))
        let.setRequiredType(SequenceType.SINGLE_ITEM)
        let.setSequence(new CurrentItemExpression)
        replaceCallsToCurrent(exp, let)
        let.setAction(exp)
        let
      } else
        exp
    }

  def gatherVariableReferences(exp: Expression, binding: Binding, list: util.List[VariableReference]): Unit =
    exp match {
      case varRef: VariableReference if varRef.getBinding eq binding =>
        list.add(varRef)
      case _ =>
        for (o <- exp.operands.asScala)
          gatherVariableReferences(o.getChildExpression, binding, list)
    }

  @throws[XPathException]
  def processExpressionTree(root: Expression, result: Any, action: ExpressionAction): Boolean = {
    var done = action.process(root, result.asInstanceOf[AnyRef])
    if (! done) {
      for (o <- root.operands.asScala) {
        done = processExpressionTree(o.getChildExpression, result, action)
        if (done)
          return true
      }
    }
    false
  }


  trait ExpressionSelector {
    def matches(exp: Expression): Boolean
  }

  def replaceSelectedSubexpressions(exp: Expression, selector: ExpressionTool.ExpressionSelector, replacement: Expression, mustCopy: Boolean): Boolean = {
    var replaced = false
    var mustCpy = mustCopy
    for (o <- exp.operands.asScala) {
      if (replaced)
        mustCpy = true
      val child = o.getChildExpression
      if (selector.matches(child)) {
        val e2 = if (mustCpy) replacement.copy(new RebindingMap)
        else replacement
        o.setChildExpression(e2)
        replaced = true
      } else
        replaced = replaceSelectedSubexpressions(child, selector, replacement, mustCpy)
    }
    replaced
  }

  def replaceVariableReferences(exp: Expression, binding: Binding, replacement: Expression, mustCopy: Boolean): Boolean = {
    val selector: ExpressionSelector = (child: Expression) => child.isInstanceOf[VariableReference] && (child.asInstanceOf[VariableReference].getBinding eq binding)
    replaceSelectedSubexpressions(exp, selector, replacement, mustCopy)
  }

  import scala.util.control.Breaks._

  def getReferenceCount(exp: Expression, binding: Binding, inLoop: Boolean): Int = {
    var rcount = 0
    exp match {
      case varRef: VariableReference if varRef.getBinding eq binding =>
        if (varRef.isFiltered)
          return FilterExpression.FILTERED
        else {
          if (inLoop)
            rcount += 10
          else
            rcount += 1
        }
      case _ =>
        if ((exp.getDependencies & StaticProperty.DEPENDS_ON_LOCAL_VARIABLES) != 0)
          return 0
        else
          breakable {
            for (info <- exp.operands.asScala) {
              val child = info.getChildExpression
              val childLoop = inLoop || info.isEvaluatedRepeatedly
              rcount += getReferenceCount(child, binding, childLoop)
              if (rcount >= FilterExpression.FILTERED)
                break()
            }
          }
    }
    rcount
  }

  def expressionSize(exp: Expression): Int = {
    var expres = exp
    expres = expres.getInterpretedExpression
    var total = 1

    for (o <- expres.operands.asScala)
      total += expressionSize(o.getChildExpression)

    total
  }

  def rebindVariableReferences(exp: Expression, oldBinding: Binding, newBinding: Binding): Unit =
    exp match {
      case varRef: VariableReference =>
        if (varRef.getBinding eq oldBinding)
          varRef.fixup(newBinding)
      case _ =>
        for (o <- exp.operands.asScala)
          rebindVariableReferences(o.getChildExpression, oldBinding, newBinding)
    }

  def makePathExpression(start: Expression, step: Expression): Expression = {

    if (start.isInstanceOf[RootExpression] && step.isInstanceOf[AxisExpression] && step.asInstanceOf[AxisExpression].getAxis == AxisInfo.PARENT)
      return Literal.makeEmptySequence

    val expr = new SlashExpression(start, step)

    step match {
      case stepPath: SlashExpression =>
        if (isFilteredAxisPath(stepPath.getSelectExpression) && isFilteredAxisPath(stepPath.getActionExpression)) {
          expr.setStart(ExpressionTool.makePathExpression(start, stepPath.getSelectExpression))
          expr.setStep(stepPath.getActionExpression)
        }
      case _ =>
    }
    expr
  }

  def findOperand(parentExpression: Expression, childExpression: Expression): Operand = {

    for (o <- parentExpression.operands.asScala)
      if (o.getChildExpression eq childExpression)
        return o

    null
  }

  private def isFilteredAxisPath(exp: Expression): Boolean =
    unfilteredExpression(exp, allowPositional = true).isInstanceOf[AxisExpression]

  @tailrec
  def unfilteredExpression(exp: Expression, allowPositional: Boolean): Expression =
    exp match {
      case filterExpr: FilterExpression if allowPositional || !filterExpr.isFilterIsPositional =>
        unfilteredExpression(filterExpr.getSelectExpression, allowPositional)
      case filter: SingleItemFilter if allowPositional =>
        unfilteredExpression(filter.getBaseExpression, allowPositional)
      case _ => exp
    }

  def tryToFactorOutDot(exp: Expression, contextItemType: ItemType): Expression =
    exp match {
      case _: ContextItemExpression => null
      case letExpr: LetExpression if letExpr.getSequence.isInstanceOf[ContextItemExpression] =>
        val action = letExpr.getAction
        val changed = factorOutDot(action, letExpr)
        if (changed)
          exp.resetLocalStaticProperties()
        exp
      case _ =>
        if ((exp.getDependencies & (StaticProperty.DEPENDS_ON_CONTEXT_ITEM | StaticProperty.DEPENDS_ON_CONTEXT_DOCUMENT)) != 0) {
          val let = new LetExpression
          let.setVariableQName(new StructuredQName("saxon", NamespaceConstant.SAXON, "dot" + exp.hashCode))
          let.setRequiredType(SequenceType.makeSequenceType(contextItemType, StaticProperty.EXACTLY_ONE))
          let.setSequence(new ContextItemExpression)
          let.setAction(exp)
          val changed = factorOutDot(exp, let)
          if (changed)
            let
          else
            exp
        } else
          null
    }

  def factorOutDot(exp: Expression, variable: Binding): Boolean = {
    var changed = false
    if ((exp.getDependencies & (StaticProperty.DEPENDS_ON_CONTEXT_ITEM | StaticProperty.DEPENDS_ON_CONTEXT_DOCUMENT)) != 0) {

      for (info <- exp.operands.asScala) {
        if (info.hasSameFocus) {
          val child = info.getChildExpression
          if (child.isInstanceOf[ContextItemExpression]) {
            val ref =
              if (variable.isGlobal)
                new GlobalVariableReference(variable.asInstanceOf[GlobalVariable])
              else
                new LocalVariableReference(variable.asInstanceOf[LocalBinding])
            copyLocationInfo(child, ref)
            info.setChildExpression(ref)
            changed = true
          } else if (child.isInstanceOf[AxisExpression] || child.isInstanceOf[RootExpression]) {
            val ref =
              if (variable.isGlobal)
                new GlobalVariableReference(variable.asInstanceOf[GlobalVariable])
              else
                new LocalVariableReference(variable.asInstanceOf[LocalBinding])
            copyLocationInfo(child, ref)
            val path = ExpressionTool.makePathExpression(ref, child)
            info.setChildExpression(path)
            changed = true
          } else changed |= factorOutDot(child, variable)
        }
      }
    }
    if (changed)
      exp.resetLocalStaticProperties()
    changed
  }

  def inlineVariableReferences(expr: Expression, binding: Binding, replacement: Expression): Boolean =
    inlineVariableReferencesInternal(expr, binding, replacement)

  def inlineVariableReferencesInternal(expr: Expression, binding: Binding, replacement: Expression): Boolean =
    if (expr.isInstanceOf[TryCatch] && !replacement.isInstanceOf[Literal]) {
      false
    } else {
      var found = false

      for (o <- expr.operands.asScala) {
        val child = o.getChildExpression
        child match {
          case varRef: VariableReference if varRef.getBinding eq binding =>
            var copy: Expression = null
            try {
              copy = replacement.copy(new RebindingMap)
              ExpressionTool.copyLocationInfo(child, copy)
            } catch {
              case _: UnsupportedOperationException =>
                copy = replacement
            }
            o.setChildExpression(copy)
            found = true
          case _ =>
            found |= inlineVariableReferencesInternal(child, binding, replacement)
        }
      }
      if (found)
        expr.resetLocalStaticProperties()
      found
    }

  def replaceTrivialCallsToCurrent(expr: Expression): Boolean = {
    var found = false

    for (o <- expr.operands.asScala) {
      if (o.hasSameFocus) {
        val child = o.getChildExpression
        if (child.isCallOn(classOf[Current])) {
          val `var` = new CurrentItemExpression
          ExpressionTool.copyLocationInfo(child, `var`)
          o.setChildExpression(`var`)
          found = true
        }
        else found = replaceTrivialCallsToCurrent(child)
      }
    }
    if (found)
      expr.resetLocalStaticProperties()
    found
  }

  def replaceCallsToCurrent(expr: Expression, binding: LocalBinding): Boolean = {
    var found = false

    for (o <- expr.operands.asScala) {
      val child = o.getChildExpression
      if (child.isCallOn(classOf[Current])) {
        val varRef = new LocalVariableReference(binding)
        ExpressionTool.copyLocationInfo(child, varRef)
        o.setChildExpression(varRef)
        binding.addReference(varRef, isLoopingReference = true)
        found = true
      } else
        found = replaceCallsToCurrent(child, binding)
    }
    if (found)
      expr.resetLocalStaticProperties()
    found
  }

  def isNotAllowedInUpdatingContext(exp: Expression): Boolean = !exp.isUpdatingExpression && !exp.isVacuousExpression

  // ORBEON: No support for current directory.
  def getCurrentDirectory: String = {
    null
//    var dir: String = null
//    try
//      dir = System.getProperty("user.dir")
//    catch {
//      case _: Exception =>
//        return null
//    }
//    if (!dir.endsWith("/"))
//      dir = dir + '/'
//    val currentDirectoryURL = new File(dir).toURI
//    currentDirectoryURL.toString
  }

  @throws[XPathException]
  def getBaseURI(env: StaticContext, locator: SourceLocator, fail: Boolean): URI = {
    var expressionBaseURI: URI = null
    var base: String = null
    try {
      base = env.getStaticBaseURI
      if (base == null)
        base = getCurrentDirectory
      if (base != null)
        expressionBaseURI = new URI(base)
    } catch {
      case _: URISyntaxException =>
        val esc = IriToUri.iriToUri(base).toString
        try expressionBaseURI = new URI(esc)
        catch {
          case _: URISyntaxException =>
            expressionBaseURI = null
        }
        if (expressionBaseURI == null && fail) {
          val err = new XPathException("The base URI " + Err.wrap(env.getStaticBaseURI, Err.URI) + " is not a valid URI")
          err.setLocator(locator)
          throw err
        }
    }
    expressionBaseURI
  }

  def parenthesize(exp: Expression): String =
    if (exp.operands.iterator.hasNext)
      "(" + exp.toString + ")"
    else
      exp.toString

  def parenthesizeShort(exp: Expression): String =
    if (hasTwoOrMoreOperands(exp))
      "(" + exp.toShortString + ")"
    else
      exp.toShortString

  private def hasTwoOrMoreOperands(exp: Expression): Boolean = {
    val ops = exp.operands.iterator
    if (! ops.hasNext)
      return false
    ops.next
    ops.hasNext
  }

  def validateTree(exp: Expression): Unit =
    try {
      for (o <- exp.checkedOperands.asScala)
        validateTree(o.getChildExpression)
    } catch {
      case e: IllegalStateException =>
        e.printStackTrace()
    }

  def isLocalConstructor(child: Expression): Boolean = {
    var childExp = child
    if (! (childExp.isInstanceOf[ParentNodeConstructor] || childExp.isInstanceOf[SimpleNodeConstructor]))
      return false
    var parent = childExp.getParentExpression
    while (parent != null) {
      if (parent.isInstanceOf[ParentNodeConstructor])
        return true
      val o = findOperand(parent, childExp)
      if (o.getUsage ne OperandUsage.TRANSMISSION)
        return false
      childExp = parent
      parent = parent.getParentExpression
    }
    false
  }
}
