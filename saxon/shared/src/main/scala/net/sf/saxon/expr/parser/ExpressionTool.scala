


package net.sf.saxon.expr.parser

import net.sf.saxon.utils.Configuration
import net.sf.saxon.utils.Controller
import net.sf.saxon.event.ComplexContentOutputter
import net.sf.saxon.event.SequenceCollector
import net.sf.saxon.expr._
import net.sf.saxon.expr.flwor.Clause
import net.sf.saxon.expr.flwor.FLWORExpression
import net.sf.saxon.expr.flwor.LocalVariableBinding
import net.sf.saxon.expr.instruct._
import net.sf.saxon.expr.sort.ConditionalSorter
import net.sf.saxon.expr.sort.DocumentSorter
import net.sf.saxon.functions._
import net.sf.saxon.lib.Logger
import net.sf.saxon.lib.NamespaceConstant
import net.sf.saxon.lib.StandardLogger
import net.sf.saxon.ma.arrays.ArrayItem
import net.sf.saxon.ma.map.MapItem
import net.sf.saxon.model.AnyItemType
import net.sf.saxon.model.ItemType
import net.sf.saxon.om._
import net.sf.saxon.pattern.Pattern
import scala.jdk.CollectionConverters._
import net.sf.saxon.trans.Err
import net.sf.saxon.trans.NoDynamicContextException
import net.sf.saxon.trans.SymbolicName
import net.sf.saxon.trans.XPathException
import net.sf.saxon.tree.util.FastStringBuffer
import net.sf.saxon.value._
import javax.xml.transform.SourceLocator
import java.io.File
import java.net.URI
import java.net.URISyntaxException
import java.util
import java.util.function.Predicate

import net.sf.saxon.query.QueryModule


object ExpressionTool {

  @throws[XPathException]
  def make(expression: String, env: StaticContext, start: Int, terminator: Int, codeInjector: CodeInjector) = {
    var rTerminator = terminator
    val languageLevel = env.getXPathVersion
    val parser = env.getConfiguration.newExpressionParser("XP", false, languageLevel)
    if (codeInjector != null) parser.setCodeInjector(codeInjector)
    if (terminator == -1)
      rTerminator = Token.EOF
    var exp = parser.parse(expression, start, terminator, env)

    setDeepRetainedStaticContext(exp, env.makeRetainedStaticContext)
    exp = exp.simplify
    exp
  }


  def setDeepRetainedStaticContext(exp: Expression, rsc: RetainedStaticContext): Unit = {
    var rSC = rsc
    if (exp.getLocalRetainedStaticContext == null) exp.setRetainedStaticContextLocally(rsc)
    else rSC = exp.getLocalRetainedStaticContext

    for (o <- exp.operands.asScala) {
      setDeepRetainedStaticContext(o.getChildExpression, rsc)
    }
  }


  def copyLocationInfo(from: Expression, to: Expression) = if (from != null && to != null) {
    if (to.getLocation == null || (to.getLocation eq Loc.NONE)) to.setLocation(from.getLocation)
    if (to.getLocalRetainedStaticContext == null) to.setRetainedStaticContextLocally(from.getLocalRetainedStaticContext)
  }


  @throws[XPathException]
  def unsortedIfHomogeneous(exp: Expression, forStreaming: Boolean): Expression = {
    if (exp.isInstanceOf[Literal]) return exp
    if (exp.getItemType.isInstanceOf[AnyItemType]) exp
    else exp.unordered(false, forStreaming)
  }


  def injectCode(exp: Expression, injector: CodeInjector): Expression = {
    if (exp.isInstanceOf[FLWORExpression]) exp.asInstanceOf[FLWORExpression].injectCode(injector)
    else if (!exp.isInstanceOf[TraceExpression]) {

      for (o <- exp.operands.asScala) {
        o.setChildExpression(injectCode(o.getChildExpression, injector))
      }
    }
    injector.inject(exp)
  }


  def lazyEvaluator(exp: Expression, repeatable: Boolean) = if (exp.isInstanceOf[Literal]) Evaluator.LITERAL
  else if (exp.isInstanceOf[VariableReference]) Evaluator.VARIABLE
  else if (exp.isInstanceOf[SuppliedParameterReference]) Evaluator.SUPPLIED_PARAMETER
  else if ((exp.getDependencies & (StaticProperty.DEPENDS_ON_POSITION | StaticProperty.DEPENDS_ON_LAST | StaticProperty.DEPENDS_ON_CURRENT_ITEM |
    StaticProperty.DEPENDS_ON_CURRENT_GROUP | StaticProperty.DEPENDS_ON_REGEX_GROUP)) != 0) {
    eagerEvaluator(exp)
  }
  else if (exp.isInstanceOf[ErrorExpression]) {
    Evaluator.SINGLE_ITEM

  }
  else if (!Cardinality.allowsMany(exp.getCardinality)) {
    eagerEvaluator(exp)
  }
  else if (exp.isInstanceOf[TailExpression]) {

    val tail = exp.asInstanceOf[TailExpression]
    val base = tail.getBaseExpression
    if (base.isInstanceOf[VariableReference]) Evaluator.LAZY_TAIL
    else if (repeatable) Evaluator.MEMO_CLOSURE
    else Evaluator.LAZY_SEQUENCE
  }
  else if (exp.isInstanceOf[Block] && exp.asInstanceOf[Block].isCandidateForSharedAppend) {


    Evaluator.SHARED_APPEND
  }
  else if (repeatable) {
    Evaluator.MEMO_CLOSURE
  }
  else Evaluator.LAZY_SEQUENCE


  def eagerEvaluator(exp: Expression): Evaluator = {
    if (exp.isInstanceOf[Literal] && !(exp.asInstanceOf[Literal]).getValue.isInstanceOf[Closure]) return Evaluator.LITERAL
    if (exp.isInstanceOf[VariableReference]) return Evaluator.VARIABLE
    val m = exp.getImplementationMethod
    if (((m & Expression.EVALUATE_METHOD)  != 0) && !Cardinality.allowsMany(exp.getCardinality)) if (Cardinality.allowsZero(exp.getCardinality)) Evaluator.OPTIONAL_ITEM
    else Evaluator.SINGLE_ITEM
    else if ((m & Expression.ITERATE_METHOD) != 0) Evaluator.EAGER_SEQUENCE
    else Evaluator.PROCESS
  }


  @throws[XPathException]
  def lazyEvaluate(exp: Expression, context: XPathContext, repeatable: Boolean) = {
    val evaluator = lazyEvaluator(exp, repeatable)
    evaluator.evaluate(exp, context)
  }


  @throws[XPathException]
  def eagerEvaluate(exp: Expression, context: XPathContext) = {
    val evaluator = eagerEvaluator(exp)
    evaluator.evaluate(exp, context).materialize
  }


  def markTailFunctionCalls(exp: Expression, qName: StructuredQName, arity: Int) = exp.markTailFunctionCalls(qName, arity)


  def indent(level: Int) = {
    val fsb = new FastStringBuffer(level)
    for (i <- 0 until level) {
      fsb.append("  ")
    }
    fsb.toString
  }


  def contains(a: Expression, b: Expression): Boolean = {
    var temp = b
    while ( {
      temp != null
    }) if (temp eq a) return true
    else temp = temp.getParentExpression
    false
  }


  def containsLocalParam(exp: Expression) = contains(exp, true, (e: Expression) => e.isInstanceOf[LocalParam])


  def containsLocalVariableReference(exp: Expression) = contains(exp, false, (e: Expression) => {
    def foo(e: Expression) = {
      if (e.isInstanceOf[LocalVariableReference]) {
        val vref = e.asInstanceOf[LocalVariableReference]
        val binding = vref.getBinding
        !(binding.isInstanceOf[Expression] && contains(exp, binding.asInstanceOf[Expression]))
      }
      false
    }

    foo(e)
  })


  def contains(exp: Expression, sameFocusOnly: Boolean, predicate: Predicate[Expression]): Boolean = {
    if (predicate.test(exp)) return true

    for (info <- exp.operands.asScala) {
      if ((info.hasSameFocus || !sameFocusOnly) && contains(info.getChildExpression, sameFocusOnly, predicate)) return true
    }
    false
  }


  def changesXsltContext(exp: Expression): Boolean = {
    var expression = exp
    expression = exp.getInterpretedExpression
    if (exp.isInstanceOf[ResultDocument] || exp.isInstanceOf[CallTemplate] || exp.isInstanceOf[ApplyTemplates] || exp.isInstanceOf[NextMatch] || exp.isInstanceOf[ApplyImports] || exp.isCallOn(classOf[RegexGroup]) || exp.isCallOn(classOf[CurrentGroup])) return true

    for (o <- exp.operands.asScala) {
      if (changesXsltContext(o.getChildExpression)) return true
    }
    false
  }


  def isLoopingSubexpression(child: Expression, ancestor: Expression): Boolean = {
    var childExp = child
    val parent = childExp.getParentExpression
    var result = false
    while (true) {
      if (parent == null) result = false
      if (hasLoopingSubexpression(parent, childExp)) result = true
      if (parent eq ancestor) result = false
      childExp = parent
    }
    result
  }

  def isLoopingReference(reference: VariableReference, binding: Binding): Boolean = {
    var child: Expression = reference
    var parent = child.getParentExpression
    while ( {
      true
    }) {
      if (parent == null) {
        return true
      }
      if (parent.isInstanceOf[FLWORExpression]) if (parent.hasVariableBinding(binding)) {
        return parent.asInstanceOf[FLWORExpression].hasLoopingVariableReference(binding)
      }
      else {
        if (hasLoopingSubexpression(parent, child)) return true
      }
      else if (parent.getExpressionName == "tryCatch") return true
      else {
        if (parent.isInstanceOf[ForEachGroup] && parent.hasVariableBinding(binding)) return false
        if (hasLoopingSubexpression(parent, child)) return true
        if (parent.hasVariableBinding(binding)) return false
      }
      child = parent
      parent = child.getParentExpression
    }
    false
  }

  def hasLoopingSubexpression(parent: Expression, child: Expression): Boolean = {

    for (info <- parent.operands.asScala) {
      if (info.getChildExpression eq child) return info.isEvaluatedRepeatedly
    }
    false
  }


  def getFocusSettingContainer(exp: Expression): Expression = {
    var child = exp
    var parent = child.getParentExpression
    while ( {
      parent != null
    }) {
      val o = findOperand(parent, child)
      if (o == null) throw new AssertionError
      if (!o.hasSameFocus) return parent
      child = parent
      parent = child.getParentExpression
    }
    null
  }


  def getContextDocumentSettingContainer(exp: Expression): Expression = {
    var child = exp
    var parent = child.getParentExpression
    while ( {
      parent != null
    }) {
      if (parent.isInstanceOf[ContextSwitchingExpression]) {
        val switcher = parent.asInstanceOf[ContextSwitchingExpression]
        if (child eq switcher.getActionExpression) if (switcher.getSelectExpression.hasSpecialProperty(StaticProperty.CONTEXT_DOCUMENT_NODESET)) {
          parent.resetLocalStaticProperties()
          parent.getSpecialProperties
          return getContextDocumentSettingContainer(parent)
        }
      }
      val o = findOperand(parent, child)
      if (o == null) throw new AssertionError
      if (!o.hasSameFocus) return parent
      child = parent
      parent = child.getParentExpression
    }
    null
  }


  def resetStaticProperties(exp: Expression) = {
    var expression = exp
    var i: Int = 0
    while ( {
      expression != null
    }) {
      expression.resetLocalStaticProperties()
      expression = expression.getParentExpression
      i += 1
      if (i > 100000) throw new IllegalStateException("Loop in parent expression chain")
    }
  }


  def equalOrNull(x: Any, y: Any) = if (x == null) y == null
  else x == y


  @throws[XPathException]
  def getIteratorFromProcessMethod(exp: Expression, context: XPathContext) = {
    val controller = context.getController
    assert(controller != null)
    val seq = controller.allocateSequenceOutputter
    exp.process(new ComplexContentOutputter(seq), context)
    seq.close()
    seq.iterate
  }


  @throws[XPathException]
  def getItemFromProcessMethod(exp: Expression, context: XPathContext) = {
    val controller = context.getController
    if (controller == null) throw new NoDynamicContextException("No controller available")
    val seq = controller.allocateSequenceOutputter(1)
    exp.process(new ComplexContentOutputter(seq), context)
    seq.close()
    val result = seq.getFirstItem
    seq.reset()
    result
  }


  def allocateSlots(exp: Expression, nextFree: Int, frame: SlotManager): Int = {
    var nextFreeCount = nextFree
    if (exp.isInstanceOf[Assignation]) {
      exp.asInstanceOf[Assignation].setSlotNumber(nextFreeCount)
      val count = exp.asInstanceOf[Assignation].getRequiredSlots
      nextFreeCount += count
      if (frame != null) frame.allocateSlotNumber(exp.asInstanceOf[Assignation].getVariableQName())
    }
    if (exp.isInstanceOf[LocalParam] && exp.asInstanceOf[LocalParam].getSlotNumber < 0) exp.asInstanceOf[LocalParam].setSlotNumber({
      nextFreeCount += 1
      nextFreeCount
    })
    if (exp.isInstanceOf[FLWORExpression]) {

      for (c <- exp.asInstanceOf[FLWORExpression].getClauseList.asScala) {
        for (b <- c.getRangeVariables) {
          b.setSlotNumber({
            nextFreeCount += 1
            nextFreeCount
          })
          frame.allocateSlotNumber(b.getVariableQName)
        }
      }
    }
    if (exp.isInstanceOf[VariableReference]) {
      val `var` = exp.asInstanceOf[VariableReference]
      val binding = `var`.getBinding
      if (exp.isInstanceOf[LocalVariableReference]) `var`.asInstanceOf[LocalVariableReference].setSlotNumber(binding.asInstanceOf[LocalBinding].getLocalSlotNumber())
      if (binding.isInstanceOf[Assignation] && binding.asInstanceOf[LocalBinding].getLocalSlotNumber() < 0) {


        val decl = binding.asInstanceOf[Assignation]
        var err: Logger = null
        try err = exp.getConfiguration.getLogger
        catch {
          case ex: Exception =>
            err = new StandardLogger
        }
        val msg = "*** Internal Saxon error: local variable encountered whose binding has been deleted"
        err.error(msg)
        err.error("Variable name: " + decl.getVariableName)
        err.error("Line number of reference: " + `var`.getLocation.getLineNumber + " in " + `var`.getLocation.getSystemId)
        err.error("Line number of declaration: " + decl.getLocation.getLineNumber + " in " + decl.getLocation.getSystemId)
        err.error("DECLARATION:")
        try decl.explain(err)
        catch {
          case e: Exception =>


        }
        throw new IllegalStateException(msg)
      }
    }
    if (exp.isInstanceOf[Pattern]) nextFreeCount = exp.asInstanceOf[Pattern].allocateSlots(frame, nextFreeCount)
    //else if (exp.isInstanceOf[ScopedBindingElement]) nextFree = exp.asInstanceOf[ScopedBindingElement].allocateSlots(frame, nextFreeCount) // ScopedBindingElement not exist
    else {

      for (o <- exp.operands.asScala) {
        nextFreeCount = allocateSlots(o.getChildExpression, nextFree, frame)
      }
    }
    nextFree


  }


  @throws[XPathException]
  def effectiveBooleanValue(iterator: SequenceIterator): Boolean = {
    val first = iterator.next
    if (first == null) return false
    if (first.isInstanceOf[NodeInfo]) {
      iterator.close()
      return true
    }
    else if (first.isInstanceOf[AtomicValue]) if (first.isInstanceOf[BooleanValue]) {
      if (iterator.next != null) {
        iterator.close()
        ebvError("a sequence of two or more items starting with a boolean")
      }
      iterator.close()
      return first.asInstanceOf[BooleanValue].getBooleanValue
    }
    else if (first.isInstanceOf[StringValue]) {
      if (iterator.next != null) {
        iterator.close()
        ebvError("a sequence of two or more items starting with a string")
      }
      return !(first.asInstanceOf[StringValue]).isZeroLength
    }
    else if (first.isInstanceOf[NumericValue]) {
      if (iterator.next != null) {
        iterator.close()
        ebvError("a sequence of two or more items starting with a numeric value")
      }
      val n = first.asInstanceOf[NumericValue]
      return (n.compareTo(0) != 0) && !n.isNaN
    }
    else {
      iterator.close()
      ebvError("a sequence starting with an atomic value of type " + first.asInstanceOf[AtomicValue].getItemType.getTypeName.getDisplayName)
      return false
    }
    else if (first.isInstanceOf[Function]) {
      iterator.close()
      if (first.isInstanceOf[ArrayItem]) {
        ebvError("a sequence starting with an array item (" + first.toShortString + ")")
        return false
      }
      else if (first.isInstanceOf[MapItem]) {
        ebvError("a sequence starting with a map (" + first.toShortString + ")")
        return false
      }
      else {
        ebvError("a sequence starting with a function (" + first.toShortString + ")")
        return false
      }
    }
    else if (first.isInstanceOf[ObjectValue[_]]) {
      if (iterator.next != null) {
        iterator.close()
        ebvError("a sequence of two or more items starting with an external object value")
      }
      return true
    }
    ebvError("a sequence starting with an item of unknown kind")
    false
  }


  @throws[XPathException]
  def effectiveBooleanValue(item: Item): Boolean = {
    if (item == null) return false
    if (item.isInstanceOf[NodeInfo]) true
    else if (item.isInstanceOf[AtomicValue]) if (item.isInstanceOf[BooleanValue]) item.asInstanceOf[BooleanValue].getBooleanValue
    else if (item.isInstanceOf[StringValue]) !(item.asInstanceOf[StringValue]).isZeroLength
    else if (item.isInstanceOf[NumericValue]) {
      val n = item.asInstanceOf[NumericValue]
      (n.compareTo(0) != 0) && !n.isNaN
    }
    else if (item.isInstanceOf[ExternalObject[_]]) true
    else {
      ebvError("an atomic value of type " + item.asInstanceOf[AtomicValue].getPrimitiveType.getDisplayName)
      false
    }
    else {
      ebvError(item.getGenre.toString)
      false
    }
  }


  @throws[XPathException]
  def ebvError(reason: String) = {
    val err = new XPathException("Effective boolean value is not defined for " + reason)
    err.setErrorCode("FORG0006")
    err.setIsTypeError(true)
    throw err
  }

  @throws[XPathException]
  def ebvError(reason: String, cause: Expression) = {
    val err = new XPathException("Effective boolean value is not defined for " + reason)
    err.setErrorCode("FORG0006")
    err.setIsTypeError(true)
    err.setFailingExpression(cause)
    throw err
  }


  def dependsOnFocus(exp: Expression): Boolean = (exp.getDependencies & StaticProperty.DEPENDS_ON_FOCUS) != 0


  def dependsOnVariable(exp: Expression, bindingList: Array[Binding]) = !(bindingList == null || bindingList.length == 0) && contains(exp, false, (e: Expression) => {
    def foo(e: Expression) = {
      if (e.isInstanceOf[VariableReference]) for (binding <- bindingList) {
        if (e.asInstanceOf[VariableReference].getBinding eq binding) true
      }
      false
    }

    foo(e)
  })


  def gatherReferencedVariables(e: Expression, list: util.List[Binding]): Unit = if (e.isInstanceOf[VariableReference]) {
    val binding = e.asInstanceOf[VariableReference].getBinding
    if (!list.contains(binding)) list.add(binding)
  }
  else {

    for (o <- e.operands.asScala) {
      if (!o.getOperandRole.isInChoiceGroup) gatherReferencedVariables(o.getChildExpression, list)
    }
  }


  def refersToVariableOrFunction(exp: Expression) = contains(exp, false, (e: Expression) => e.isInstanceOf[VariableReference] ||
    e.isInstanceOf[UserFunctionCall] || e.isInstanceOf[Binding] || e.isInstanceOf[CallTemplate] || e.isInstanceOf[ApplyTemplates] || e.isInstanceOf[ApplyImports] ||
    isCallOnSystemFunction(e, "function-lookup") || e.isCallOn(classOf[ApplyFn]))

  def isCallOnSystemFunction(e: Expression, localName: String) = e.isInstanceOf[StaticFunctionCall] && localName == e.asInstanceOf[StaticFunctionCall].
    getFunctionName.getLocalPart


  def callsFunction(exp: Expression, qName: StructuredQName, sameFocusOnly: Boolean) = contains(exp, sameFocusOnly,
    (e: Expression) => e.isInstanceOf[FunctionCall] && qName == e.asInstanceOf[FunctionCall].getFunctionName())


  def containsSubexpression(exp: Expression, subClass: Class[_ <: Expression]) = contains(exp, false,
    (e: Expression) => subClass.isAssignableFrom(e.getClass))


  def gatherCalledFunctions(e: Expression, list: util.List[UserFunction]): Unit = if (e.isInstanceOf[UserFunctionCall]) {
    val function = e.asInstanceOf[UserFunctionCall].getFunction
    if (!list.contains(function)) list.add(function)
  }
  else {

    for (o <- e.operands.asScala) {
      gatherCalledFunctions(o.getChildExpression, list)
    }
  }


  def gatherCalledFunctionNames(e: Expression, list: util.List[SymbolicName]): Unit = if (e.isInstanceOf[UserFunctionCall]) list.add(e.asInstanceOf[UserFunctionCall].getSymbolicName)
  else {

    for (o <- e.operands.asScala) {
      gatherCalledFunctionNames(o.getChildExpression, list)
    }
  }

  // Compilation,ExpressionContext scala class not exist

  @throws[XPathException]
  def optimizeComponentBody(body: Expression/*, compilation: Compilation*/, visitor: ExpressionVisitor, cisi: ContextItemStaticInfo, extractGlobals: Boolean): Expression = { //
    val config = visitor.getConfiguration
    val opt = visitor.obtainOptimizer
    val env = visitor.getStaticContext
    var compileWithTracing = config.isCompileWithTracing
    var expBody = body
    if (!compileWithTracing) {
     /* if (compilation != null) compileWithTracing = compilation.getCompilerInfo.isCompileWithTracing
      else*/ if (env.isInstanceOf[QueryModule]) compileWithTracing = env.asInstanceOf[QueryModule].getUserQueryContext.isCompileWithTracing
      //else if (env.isInstanceOf[ExpressionContext]) compileWithTracing = env.asInstanceOf[ExpressionContext].getStyleElement.getCompilation.getCompilerInfo.isCompileWithTracing
    }
    if (opt.isOptionSet(OptimizerOptions.MISCELLANEOUS) && !compileWithTracing) {
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
      if (opt.isOptionSet(OptimizerOptions.LOOP_LIFTING)) expBody = LoopLifter.process(expBody, visitor, cisi)
    }
    else expBody = avoidDocumentSort(expBody)
    if (!visitor.isOptimizeForStreaming) expBody = opt.eliminateCommonSubexpressions(expBody)
    opt.injectByteCodeCandidates(expBody)
    opt.prepareForStreaming(expBody)
    computeEvaluationModesForUserFunctionCalls(expBody)
    expBody.restoreParentPointers()
    expBody
  }


  private def avoidDocumentSort(exp: Expression): Expression = {
    if (exp.isInstanceOf[DocumentSorter]) {
      val base = exp.asInstanceOf[DocumentSorter].getBaseExpression
      if (base.hasSpecialProperty(StaticProperty.ORDERED_NODESET)) return base
      return exp
    }
    else if (exp.isInstanceOf[ConditionalSorter]) {
      val sorter = exp.asInstanceOf[ConditionalSorter].getDocumentSorter
      val eliminatedSorter = avoidDocumentSort(sorter)
      if (eliminatedSorter ne sorter) return eliminatedSorter
    }

    for (o <- exp.operands.asScala) {
      o.setChildExpression(avoidDocumentSort(o.getChildExpression))
    }
    exp
  }


  @throws[XPathException]
  def computeEvaluationModesForUserFunctionCalls(exp: Expression) = ExpressionTool.processExpressionTree(exp, null, (expression: Expression, result: Any) => {
    def foo(expression: Expression, result: Any) = {
      if (expression.isInstanceOf[UserFunctionCall]) expression.asInstanceOf[UserFunctionCall].allocateArgumentEvaluators()
      if (expression.isInstanceOf[LocalParam]) expression.asInstanceOf[LocalParam].computeEvaluationMode()
      false
    }

    foo(expression, result)
  })


  @throws[XPathException]
  def clearStreamabilityData(exp: Expression) = ExpressionTool.processExpressionTree(exp, null, (expression: Expression, result: Any) => {
    def foo(expression: Expression, result: Any) = {
      expression.setExtraProperty("P+S", null)
      expression.setExtraProperty("inversion", null)
      false
    }

    foo(expression, result)
  })


  def resetPropertiesWithinSubtree(exp: Expression): Unit = {
    exp.resetLocalStaticProperties()
    if (exp.isInstanceOf[LocalVariableReference]) {
      val ref = exp.asInstanceOf[LocalVariableReference]
      val binding = ref.getBinding
      if (binding.isInstanceOf[Assignation]) binding.addReference(ref, ref.isInLoop)
    }

    for (o <- exp.operands.asScala) {
      resetPropertiesWithinSubtree(o.getChildExpression)
      o.getChildExpression.setParentExpression(exp)
    }
  }


  def resolveCallsToCurrentFunction(exp: Expression): Expression = if (exp.isCallOn(classOf[Current])) {
    val cie = new ContextItemExpression
    copyLocationInfo(exp, cie)
    cie
  }
  else {
    if (callsFunction(exp, Current.FN_CURRENT, true)) {
      replaceTrivialCallsToCurrent(exp)
    }
    if (callsFunction(exp, Current.FN_CURRENT, false)) {
      val let = new LetExpression
      let.setVariableQName(new StructuredQName("vv", NamespaceConstant.SAXON_GENERATED_VARIABLE, "current" + exp.hashCode))
      let.setRequiredType(SequenceType.SINGLE_ITEM)
      let.setSequence(new CurrentItemExpression)
      replaceCallsToCurrent(exp, let)
      let.setAction(exp)
      let
    }
    else exp
  }


  def gatherVariableReferences(exp: Expression, binding: Binding, list: util.List[VariableReference]): Unit = if (exp.isInstanceOf[VariableReference] && (exp.asInstanceOf[VariableReference].getBinding eq binding)) list.add(exp.asInstanceOf[VariableReference])
  else {
    for (o <- exp.operands.asScala) {
      gatherVariableReferences(o.getChildExpression, binding, list)
    }
  }


  @throws[XPathException]
  def processExpressionTree(root: Expression, result: Any, action: ExpressionAction): Boolean = {
    var done = action.process(root, result.asInstanceOf[AnyRef])
    if (!done) {

      for (o <- root.operands.asScala) {
        done = processExpressionTree(o.getChildExpression, result, action)
        if (done) return true
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
      if (replaced) mustCpy = true
      val child = o.getChildExpression
      if (selector.matches(child)) {
        val e2 = if (mustCpy) replacement.copy(new RebindingMap)
        else replacement
        o.setChildExpression(e2)
        replaced = true
      }
      else replaced = replaceSelectedSubexpressions(child, selector, replacement, mustCpy)
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
    if (exp.isInstanceOf[VariableReference] && (exp.asInstanceOf[VariableReference].getBinding eq binding))
      if (exp.asInstanceOf[VariableReference].isFiltered) return FilterExpression.FILTERED
      else {
        if (inLoop)
          rcount += 10
        else
          rcount += 1
      }
    else if ((exp.getDependencies & StaticProperty.DEPENDS_ON_LOCAL_VARIABLES) != 0) return 0
    else {

      breakable {
        for (info <- exp.operands.asScala) {
          val child = info.getChildExpression
          val childLoop = inLoop || info.isEvaluatedRepeatedly
          rcount += getReferenceCount(child, binding, childLoop)
          if (rcount >= FilterExpression.FILTERED) break
        }
      }
    }
    rcount
  }


  def expressionSize(exp: Expression): Int = {
    var expres = exp
    expres = expres.getInterpretedExpression
    var total = 1

    for (o <- expres.operands.asScala) {
      total += expressionSize(o.getChildExpression)
    }
    total
  }


  def rebindVariableReferences(exp: Expression, oldBinding: Binding, newBinding: Binding): Unit =
    if (exp.isInstanceOf[VariableReference]) if (exp.asInstanceOf[VariableReference].getBinding eq oldBinding) exp.asInstanceOf[VariableReference].fixup(newBinding)
    else {
      for (o <- exp.operands.asScala) {
        rebindVariableReferences(o.getChildExpression, oldBinding, newBinding)
      }
    }


  def makePathExpression(start: Expression, step: Expression): Expression = {

    if (start.isInstanceOf[RootExpression] && step.isInstanceOf[AxisExpression] && step.asInstanceOf[AxisExpression].getAxis == AxisInfo.PARENT)
      return Literal.makeEmptySequence
    val expr = new SlashExpression(start, step)


    if (step.isInstanceOf[SlashExpression]) {
      val stepPath = step.asInstanceOf[SlashExpression]
      if (isFilteredAxisPath(stepPath.getSelectExpression) && isFilteredAxisPath(stepPath.getActionExpression)) {
        expr.setStart(ExpressionTool.makePathExpression(start, stepPath.getSelectExpression))
        expr.setStep(stepPath.getActionExpression)
      }
    }
    expr
  }


  def findOperand(parentExpression: Expression, childExpression: Expression): Operand = {

    for (o <- parentExpression.operands.asScala) {
      if (o.getChildExpression eq childExpression) return o
    }
    null
  }


  private def isFilteredAxisPath(exp: Expression): Boolean = unfilteredExpression(exp, true).isInstanceOf[AxisExpression]


  def unfilteredExpression(exp: Expression, allowPositional: Boolean): Expression =
    if (exp.isInstanceOf[FilterExpression] && (allowPositional || !(exp.asInstanceOf[FilterExpression]).isFilterIsPositional)) unfilteredExpression(exp.asInstanceOf[FilterExpression].getSelectExpression, allowPositional)
    else if (exp.isInstanceOf[SingleItemFilter] && allowPositional) unfilteredExpression(exp.asInstanceOf[SingleItemFilter].getBaseExpression, allowPositional)
    else exp


  def tryToFactorOutDot(exp: Expression, contextItemType: ItemType): Expression = if (exp.isInstanceOf[ContextItemExpression]) null
  else if (exp.isInstanceOf[LetExpression] && exp.asInstanceOf[LetExpression].getSequence.isInstanceOf[ContextItemExpression]) {
    val action = exp.asInstanceOf[LetExpression].getAction
    val changed = factorOutDot(action, exp.asInstanceOf[LetExpression])
    if (changed) exp.resetLocalStaticProperties()
    exp
  }
  else if ((exp.getDependencies & (StaticProperty.DEPENDS_ON_CONTEXT_ITEM | StaticProperty.DEPENDS_ON_CONTEXT_DOCUMENT)) != 0) {
    val let = new LetExpression
    let.setVariableQName(new StructuredQName("saxon", NamespaceConstant.SAXON, "dot" + exp.hashCode))
    let.setRequiredType(SequenceType.makeSequenceType(contextItemType, StaticProperty.EXACTLY_ONE))
    let.setSequence(new ContextItemExpression)
    let.setAction(exp)
    val changed = factorOutDot(exp, let)
    if (changed) let
    else exp
  }
  else null


  def factorOutDot(exp: Expression, variable: Binding): Boolean = {
    var changed = false
    if ((exp.getDependencies & (StaticProperty.DEPENDS_ON_CONTEXT_ITEM | StaticProperty.DEPENDS_ON_CONTEXT_DOCUMENT)) != 0) {

      for (info <- exp.operands.asScala) {
        if (info.hasSameFocus) {
          val child = info.getChildExpression
          if (child.isInstanceOf[ContextItemExpression]) {
            val ref = if (variable.isGlobal) new GlobalVariableReference(variable.asInstanceOf[GlobalVariable])
            else new LocalVariableReference(variable.asInstanceOf[LocalBinding])
            copyLocationInfo(child, ref)
            info.setChildExpression(ref)
            changed = true
          }
          else if (child.isInstanceOf[AxisExpression] || child.isInstanceOf[RootExpression]) {
            val ref = if (variable.isGlobal) new GlobalVariableReference(variable.asInstanceOf[GlobalVariable])
            else new LocalVariableReference(variable.asInstanceOf[LocalBinding])
            copyLocationInfo(child, ref)
            val path = ExpressionTool.makePathExpression(ref, child)
            info.setChildExpression(path)
            changed = true
          }
          else changed |= factorOutDot(child, variable)
        }
      }
    }
    if (changed) exp.resetLocalStaticProperties()
    changed
  }


  def inlineVariableReferences(expr: Expression, binding: Binding, replacement: Expression): Boolean = inlineVariableReferencesInternal(expr, binding, replacement)

  def inlineVariableReferencesInternal(expr: Expression, binding: Binding, replacement: Expression): Boolean = if (expr.isInstanceOf[TryCatch] && !replacement.isInstanceOf[Literal]) {

    false
  }
  else {
    var found = false

    for (o <- expr.operands.asScala) {
      val child = o.getChildExpression
      if (child.isInstanceOf[VariableReference] && (child.asInstanceOf[VariableReference].getBinding eq binding)) {
        var copy: Expression = null
        try {
          copy = replacement.copy(new RebindingMap)
          ExpressionTool.copyLocationInfo(child, copy)
        } catch {
          case err: UnsupportedOperationException =>
            copy = replacement
        }
        o.setChildExpression(copy)
        found = true
      }
      else found |= inlineVariableReferencesInternal(child, binding, replacement)
    }
    if (found) expr.resetLocalStaticProperties()
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
    if (found) expr.resetLocalStaticProperties()
    found
  }


  def replaceCallsToCurrent(expr: Expression, binding: LocalBinding): Boolean = {
    var found = false

    for (o <- expr.operands.asScala) {
      val child = o.getChildExpression
      if (child.isCallOn(classOf[Current])) {
        val `var` = new LocalVariableReference(binding)
        ExpressionTool.copyLocationInfo(child, `var`)
        o.setChildExpression(`var`)
        binding.addReference(`var`, true)
        found = true
      }
      else found = replaceCallsToCurrent(child, binding)
    }
    if (found) expr.resetLocalStaticProperties()
    found
  }


  def isNotAllowedInUpdatingContext(exp: Expression): Boolean = !exp.isUpdatingExpression && !exp.isVacuousExpression

  def getCurrentDirectory: String = {
    var dir: String = null
    try dir = System.getProperty("user.dir")
    catch {
      case geterr: Exception =>

        return null
    }
    if (!dir.endsWith("/")) dir = dir + '/'
    val currentDirectoryURL = new File(dir).toURI
    currentDirectoryURL.toString
  }


  @throws[XPathException]
  def getBaseURI(env: StaticContext, locator: SourceLocator, fail: Boolean): URI = {
    var expressionBaseURI: URI = null
    var base: String = null
    try {
      base = env.getStaticBaseURI
      if (base == null) base = getCurrentDirectory
      if (base != null) expressionBaseURI = new URI(base)
    } catch {
      case e: URISyntaxException =>

        val esc = IriToUri.iriToUri(base).toString
        try expressionBaseURI = new URI(esc)
        catch {
          case e2: URISyntaxException =>

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


  def parenthesize(exp: Expression): String = if (exp.operands.iterator.hasNext) "(" + exp.toString + ")"
  else exp.toString

  def parenthesizeShort(exp: Expression): String = if (hasTwoOrMoreOperands(exp)) "(" + exp.toShortString + ")"
  else exp.toShortString

  private def hasTwoOrMoreOperands(exp: Expression): Boolean = {
    val ops = exp.operands.iterator
    if (!ops.hasNext) return false
    ops.next
    ops.hasNext
  }

  def validateTree(exp: Expression): Unit = {
    try {
      for (o <- exp.checkedOperands.asScala) {
        validateTree(o.getChildExpression)
      }
    } catch {
      case e: IllegalStateException =>
        e.printStackTrace()
    }
  }


  def isLocalConstructor(child: Expression): Boolean = {
    var childExp = child
    if (!(childExp.isInstanceOf[ParentNodeConstructor] || childExp.isInstanceOf[SimpleNodeConstructor])) return false
    var parent = childExp.getParentExpression
    while ( {
      parent != null
    }) {
      if (parent.isInstanceOf[ParentNodeConstructor]) return true
      val o = findOperand(parent, childExp)
      if (o.getUsage ne OperandUsage.TRANSMISSION) return false
      childExp = parent
      parent = parent.getParentExpression
    }
    false
  }
}

class ExpressionTool private() {
}