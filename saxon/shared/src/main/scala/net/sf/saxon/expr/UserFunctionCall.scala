package net.sf.saxon.expr

import net.sf.saxon.event.Outputter
import net.sf.saxon.expr.instruct.Block
import net.sf.saxon.expr.instruct.UserFunction
import net.sf.saxon.expr.parser._
import net.sf.saxon.model.AnyItemType
import net.sf.saxon.model.ItemType
import net.sf.saxon.model.UType
import net.sf.saxon.om._
import net.sf.saxon.trace.ExpressionPresenter
import net.sf.saxon.trans._
import net.sf.saxon.tree.util.FastStringBuffer
import net.sf.saxon.value.Cardinality
import net.sf.saxon.value.EmptySequence
import net.sf.saxon.value.SequenceType
import net.sf.saxon.value.Whitespace
import java.util.ArrayList
import java.util.List

import UserFunctionCall._
import net.sf.saxon.expr.TailCallLoop.{TailCallComponent, TailCallFunction}

import scala.beans.{BeanProperty, BooleanBeanProperty}
import scala.jdk.CollectionConverters._
import Expression._

object UserFunctionCall {

  val NOT_TAIL_CALL: Int = 0

  val FOREIGN_TAIL_CALL: Int = 1

  val SELF_TAIL_CALL: Int = 2

  private val UNHANDLED_DEPENDENCIES: Int = StaticProperty.DEPENDS_ON_POSITION | StaticProperty.DEPENDS_ON_LAST |
    StaticProperty.DEPENDS_ON_XSLT_CONTEXT |
    StaticProperty.DEPENDS_ON_USER_FUNCTIONS

  private var depth: Int = 0

}

class UserFunctionCall extends FunctionCall
  with UserFunctionResolvable
  with ComponentInvocation
  with ContextOriginator {

  private var staticType: SequenceType = _

  var function: UserFunction = _

  var bindingSlot: Int = -1

  private var tailCall: Int = NOT_TAIL_CALL

  private var name: StructuredQName = _

  @BooleanBeanProperty
  var beingInlined: Boolean = false

  @BeanProperty
  var argumentEvaluators: Array[Evaluator] = null

   def setFunction(compiledFunction: UserFunction): Unit = {
    function = compiledFunction
  }

  def getFunction: UserFunction = function

  def setBindingSlot(slot: Int): Unit = {
    this.bindingSlot = slot
  }

  def getBindingSlot: Int = bindingSlot

  def setFunctionName(name: StructuredQName): Unit = {
    this.name = name
  }

  def setStaticType(`type`: SequenceType): Unit = {
    staticType = `type`
  }

  def getFixedTarget(): Component = {
    val v: Visibility.Visibility = function.getDeclaringComponent.getVisibility
    if (v == Visibility.PRIVATE || v == Visibility.FINAL) {
      function.getDeclaringComponent
    } else {
      null
    }
  }

  def isTailCall: Boolean = tailCall != NOT_TAIL_CALL

  def isRecursiveTailCall: Boolean = tailCall == SELF_TAIL_CALL

  def getFunctionName: StructuredQName =
    if (name == null) {
      function.getFunctionName
    } else {
      name
    }

  def getSymbolicName(): SymbolicName =
    new SymbolicName.F(getFunctionName, getArity)

  def getTarget: Component = function.getDeclaringComponent

  def setArgumentEvaluationModes(evalModes: Array[EvaluationMode.EvaluationMode]): Unit = {
    argumentEvaluators = Array.ofDim[Evaluator](evalModes.length)
    for (i <- 0 until evalModes.length) {
      argumentEvaluators(i) = evalModes(i).getEvaluator
    }
  }

  def allocateArgumentEvaluators(): Unit = {
    argumentEvaluators = Array.ofDim[Evaluator](getArity)
    var i: Int = 0
    for (o <- operands.asScala) {
      val arg: Expression = o.getChildExpression
      val required: SequenceType = function.getArgumentType(i)
      val cardinality: Int = required.getCardinality
      argumentEvaluators(i) =
        if (i == 0 && function.getDeclaredStreamability.isConsuming)
          Evaluator.STREAMING_ARGUMENT
        else if (function.getParameterDefinitions()(i).isIndexedVariable)
          Evaluator.MAKE_INDEXED_VARIABLE
        else if (arg.isInstanceOf[Literal]) Evaluator.LITERAL
        else if (arg.isInstanceOf[VariableReference]) Evaluator.VARIABLE
        else if (cardinality == StaticProperty.EXACTLY_ONE)
          Evaluator.SINGLE_ITEM
        else if ((arg.getDependencies & UNHANDLED_DEPENDENCIES) != 0)
          Evaluator.EAGER_SEQUENCE
        else if (!Cardinality.allowsMany(arg.getCardinality) && arg.getCost < 20)
          Evaluator.EAGER_SEQUENCE
        else if (cardinality == StaticProperty.ALLOWS_ZERO_OR_ONE)
          Evaluator.OPTIONAL_ITEM
        else if (arg.isInstanceOf[Block] && arg
          .asInstanceOf[Block]
          .isCandidateForSharedAppend) Evaluator.SHARED_APPEND
        else Evaluator.MEMO_CLOSURE
      i += 1
    }
  }

  override def preEvaluate(visitor: ExpressionVisitor): Expression = this

  def getItemType: ItemType =
    if (staticType == null) {
      AnyItemType
    } else {
      staticType.getPrimaryType
    }

  override def getStaticUType(contextItemType: UType): UType = {
    val f: UserFunction = getFunction
    if (f == null) {
      UType.ANY
    }
    f.getResultType.getPrimaryType.getUType
  }

  override def getIntrinsicDependencies: Int =
    StaticProperty.DEPENDS_ON_USER_FUNCTIONS

  override def isUpdatingExpression(): Boolean = function.isUpdating

  override def computeSpecialProperties(): Int =
    if (function == null) {
      super.computeSpecialProperties()
    } else if (function.getBody != null &&
      (function.getDeclaredVisibility == Visibility.PRIVATE ||
        function.getDeclaredVisibility == Visibility.FINAL)) {
      var props: Int = 0
      val calledFunctions: List[UserFunction] = new ArrayList[UserFunction]()
      ExpressionTool.gatherCalledFunctions(function.getBody, calledFunctions)
      props =
        if (calledFunctions.isEmpty) function.getBody.getSpecialProperties
        else super.computeSpecialProperties()
      if (function.getDeterminism != UserFunction.Determinism.PROACTIVE) {
        props |= StaticProperty.NO_NODES_NEWLY_CREATED
      }
      props
    } else {
      super.computeSpecialProperties()
    }

  def copy(rebindings: RebindingMap): Expression = {
    if (function == null) {
      throw new UnsupportedOperationException("UserFunctionCall.copy()")
    }
    val ufc: UserFunctionCall = new UserFunctionCall()
    ufc.setFunction(function)
    ufc.setStaticType(staticType)
    val numArgs: Int = getArity
    val a2: Array[Expression] = Array.ofDim[Expression](numArgs)
    for (i <- 0 until numArgs) {
      a2(i) = getArg(i).copy(rebindings)
    }
    ufc.setArguments(a2)
    ExpressionTool.copyLocationInfo(this, ufc)
    ufc
  }

  def computeCardinality(): Int =
    if (staticType == null) {
      StaticProperty.ALLOWS_ZERO_OR_MORE
    } else {
      staticType.getCardinality
    }

  override def typeCheck(visitor: ExpressionVisitor,
                         contextInfo: ContextItemStaticInfo): Expression = {
    val e: Expression = super.typeCheck(visitor, contextInfo)
    if (e != this) {
      return e
    }
    if (function != null) {
      checkFunctionCall(function, visitor)
      if (staticType == null || staticType == SequenceType.ANY_SEQUENCE) {
        staticType = function.getResultType
      }
    }
    this
  }

  override def optimize(visitor: ExpressionVisitor,
                        contextItemType: ContextItemStaticInfo): Expression = {
    val e: Expression = super.optimize(visitor, contextItemType)
    if (e == this && function != null) {
      visitor
        .obtainOptimizer()
        .tryInlineFunctionCall(this, visitor, contextItemType)
    }
    e
  }

  override def resetLocalStaticProperties(): Unit = {
    super.resetLocalStaticProperties()
    argumentEvaluators = null
  }

  override def addToPathMap(
                             pathMap: PathMap,
                             pathMapNodeSet: PathMap.PathMapNodeSet): PathMap.PathMapNodeSet =
    addExternalFunctionCallToPathMap(pathMap, pathMapNodeSet)

  override def markTailFunctionCalls(qName: StructuredQName, arity: Int): Int = {
    tailCall =
      if (getFunctionName == qName && arity == getArity) SELF_TAIL_CALL
      else FOREIGN_TAIL_CALL
    tailCall
  }

  override def getImplementationMethod: Int =
    if (Cardinality.allowsMany(getCardinality)) {
      ITERATE_METHOD | PROCESS_METHOD
    } else {
      EVALUATE_METHOD
    }

  override def evaluateItem(c: XPathContext): Item = callFunction(c).head

  override def iterate(c: XPathContext): SequenceIterator = callFunction(c).iterate()

  override def evaluatePendingUpdates(context: XPathContext,
                                      pul: PendingUpdateList): Unit = {
    val actualArgs: Array[Sequence] = evaluateArguments(context)
    val c2: XPathContextMajor = context.newCleanContext()
    c2.setOrigin(this)
    function.callUpdating(actualArgs, c2, pul)
  }

  private def callFunction(context: XPathContext): Sequence = {
    var targetFunction: UserFunction = null
    val actualArgs: Array[Sequence] = evaluateArguments(context)
    var c2: XPathContextMajor = null
    if (isTailCall) {
      requestTailCall(context, actualArgs)
      EmptySequence.getInstance
    }
    if (bindingSlot >= 0) {
      val target: Component = getTargetComponent(context)
      if (target.isHiddenAbstractComponent) {
        throw new XPathException(
          "Cannot call an abstract function (" + name.getDisplayName +
            ") with no implementation",
          "XTDE3052")
      }
      targetFunction = target.getActor.asInstanceOf[UserFunction]
      c2 = targetFunction.makeNewContext(context, this)
      c2.setCurrentComponent(target)
      c2.setOrigin(this)
    } else {
      targetFunction = function
      c2 = targetFunction.makeNewContext(context, this)
      c2.setOrigin(this)
    }
    try targetFunction.call(c2, actualArgs)
    catch {
      case e: UncheckedXPathException => {
        val xe: XPathException = e.getXPathException
        xe.maybeSetLocation(getLocation)
        throw xe
      }

      case err: StackOverflowError =>
        throw new XPathException.StackOverflow(
          "Too many nested function calls. May be due to infinite recursion",
          SaxonErrorCode.SXLM0001,
          getLocation)

    }
  }

  private def requestTailCall(context: XPathContext,
                              actualArgs: Array[Sequence]): Unit = {
    if (bindingSlot >= 0) {
      val info: TailCallComponent =
        new TailCallLoop.TailCallComponent()
      val target: Component = getTargetComponent(context)
      info.component = target
      info.function = target.getActor.asInstanceOf[UserFunction]
      if (target.isHiddenAbstractComponent) {
        throw new XPathException(
          "Cannot call an abstract function (" + name.getDisplayName +
            ") with no implementation",
          "XTDE3052")
      }
      context.asInstanceOf[XPathContextMajor].requestTailCall(info, actualArgs)
    } else {
      val info: TailCallFunction =
        new TailCallLoop.TailCallFunction()
      info.function = function
      context.asInstanceOf[XPathContextMajor].requestTailCall(info, actualArgs)
    }
  }

  override def process(output: Outputter, context: XPathContext): Unit = {
    val actualArgs: Array[Sequence] = evaluateArguments(context)
    if (isTailCall) {
      requestTailCall(context, actualArgs)
      return
    }
    if (bindingSlot >= 0) {
      val target: Component = getTargetComponent(context)
      val targetFunction: UserFunction =
        target.getActor.asInstanceOf[UserFunction]
      if (target.getVisibility == Visibility.ABSTRACT) {
        throw new XPathException(
          "Cannot call a function defined with visibility=abstract",
          "XTDE3052")
      }
      val c2: XPathContextMajor = targetFunction.makeNewContext(context, this)
      c2.setCurrentComponent(target)
      c2.setOrigin(this)
      targetFunction.process(c2, actualArgs, output)
    } else {
      val c2: XPathContextMajor = function.makeNewContext(context, this)
      c2.setOrigin(this)
      function.process(c2, actualArgs, output)
    }
  }

  def getTargetComponent(context: XPathContext): Component =
    if (bindingSlot == -1) {
      function.getDeclaringComponent
    } else {
      context.getTargetComponent(bindingSlot)
    }

  def getTargetFunction(context: XPathContext): UserFunction =
    getTargetComponent(context).getActor.asInstanceOf[UserFunction]

  override def evaluateArguments(c: XPathContext): Array[Sequence] =
    evaluateArguments(c, streamed = false)

  def evaluateArguments(c: XPathContext, streamed: Boolean): Array[Sequence] = {
    val numArgs: Int = getArity
    val actualArgs: Array[Sequence] = SequenceTool.makeSequenceArray(numArgs)
    this.synchronized {
      if (argumentEvaluators == null) {
        allocateArgumentEvaluators()
      }
    }
    for (i <- 0 until numArgs) {
      var eval: Evaluator = argumentEvaluators(i)
      if (eval == Evaluator.STREAMING_ARGUMENT && !streamed) {
        eval = Evaluator.EAGER_SEQUENCE
      }
      actualArgs(i) = eval.evaluate(getArg(i), c)
      if (actualArgs(i) == null) {
        actualArgs(i) = EmptySequence.getInstance
      }
    }
    actualArgs
  }

  override def export(out: ExpressionPresenter): Unit = {
    out.startElement("ufCall", this)
    if (getFunctionName != null) {
      out.emitAttribute("name", getFunctionName)
      out.emitAttribute("tailCall",
        if (tailCall == NOT_TAIL_CALL) "false"
        else if (tailCall == SELF_TAIL_CALL) "self"
        else "foreign")
    }
    out.emitAttribute("bSlot", "" + getBindingSlot)
    if (argumentEvaluators != null && getArity > 0) {
      val fsb: FastStringBuffer = new FastStringBuffer(FastStringBuffer.C64)
      for (e <- argumentEvaluators) {
        fsb.append(e.getEvaluationMode.getCode.toString + " ")
      }
      out.emitAttribute("eval", Whitespace.trim(fsb))
    }
    for (o <- operands.asScala) {
      o.getChildExpression.export(out)
    }
    if (getFunctionName == null) {
      out.setChildRole("inline")
      function.getBody.export(out)
      out.endElement()
    }
    out.endElement()
  }

  override def getExpressionName: String = "userFunctionCall"

  override def getProperty(name: String): AnyRef = {
    if (name.==("target")) {
      return function
    }
    super.getProperty(name)
  }

  override def getObjectName(): StructuredQName = getFunctionName

}
