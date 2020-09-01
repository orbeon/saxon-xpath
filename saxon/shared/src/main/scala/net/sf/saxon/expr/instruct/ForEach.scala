package net.sf.saxon.expr.instruct

import net.sf.saxon.utils.Controller

import net.sf.saxon.event.Outputter

import net.sf.saxon.event.PipelineConfiguration

import net.sf.saxon.expr._

import net.sf.saxon.expr.parser._

import net.sf.saxon.lib.TraceListener

import net.sf.saxon.model.ErrorType

import net.sf.saxon.model.ItemType

import net.sf.saxon.model.SchemaType

import net.sf.saxon.model.UType

import net.sf.saxon.om.FocusIterator

import net.sf.saxon.om.Item

import net.sf.saxon.om.SequenceIterator

import net.sf.saxon.om.StandardNames

import net.sf.saxon.trace.ExpressionPresenter

import net.sf.saxon.trans.XPathException

import net.sf.saxon.value.Cardinality

import Expression._

class ForEach(select: Expression,
              action: Expression,
              containsTailCall: Boolean,
              threads: Expression)
  extends Instruction
    with ContextMappingFunction
    with ContextSwitchingExpression {

  var containTailCall: Boolean = containsTailCall && action
    .isInstanceOf[TailCallReturner]

  var selectOp: Operand =
    new Operand(this, select, OperandRole.FOCUS_CONTROLLING_SELECT)

  var actionOp: Operand =
    new Operand(this, action, OperandRole.FOCUS_CONTROLLED_ACTION)

  var threadsOp: Operand = _

  var isInstr: Boolean = _

  if (threads != null) {
    threadsOp = new Operand(this, threads, OperandRole.SINGLE_ATOMIC)
  }

  def this(select: Expression, action: Expression) =
    this(select, action, false, null)

  def setInstruction(inst: Boolean): Unit = {
    isInstr = inst
  }

  override def isInstruction(): Boolean = isInstr

  def getSelect: Expression = selectOp.getChildExpression

  def setSelect(select: Expression): Unit = {
    selectOp.setChildExpression(select)
  }

  def getAction: Expression = actionOp.getChildExpression

  def setAction(action: Expression): Unit = {
    actionOp.setChildExpression(action)
  }

  def getThreads: Expression =
    if (threadsOp == null) null else threadsOp.getChildExpression

  def setThreads(threads: Expression): Unit = {
    if (threads != null) {
      if (threadsOp == null) {
        threadsOp = new Operand(this, threads, OperandRole.SINGLE_ATOMIC)
      } else {
        threadsOp.setChildExpression(threads)
      }
    }
  }

  override def operands: java.lang.Iterable[Operand] =
    if (threadsOp == null) {
      operandList(selectOp, actionOp)
    } else {
      operandList(selectOp, actionOp, threadsOp)
    }

  override def getInstructionNameCode(): Int = StandardNames.XSL_FOR_EACH

  def getSelectExpression(): Expression = getSelect

  def setSelectExpression(select: Expression): Unit = {
    this.setSelect(select)
  }

  def setActionExpression(action: Expression): Unit = {
    this.setAction(action)
  }

  def getActionExpression(): Expression = getAction

  def getNumberOfThreadsExpression: Expression = getThreads

  override def getItemType: ItemType = getAction.getItemType

  override def getStaticUType(contextItemType: UType): UType =
    if (isInstruction) {
      super.getStaticUType(contextItemType)
    } else {
      getAction.getStaticUType(getSelect.getStaticUType(contextItemType))
    }

  override def mayCreateNewNodes(): Boolean = {
    val props: Int = getAction.getSpecialProperties
    (props & StaticProperty.NO_NODES_NEWLY_CREATED) == 0
  }

  override def typeCheck(visitor: ExpressionVisitor,
                         contextInfo: ContextItemStaticInfo): Expression = {
    selectOp.typeCheck(visitor, contextInfo)
    val selectType: ItemType = getSelect.getItemType
    if (selectType == ErrorType) {
      Literal.makeEmptySequence()
    }
    val cit: ContextItemStaticInfo = visitor.getConfiguration
      .makeContextItemStaticInfo(getSelect.getItemType, maybeUndefined = false)
    cit.setContextSettingExpression(getSelect)
    actionOp.typeCheck(visitor, cit)
    if (!Cardinality.allowsMany(getSelect.getCardinality)) {
      actionOp.setOperandRole(
        actionOp.getOperandRole.modifyProperty(OperandRole.SINGLETON, on = true))
    }
    this
  }

  override def optimize(visitor: ExpressionVisitor,
                        contextInfo: ContextItemStaticInfo): Expression = {
    selectOp.optimize(visitor, contextInfo)
    val cit: ContextItemStaticInfo = visitor.getConfiguration
      .makeContextItemStaticInfo(getSelect.getItemType, maybeUndefined = false)
    cit.setContextSettingExpression(getSelect)
    actionOp.optimize(visitor, cit)
    if (!visitor.isOptimizeForStreaming) {
      if (Literal.isEmptySequence(getSelect)) {
        getSelect
      }
      if (Literal.isEmptySequence(getAction)) {
        getAction
      }
    }
    if (getSelect.getCardinality == StaticProperty.EXACTLY_ONE &&
      getAction.isInstanceOf[AxisExpression]) {
      new SimpleStepExpression(getSelect, getAction)
    }
    if (threadsOp != null && !Literal.isEmptySequence(getThreads)) {
      visitor.obtainOptimizer().generateMultithreadedInstruction(this)
    }
    this
  }

  override def unordered(retainAllNodes: Boolean,
                         forStreaming: Boolean): Expression = {
    this.setSelect(getSelect.unordered(retainAllNodes, forStreaming))
    this.setAction(getAction.unordered(retainAllNodes, forStreaming))
    this
  }

  override def addToPathMap(
                             pathMap: PathMap,
                             pathMapNodeSet: PathMap.PathMapNodeSet): PathMap.PathMapNodeSet = {
    val target: PathMap.PathMapNodeSet =
      getSelect.addToPathMap(pathMap, pathMapNodeSet)
    getAction.addToPathMap(pathMap, target)
  }

  def copy(rebindings: RebindingMap): Expression = {
    val f2: ForEach = new ForEach(getSelect.copy(rebindings),
      getAction.copy(rebindings),
      containTailCall,
      getThreads)
    ExpressionTool.copyLocationInfo(this, f2)
    f2.setInstruction(isInstruction)
    f2
  }

  override def computeSpecialProperties(): Int = {
    var p: Int = super.computeSpecialProperties()
    if (getSelect.getCardinality == StaticProperty.EXACTLY_ONE) {
      p |= getAction.getSpecialProperties
    } else {
      p |= getAction.getSpecialProperties & StaticProperty.ALL_NODES_UNTYPED
    }
    p
  }

  override def alwaysCreatesNewNodes(): Boolean =
    (getAction.isInstanceOf[Instruction]) &&
      getAction.asInstanceOf[Instruction].alwaysCreatesNewNodes()

  override def isUpdatingExpression(): Boolean = getAction.isUpdatingExpression

  override def checkForUpdatingSubexpressions(): Unit = {
    if (getSelect.isUpdatingExpression) {
      val err = new XPathException(
        "Updating expression appears in a context where it is not permitted",
        "XUST0001")
      err.setLocation(getSelect.getLocation)
      throw err
    }
  }

  override def getImplementationMethod: Int =
    ITERATE_METHOD | PROCESS_METHOD | WATCH_METHOD | ITEM_FEED_METHOD

  override def checkPermittedContents(parentType: SchemaType, whole: Boolean): Unit = {
    getAction.checkPermittedContents(parentType, whole = false)
  }

  def processLeavingTail(output: Outputter, context: XPathContext): TailCall = {
    val controller: Controller = context.getController
    assert(controller != null)
    val c2: XPathContextMajor = context.newContext()
    c2.setOrigin(this)
    val iter: FocusIterator = c2.trackFocus(getSelect.iterate(context))
    c2.setCurrentTemplateRule(null)
    val action: Expression = getAction
    if (containTailCall) {
      if (controller.isTracing) {
        val listener: TraceListener = controller.getTraceListener
        assert(listener != null)
        val item: Item = iter.next()
        if (item == null) {
          return null
        }
        listener.startCurrentItem(item)
        val tc: TailCall =
          action.asInstanceOf[TailCallReturner].processLeavingTail(output, c2)
        listener.endCurrentItem(item)
        return tc
      } else {
        val item: Item = iter.next()
        if (item == null) {
          return null
        }
        action.asInstanceOf[TailCallReturner].processLeavingTail(output, c2)
      }
    } else {
      val pipe: PipelineConfiguration = output.getPipelineConfiguration
      pipe.setXPathContext(c2)
      if (controller.isTracing) {
        val listener: TraceListener = controller.getTraceListener
        assert(listener != null)
        var item: Item = null
        while (({
          item = iter.next()
          item
        }) != null) {
          listener.startCurrentItem(item)
          action.process(output, c2)
          listener.endCurrentItem(item)
        }
      } else {
        iter.forEachOrFail((item) => action.process(output, c2))
      }
      pipe.setXPathContext(context)
    }
    null
  }

  override def iterate(context: XPathContext): SequenceIterator = {
    val c2: XPathContextMinor = context.newMinorContext()
    c2.trackFocus(getSelect.iterate(context))
    new ContextMappingIterator(this, c2)
  }

  def map(context: XPathContext): SequenceIterator = getAction.iterate(context)

  override def evaluatePendingUpdates(context: XPathContext,
                                      pul: PendingUpdateList): Unit = {
    val c2: XPathContextMinor = context.newMinorContext()
    c2.trackFocus(getSelect.iterate(context))
    val iter: SequenceIterator = c2.getCurrentIterator
    var item: Item = null
    while (({
      item = iter.next()
      item
    }) != null) getAction.evaluatePendingUpdates(c2,
      pul)
  }

  def export(out: ExpressionPresenter): Unit = {
    out.startElement("forEach", this)
    getSelect.export(out)
    getAction.export(out)
    explainThreads(out)
    out.endElement()
  }

  def explainThreads(out: ExpressionPresenter): Unit = ()

  override def toString: String =
    ExpressionTool.parenthesize(getSelect) + " ! " + ExpressionTool
      .parenthesize(getAction)

  override def toShortString: String =
    getSelect.toShortString + "!" + getAction.toShortString

  override def getExpressionName: String = "forEach"

  override def getStreamerName: String = "ForEach"

}
