package net.sf.saxon.expr.flwor

import net.sf.saxon.utils.Configuration

import net.sf.saxon.event.Outputter

import net.sf.saxon.expr._

import net.sf.saxon.expr.parser._

import net.sf.saxon.functions.Count

import net.sf.saxon.model.Affinity

import net.sf.saxon.model.ItemType

import net.sf.saxon.model.TypeHierarchy

import net.sf.saxon.om.Item

import net.sf.saxon.om.Sequence

import net.sf.saxon.om.SequenceIterator

import net.sf.saxon.trace.ExpressionPresenter

import net.sf.saxon.trans.XPathException

import net.sf.saxon.tree.iter.ListIterator

import net.sf.saxon.value.Cardinality

import net.sf.saxon.value.EmptySequence

import net.sf.saxon.value.Int64Value

import net.sf.saxon.value.SequenceType

import net.sf.saxon.z.IntHashMap

import java.util.Iterator

import java.util.List

import net.sf.saxon.expr.flwor.Clause.ClauseName.WINDOW

import WindowClause._

import scala.beans.{BeanProperty, BooleanBeanProperty}

import Clause.ClauseName.ClauseName
import Affinity._

object WindowClause {

  val WINDOW_VAR: Int = 0

  val START_ITEM: Int = 1

  val START_ITEM_POSITION: Int = 2

  val START_PREVIOUS_ITEM: Int = 3

  val START_NEXT_ITEM: Int = 4

  val END_ITEM: Int = 5

  val END_ITEM_POSITION: Int = 6

  val END_PREVIOUS_ITEM: Int = 7

  val END_NEXT_ITEM: Int = 8

  def makeValue(item: Item): Sequence =
    if (item == null) {
      EmptySequence.getInstance
    } else {
      item
    }

  class Window {

    var startItem: Item = _

    var startPosition: Int = _

    var startPreviousItem: Item = _

    var startNextItem: Item = _

    var endItem: Item = _

    var endPosition: Int = 0

    var endPreviousItem: Item = _

    var endNextItem: Item = _

    var contents: List[Item] = _

    var isDespatched: Boolean = false

    def isFinished(): Boolean = endPosition > 0

    def getIsDespatched(): Boolean = isDespatched

  }

}

class WindowClause extends Clause {

  private var sliding: Boolean = _

  @BooleanBeanProperty
  var includeUnclosedWindows: Boolean = true

  private var sequenceOp: Operand = _

  private var startConditionOp: Operand = _

  private var endConditionOp: Operand = _

  private var windowVars: IntHashMap[LocalVariableBinding] = new IntHashMap(10)

  @BeanProperty
  var itemTypeChecker: ItemTypeCheckingFunction = _

  @BooleanBeanProperty
  var windowMustBeSingleton: Boolean = _

  override def getClauseKey(): ClauseName = WINDOW

  def setIsSlidingWindow(sliding: Boolean): Unit = {
    this.sliding = sliding
  }

  def isSlidingWindow(): Boolean = sliding

  def isTumblingWindow(): Boolean = !sliding

  def initSequence(flwor: FLWORExpression, sequence: Expression): Unit = {
    sequenceOp = new Operand(flwor, sequence, OperandRole.INSPECT)
  }

  def setSequence(sequence: Expression): Unit = {
    sequenceOp.setChildExpression(sequence)
  }

  def getSequence(): Expression = sequenceOp.getChildExpression

  def initStartCondition(flwor: FLWORExpression,
                         startCondition: Expression): Unit = {
    startConditionOp = new Operand(flwor, startCondition, OperandRole.INSPECT)
  }

  def setStartCondition(startCondition: Expression): Unit = {
    startConditionOp.setChildExpression(startCondition)
  }

  def getStartCondition(): Expression = startConditionOp.getChildExpression

  def initEndCondition(flwor: FLWORExpression,
                       endCondition: Expression): Unit = {
    endConditionOp = new Operand(flwor, endCondition, OperandRole.INSPECT)
  }

  def setEndCondition(endCondition: Expression): Unit = {
    endConditionOp.setChildExpression(endCondition)
  }

  def getEndCondition(): Expression =
    if (endConditionOp == null) null else endConditionOp.getChildExpression

  def setVariableBinding(role: Int, binding: LocalVariableBinding): Unit = {
    var iter: Iterator[LocalVariableBinding] = windowVars.valueIterator()
    while (iter.hasNext) if (iter
      .next()
      .getVariableQName == binding.getVariableQName) {
      throw new XPathException(
        "Two variables in a window clause cannot have the same name (" +
          binding.getVariableQName.getDisplayName +
          ")",
        "XQST0103")
    }
    windowVars.put(role, binding)
  }

  def getVariableBinding(role: Int): LocalVariableBinding =
    windowVars.get(role)

  override def typeCheck(visitor: ExpressionVisitor,
                         contextInfo: ContextItemStaticInfo): Unit = {
    val requiredType: SequenceType = getVariableBinding(
      WindowClause.WINDOW_VAR).getRequiredType
    val required: ItemType = requiredType.getPrimaryType
    val supplied: ItemType = getSequence.getItemType
    val config: Configuration = visitor.getConfiguration
    val th: TypeHierarchy = config.getTypeHierarchy
    val rel: Affinity.Affinity = th.relationship(required, supplied)
    rel match {
      case SAME_TYPE | SUBSUMES =>
      case OVERLAPS | SUBSUMED_BY =>
        var role: RoleDiagnostic = new RoleDiagnostic(
          RoleDiagnostic.VARIABLE,
          getVariableBinding(WindowClause.WINDOW_VAR).getVariableQName.getDisplayName,
          0)
        itemTypeChecker =
          new ItemTypeCheckingFunction(required, role, getLocation, config)
      case DISJOINT =>
        var message: String = "The items in the window will always be instances of " +
          supplied +
          ", never of the required type " +
          required
        throw new XPathException(message, "XPTY0004", getLocation)

    }
    windowMustBeSingleton =
      !Cardinality.allowsMany(requiredType.getCardinality)
    if (requiredType.getCardinality == StaticProperty.ALLOWS_ZERO) {
      val message: String =
        "The value of the window variable can never be an empty sequence"
      throw new XPathException(message, "XPTY0004", getLocation)
    }
  }

  def checkWindowContents(w: Window): Unit = {
    if (windowMustBeSingleton && w.contents.size > 1) {
      throw new XPathException(
        "Required type of window allows only a single item; window has length " +
          w.contents.size,
        "XPTY0004",
        getLocation)
    }
    val checker: ItemTypeCheckingFunction = getItemTypeChecker
    if (checker != null) {
      val check: SequenceIterator =
        new ItemMappingIterator(new ListIterator(w.contents), checker)
      Count.count(check)
    }
  }

  override def copy(flwor: FLWORExpression, rebindings: RebindingMap): Clause = {
    val wc: WindowClause = new WindowClause()
    wc.setLocation(getLocation)
    wc.setPackageData(getPackageData)
    wc.sliding = sliding
    wc.includeUnclosedWindows = includeUnclosedWindows
    wc.initSequence(flwor, getSequence.copy(rebindings))
    wc.initStartCondition(flwor, getStartCondition.copy(rebindings))
    if (getEndCondition != null) {
      wc.initEndCondition(flwor, getEndCondition.copy(rebindings))
    }
    wc.windowVars = windowVars
    wc
  }

  override def getPullStream(base: TuplePull,
                             context: XPathContext): TuplePull =
    new WindowClausePull(base, this, context)

  override def getPushStream(destination: TuplePush,
                             output: Outputter,
                             context: XPathContext): TuplePush =
    new WindowClausePush(output, destination, this)

  override def processOperands(processor: OperandProcessor): Unit = {
    processor.processOperand(sequenceOp)
    processor.processOperand(startConditionOp)
    if (endConditionOp != null) {
      processor.processOperand(endConditionOp)
    }
  }

  override def addToPathMap(pathMap: PathMap,
                            pathMapNodeSet: PathMap.PathMapNodeSet): Unit = {
    throw new UnsupportedOperationException(
      "Cannot use document projection with windowing")
  }

  override def getRangeVariables(): Array[LocalVariableBinding] = {
    val vars: Array[LocalVariableBinding] =
      Array.ofDim[LocalVariableBinding](windowVars.size)
    var i: Int = 0
    val iter: Iterator[LocalVariableBinding] = windowVars.valueIterator()
    while (iter.hasNext) i += 1
    vars(i) = iter.next()
    vars
  }

  override def explain(out: ExpressionPresenter): Unit = {
    out.startElement(
      if (isSlidingWindow) "slidingWindow" else "tumblingWindow")
    out.startSubsidiaryElement("select")
    getSequence.export(out)
    out.endSubsidiaryElement()
    out.startSubsidiaryElement("start")
    getStartCondition.export(out)
    out.endSubsidiaryElement()
    if (endConditionOp != null) {
      out.startSubsidiaryElement("end")
      getEndCondition.export(out)
      out.endSubsidiaryElement()
    }
    out.endElement()
  }

  def matchesStart(previous: Item,
                             current: Item,
                             next: Item,
                             position: Int,
                             context: XPathContext): Boolean = {
    val clause: WindowClause = this
    var binding: LocalVariableBinding = null
    binding = clause.getVariableBinding(WindowClause.START_ITEM)
    if (binding != null) {
      context.setLocalVariable(binding.getLocalSlotNumber, current)
    }
    binding = clause.getVariableBinding(WindowClause.START_ITEM_POSITION)
    if (binding != null) {
      context.setLocalVariable(binding.getLocalSlotNumber,
        new Int64Value(position))
    }
    binding = clause.getVariableBinding(WindowClause.START_NEXT_ITEM)
    if (binding != null) {
      context.setLocalVariable(binding.getLocalSlotNumber, makeValue(next))
    }
    binding = clause.getVariableBinding(WindowClause.START_PREVIOUS_ITEM)
    if (binding != null) {
      context.setLocalVariable(binding.getLocalSlotNumber, makeValue(previous))
    }
    clause.getStartCondition.effectiveBooleanValue(context)
  }

  def matchesEnd(window: Window,
                           previous: Item,
                           current: Item,
                           next: Item,
                           position: Int,
                           context: XPathContext): Boolean = {
    val clause: WindowClause = this
    var binding: LocalVariableBinding = null
    binding = clause.getVariableBinding(WindowClause.START_ITEM)
    if (binding != null) {
      context.setLocalVariable(binding.getLocalSlotNumber, window.startItem)
    }
    binding = clause.getVariableBinding(WindowClause.START_ITEM_POSITION)
    if (binding != null) {
      context.setLocalVariable(binding.getLocalSlotNumber,
        new Int64Value(window.startPosition))
    }
    binding = clause.getVariableBinding(WindowClause.START_NEXT_ITEM)
    if (binding != null) {
      context.setLocalVariable(binding.getLocalSlotNumber,
        makeValue(window.startNextItem))
    }
    binding = clause.getVariableBinding(WindowClause.START_PREVIOUS_ITEM)
    if (binding != null) {
      context.setLocalVariable(binding.getLocalSlotNumber,
        makeValue(window.startPreviousItem))
    }
    binding = clause.getVariableBinding(WindowClause.END_ITEM)
    if (binding != null) {
      context.setLocalVariable(binding.getLocalSlotNumber, current)
    }
    binding = clause.getVariableBinding(WindowClause.END_ITEM_POSITION)
    if (binding != null) {
      context.setLocalVariable(binding.getLocalSlotNumber,
        new Int64Value(position))
    }
    binding = clause.getVariableBinding(WindowClause.END_NEXT_ITEM)
    if (binding != null) {
      context.setLocalVariable(binding.getLocalSlotNumber, makeValue(next))
    }
    binding = clause.getVariableBinding(WindowClause.END_PREVIOUS_ITEM)
    if (binding != null) {
      context.setLocalVariable(binding.getLocalSlotNumber, makeValue(previous))
    }
    clause.getEndCondition.effectiveBooleanValue(context)
  }

}
