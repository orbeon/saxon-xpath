package net.sf.saxon.pattern

import net.sf.saxon.expr._

import net.sf.saxon.expr.instruct.SlotManager

import net.sf.saxon.expr.parser.ContextItemStaticInfo

import net.sf.saxon.expr.parser.ExpressionTool

import net.sf.saxon.expr.parser.ExpressionVisitor

import net.sf.saxon.expr.parser.RebindingMap

import net.sf.saxon.functions.Current

import net.sf.saxon.model.ItemType

import net.sf.saxon.model.Type

import net.sf.saxon.model.UType

import net.sf.saxon.om.Item

import net.sf.saxon.om.NodeInfo

import net.sf.saxon.trace.ExpressionPresenter

import net.sf.saxon.trans.XPathException

import net.sf.saxon.tree.iter.ManualIterator

class BasePatternWithPredicate(basePattern: Pattern, predicate: Expression)
  extends Pattern
    with PatternWithPredicate {

  var basePatternOp: Operand =
    new Operand(this, basePattern, OperandRole.ATOMIC_SEQUENCE)

  var predicateOp: Operand =
    new Operand(this, predicate, OperandRole.FOCUS_CONTROLLED_ACTION)

  adoptChildExpression(getBasePattern)

  adoptChildExpression(getPredicate)

  def getPredicate(): Expression = predicateOp.getChildExpression

  def getBasePattern: Pattern =
    basePatternOp.getChildExpression.asInstanceOf[Pattern]

  override def bindCurrent(binding: LocalBinding): Unit = {
    val predicate: Expression = getPredicate
    if (predicate.isCallOn(classOf[Current])) {
      predicateOp.setChildExpression(new LocalVariableReference(binding))
    } else if (ExpressionTool.callsFunction(predicate,
      Current.FN_CURRENT,
      sameFocusOnly = false)) {
      Pattern.replaceCurrent(predicate, binding)
    }
    getBasePattern.bindCurrent(binding)
  }

  override def matchesCurrentGroup(): Boolean = getBasePattern.matchesCurrentGroup()

  override def operands: java.lang.Iterable[Operand] =
    operandList(basePatternOp, predicateOp)

  override def allocateSlots(slotManager: SlotManager, nextFree: Int): Int = {
    val n: Int =
      ExpressionTool.allocateSlots(getPredicate, nextFree, slotManager)
    getBasePattern.allocateSlots(slotManager, n)
  }

  override def matches(item: Item, context: XPathContext): Boolean = {
    if (!getBasePattern.matches(item, context)) {
      return false
    }
    matchesPredicate(item, context)
  }

  private def matchesPredicate(item: Item, context: XPathContext): Boolean = {
    val c2: XPathContext = context.newMinorContext()
    val si: ManualIterator = new ManualIterator(item)
    c2.setCurrentIterator(si)
    c2.setCurrentOutputUri(null)
    try getPredicate.effectiveBooleanValue(c2)
    catch {
      case e@(_: XPathException.Circularity |
              _: XPathException.StackOverflow) =>
        throw e

      case ex: XPathException => {
        handleDynamicError(ex, c2)
        false
      }

    }
  }

  override def matchesBeneathAnchor(node: NodeInfo,
                                    anchor: NodeInfo,
                                    context: XPathContext): Boolean =
    getBasePattern.matchesBeneathAnchor(node, anchor, context) &&
      matchesPredicate(node, context)

  override def getUType: UType = getBasePattern.getUType

  override def getFingerprint: Int = getBasePattern.getFingerprint

  override def getItemType: ItemType = getBasePattern.getItemType

  override def getDependencies(): Int = getPredicate.getDependencies

  override def typeCheck(visitor: ExpressionVisitor,
                         contextItemType: ContextItemStaticInfo): Pattern = {
    basePatternOp.setChildExpression(
      getBasePattern.typeCheck(visitor, contextItemType))
    val cit: ContextItemStaticInfo = visitor.getConfiguration
      .makeContextItemStaticInfo(getBasePattern.getItemType, maybeUndefined = false)
    predicateOp.setChildExpression(getPredicate.typeCheck(visitor, cit))
    this
  }

  override def optimize(visitor: ExpressionVisitor,
                        contextInfo: ContextItemStaticInfo): Pattern = {
    basePatternOp.setChildExpression(
      getBasePattern.optimize(visitor, contextInfo))
    val cit: ContextItemStaticInfo = visitor.getConfiguration
      .makeContextItemStaticInfo(getBasePattern.getItemType, maybeUndefined = false)
    predicateOp.setChildExpression(getPredicate.optimize(visitor, cit))
    predicateOp.setChildExpression(
      visitor.obtainOptimizer().eliminateCommonSubexpressions(getPredicate))
    this
  }

  override def convertToTypedPattern(`val`: String): Pattern = {
    val b2: Pattern = getBasePattern.convertToTypedPattern(`val`)
    if (b2 == getBasePattern) {
      this
    } else {
      new BasePatternWithPredicate(b2, getPredicate)
    }
  }

  override def toString: String = getBasePattern.toString + "[" + getPredicate.toString + "]"

  override def toShortString: String =
    getBasePattern.toShortString + "[" + getPredicate.toShortString +
      "]"

  def copy(rebindings: RebindingMap): Pattern = {
    val n: BasePatternWithPredicate = new BasePatternWithPredicate(
      getBasePattern.copy(rebindings),
      getPredicate.copy(rebindings))
    ExpressionTool.copyLocationInfo(this, n)
    n
  }

  override def equals(obj: Any): Boolean =
    obj.isInstanceOf[BasePatternWithPredicate] &&
      obj
        .asInstanceOf[BasePatternWithPredicate]
        .getBasePattern
        .isEqual(getBasePattern) &&
      obj
        .asInstanceOf[BasePatternWithPredicate]
        .getPredicate
        .isEqual(getPredicate)

  override def computeHashCode(): Int =
    getBasePattern.hashCode ^ getPredicate.hashCode

  def export(presenter: ExpressionPresenter): Unit = {
    presenter.startElement("p.withPredicate")
    getBasePattern.export(presenter)
    getPredicate.export(presenter)
    presenter.endElement()
  }

}
