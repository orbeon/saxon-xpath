package net.sf.saxon.expr.instruct

import net.sf.saxon.event.Outputter

import net.sf.saxon.event.WherePopulatedOutputter

import net.sf.saxon.expr._

import net.sf.saxon.expr.parser.RebindingMap

import net.sf.saxon.ma.map.MapItem

import net.sf.saxon.model.Type

import net.sf.saxon.om.Item

import net.sf.saxon.om.NodeInfo

import net.sf.saxon.om.SequenceIterator

import net.sf.saxon.trace.ExpressionPresenter

import Expression._

import net.sf.saxon.value.Base64BinaryValue

import net.sf.saxon.value.HexBinaryValue

import net.sf.saxon.value.StringValue

import WherePopulated._

object WherePopulated {

  def isDeemedEmpty(item: Item): Boolean =
    if (item.isInstanceOf[NodeInfo]) {
      val kind: Int = item.asInstanceOf[NodeInfo].getNodeKind
      kind match {
        case Type.DOCUMENT | Type.ELEMENT =>
          !item.asInstanceOf[NodeInfo].hasChildNodes
        case _ => item.getStringValueCS.length == 0

      }
    } else if (item.isInstanceOf[StringValue] || item
      .isInstanceOf[HexBinaryValue] ||
      item.isInstanceOf[Base64BinaryValue]) {
      item.getStringValueCS.length == 0
    } else if (item.isInstanceOf[MapItem]) {
      item.asInstanceOf[MapItem].isEmpty
    } else {
      false
    }

}

class WherePopulated(base: Expression)
  extends UnaryExpression(base)
    with ItemMappingFunction {

  override def isInstruction(): Boolean = true

   override def getOperandRole(): OperandRole =
    new OperandRole(0, OperandUsage.TRANSMISSION)

  def copy(rebindings: RebindingMap): Expression =
    new WherePopulated(getBaseExpression.copy(rebindings))

  override def getImplementationMethod: Int = ITERATE_METHOD | PROCESS_METHOD

  override def computeCardinality(): Int =
    super.computeCardinality() | StaticProperty.ALLOWS_ZERO

  override def iterate(context: XPathContext): SequenceIterator =
    new ItemMappingIterator(getBaseExpression.iterate(context), this)

  override def process(output: Outputter, context: XPathContext): Unit = {
    val filter: WherePopulatedOutputter = new WherePopulatedOutputter(output)
    getBaseExpression.process(filter, context)
  }

  def mapItem(item: Item): Item = if (isDeemedEmpty(item)) null else item

  override def getExpressionName: String = "wherePop"

 override def export(out: ExpressionPresenter): Unit = {
    out.startElement("condCont", this)
    getBaseExpression.export(out)
    out.endElement()
  }

  override def getStreamerName: String = "WherePopulated"

}
