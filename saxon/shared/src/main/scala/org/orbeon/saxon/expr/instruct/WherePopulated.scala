package org.orbeon.saxon.expr.instruct

import org.orbeon.saxon.event.Outputter

import org.orbeon.saxon.event.WherePopulatedOutputter

import org.orbeon.saxon.expr._

import org.orbeon.saxon.expr.parser.RebindingMap

import org.orbeon.saxon.ma.map.MapItem

import org.orbeon.saxon.model.Type

import org.orbeon.saxon.om.Item

import org.orbeon.saxon.om.NodeInfo

import org.orbeon.saxon.om.SequenceIterator

import org.orbeon.saxon.trace.ExpressionPresenter

import Expression._

import org.orbeon.saxon.value.Base64BinaryValue

import org.orbeon.saxon.value.HexBinaryValue

import org.orbeon.saxon.value.StringValue

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
