package org.orbeon.saxon.pattern

import org.orbeon.saxon.expr.XPathContext

import org.orbeon.saxon.expr.parser.ExpressionTool

import org.orbeon.saxon.expr.parser.RebindingMap

import org.orbeon.saxon.model.AlphaCode

import org.orbeon.saxon.model.SchemaDeclaration

import org.orbeon.saxon.model.UType

import org.orbeon.saxon.om.Item

import org.orbeon.saxon.om.NodeInfo

import org.orbeon.saxon.trace.ExpressionPresenter

import org.orbeon.saxon.trans.XPathException

import scala.beans.{BeanProperty}

class NodeTestPattern(@BeanProperty var nodeTest: NodeTest) extends Pattern {

  this.priority = nodeTest.getDefaultPriority

  def matches(item: Item, context: XPathContext): Boolean =
    item.isInstanceOf[NodeInfo] && nodeTest.test(item.asInstanceOf[NodeInfo])

  override def getItemType: NodeTest = nodeTest

  override def getUType: UType = nodeTest.getUType

  override def getFingerprint: Int = nodeTest.getFingerprint

  override def toString: String = nodeTest.toString

  override def toShortString: String = nodeTest.toShortString

  override def equals(other: Any): Boolean =
    (other.isInstanceOf[NodeTestPattern]) &&
      other.asInstanceOf[NodeTestPattern].nodeTest == nodeTest

  override def computeHashCode(): Int = 0x7aeffea8 ^ nodeTest.hashCode

  override def convertToTypedPattern(`val`: String): Pattern =
    if (nodeTest
      .isInstanceOf[NameTest] && nodeTest.getUType == UType.ELEMENT) {
      val decl: SchemaDeclaration =
        getConfiguration.getElementDeclaration(nodeTest.getMatchingNodeName)
      if (decl == null) {
        if ("lax" == `val`) {
          this
        } else {
          throw new XPathException(
            "The mode specifies typed='strict', " +
              "but there is no schema element declaration named " +
              nodeTest,
            "XTSE3105")
        }
      } else {
        val schemaNodeTest: NodeTest = decl.makeSchemaNodeTest()
        new NodeTestPattern(schemaNodeTest)
      }
    } else {
      this
    }

  def export(presenter: ExpressionPresenter): Unit = {
    presenter.startElement("p.nodeTest")
    presenter.emitAttribute("test", AlphaCode.fromItemType(nodeTest))
    presenter.endElement()
  }

  def copy(rebindings: RebindingMap): Pattern = {
    val n: NodeTestPattern = new NodeTestPattern(nodeTest.copy())
    n.setPriority(getDefaultPriority)
    ExpressionTool.copyLocationInfo(this, n)
    n
  }

}
