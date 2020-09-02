package net.sf.saxon.tree.wrapper

import java.util.Iterator

import net.sf.saxon.lib.NamespaceConstant
import net.sf.saxon.model.ComplexType
import net.sf.saxon.om._
import net.sf.saxon.pattern.NodeKindTest
import net.sf.saxon.tree.tiny.TinyTree
import net.sf.saxon.tree.wrapper.SpaceStrippedDocument._

import scala.beans.BeanProperty

object SpaceStrippedDocument {

  private def findPreserveSpace(doc: TreeInfo): Boolean =
    doc match {
      case tree: TinyTree =>
        tree.hasXmlSpacePreserveAttribute
      case _ =>
        val iter = doc.getRootNode.iterateAxis(AxisInfo.DESCENDANT, NodeKindTest.ELEMENT)
        var node: NodeInfo = null
        while ( {
          node = iter.next()
          node
        } != null) {
          val value = node.getAttributeValue(NamespaceConstant.XML, "space")
          if ("preserve" == value)
            return true
        }
        false
    }

  private def findAssertions(doc: TreeInfo): Boolean =
    if (doc.isTyped) {
      val iter= doc.getRootNode.iterateAxis(AxisInfo.DESCENDANT, NodeKindTest.ELEMENT)
      while (true) {
        val node: NodeInfo = iter.next()
        if (node == null)
          return false
        val `type` = node.getSchemaType
        if (`type`.isComplexType && `type`.asInstanceOf[ComplexType].hasAssertions)
          return true
      }
      false
    } else
      false
}

class SpaceStrippedDocument(doc: TreeInfo, @BeanProperty var strippingRule: SpaceStrippingRule) extends GenericTreeInfo(doc.getConfiguration) {

  private val preservesSpace: Boolean = findPreserveSpace(doc)
  val containsAssertions: Boolean = findAssertions(doc)
  private val underlyingTree: TreeInfo = doc

  setRootNode(wrap(doc.getRootNode))

  def wrap(node: NodeInfo): SpaceStrippedNode =
    SpaceStrippedNode.makeWrapper(node, this, null)

  override def isTyped: Boolean = underlyingTree.isTyped

  override def selectID(id: String, getParent: Boolean): NodeInfo = {
    val n: NodeInfo = underlyingTree.selectID(id, getParent = false)
    if (n == null) {
      null
    } else {
      wrap(n)
    }
  }

  override def getUnparsedEntityNames: Iterator[String] =
    underlyingTree.getUnparsedEntityNames

  override def getUnparsedEntity(name: String): Array[String] =
    underlyingTree.getUnparsedEntity(name)

  def containsPreserveSpace(): Boolean =
    preservesSpace
}
