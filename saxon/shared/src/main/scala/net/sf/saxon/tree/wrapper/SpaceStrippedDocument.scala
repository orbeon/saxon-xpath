package net.sf.saxon.tree.wrapper

import net.sf.saxon.lib.NamespaceConstant

import net.sf.saxon.model.ComplexType

import net.sf.saxon.model.SchemaType

import net.sf.saxon.om._

import net.sf.saxon.pattern.NodeKindTest

import net.sf.saxon.tree.iter.AxisIterator

import net.sf.saxon.tree.tiny.TinyTree

import java.util.Iterator

import SpaceStrippedDocument._

import scala.beans.{BeanProperty, BooleanBeanProperty}

object SpaceStrippedDocument {

  private def findPreserveSpace(doc: TreeInfo): Boolean =
    if (doc.isInstanceOf[TinyTree]) {
      doc.asInstanceOf[TinyTree].hasXmlSpacePreserveAttribute()
    } else {
      val iter: AxisIterator =
        doc.getRootNode.iterateAxis(AxisInfo.DESCENDANT, NodeKindTest.ELEMENT)
      var node: NodeInfo = null
      while (({
        node = iter.next()
        node
      }) != null) {
        val `val`: String =
          node.getAttributeValue(NamespaceConstant.XML, "space")
        if ("preserve" == `val`) {
          true
        }
      }
      false
    }

  private def findAssertions(doc: TreeInfo): Boolean =
    if (doc.isTyped) {
      val iter: AxisIterator =
        doc.getRootNode.iterateAxis(AxisInfo.DESCENDANT, NodeKindTest.ELEMENT)
      while (true) {
        val node: NodeInfo = iter.next()
        if (node == null) return false
        val `type`: SchemaType = node.getSchemaType
        if (`type`.isComplexType && `type`
          .asInstanceOf[ComplexType]
          .hasAssertions())
          true
      }
      false
    } else false

}

class SpaceStrippedDocument(doc: TreeInfo, @BeanProperty var strippingRule: SpaceStrippingRule) extends GenericTreeInfo(doc.getConfiguration) {

  private var preservesSpace: Boolean = findPreserveSpace(doc)

  var containsAssertions: Boolean = findAssertions(doc)

  private var underlyingTree: TreeInfo = doc

  setRootNode(wrap(doc.getRootNode))

  def wrap(node: NodeInfo): SpaceStrippedNode =
    SpaceStrippedNode.makeWrapper(node, this, null)

  override def isTyped(): Boolean = underlyingTree.isTyped

  override def selectID(id: String, getParent: Boolean): NodeInfo = {
    val n: NodeInfo = underlyingTree.selectID(id, false)
    if (n == null) {
      null
    } else {
      wrap(n)
    }
  }

  override def getUnparsedEntityNames(): Iterator[String] =
    underlyingTree.getUnparsedEntityNames

  override def getUnparsedEntity(name: String): Array[String] =
    underlyingTree.getUnparsedEntity(name)

  def containsPreserveSpace(): Boolean = preservesSpace

}
