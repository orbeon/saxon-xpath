package net.sf.saxon.tree.wrapper

import net.sf.saxon.model.SchemaType

import net.sf.saxon.om._

import net.sf.saxon.s9api.Location

import net.sf.saxon.trans.XPathException

import net.sf.saxon.tree.iter.AxisIterator

import net.sf.saxon.tree.util.FastStringBuffer

import net.sf.saxon.tree.util.Navigator

import java.util.function.Predicate

abstract class AbstractVirtualNode extends VirtualNode {

   var node: NodeInfo = _

   var parent: AbstractVirtualNode = _

   var docWrapper: TreeInfo = _

  def getTreeInfo: TreeInfo = docWrapper

  def getUnderlyingNode: NodeInfo = node

  override def getFingerprint: Int =
    if (node.hasFingerprint) {
      node.getFingerprint
    } else {
      throw new UnsupportedOperationException()
    }

  override def hasFingerprint: Boolean = node.hasFingerprint

  def getRealNode: AnyRef = {
    var u: AnyRef = this
    do u = u.asInstanceOf[VirtualNode].getUnderlyingNode while (u
      .isInstanceOf[VirtualNode]);
    u
  }

  def getNodeKind: Int = node.getNodeKind

  def atomize(): AtomicSequence = node.atomize()

  override def getSchemaType: SchemaType = node.getSchemaType

  override def equals(other: Any): Boolean =
    if (other.isInstanceOf[AbstractVirtualNode]) {
      node == other.asInstanceOf[AbstractVirtualNode].node
    } else {
      node == other
    }

  override def hashCode: Int = node.hashCode ^ 0x3c3c3c3c

  def getSystemId: String = node.getSystemId

  def setSystemId(uri: String): Unit = {
    node.setSystemId(uri)
  }

  def getBaseURI: String = node.getBaseURI

  override def getLineNumber: Int = node.getLineNumber

  override def getColumnNumber(): Int = node.getColumnNumber

  def saveLocation(): Location = this

  def compareOrder(other: NodeInfo): Int =
    if (other.isInstanceOf[AbstractVirtualNode]) {
      node.compareOrder(other.asInstanceOf[AbstractVirtualNode].node)
    } else {
      node.compareOrder(other)
    }

  def getStringValue: String = getStringValueCS.toString

  def getStringValueCS: CharSequence = node.getStringValueCS

  def getLocalPart: String = node.getLocalPart

  def getURI: String = node.getURI

  def getPrefix: String = node.getPrefix

  def getDisplayName: String = node.getDisplayName

  def iterateAxis(axisNumber: Int,
                  nodeTest: Predicate[_ >: NodeInfo]): AxisIterator =
    new Navigator.AxisFilter(iterateAxis(axisNumber), nodeTest)

  def getAttributeValue(uri: String, local: String): String =
    node.getAttributeValue(uri, local)

  def getRoot: NodeInfo = {
    var p: NodeInfo = this
    while (true) {
      val q: NodeInfo = p.getParent
      if (q == null)
        return p
      p = q
    }
    null
  }

  def hasChildNodes: Boolean = node.hasChildNodes

  def generateId(buffer: FastStringBuffer): Unit = {
    node.generateId(buffer)
  }

  def getDeclaredNamespaces(
                             buffer: Array[NamespaceBinding]): Array[NamespaceBinding] =
    node.getDeclaredNamespaces(buffer)

  override def getAllNamespaces: NamespaceMap = node.getAllNamespaces

  override def isId: Boolean = node.isId

  override def isIdref(): Boolean = node.isIdref

  override def isNilled(): Boolean = node.isNilled

}
