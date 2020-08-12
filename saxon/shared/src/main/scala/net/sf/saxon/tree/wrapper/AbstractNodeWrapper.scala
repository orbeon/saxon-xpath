package net.sf.saxon.tree.wrapper

import net.sf.saxon.lib.NamespaceConstant

import net.sf.saxon.model.Type

import net.sf.saxon.om._

import net.sf.saxon.pattern.AnyNodeTest

import net.sf.saxon.s9api.Location

import net.sf.saxon.trans.XPathException

import net.sf.saxon.tree.NamespaceNode

import net.sf.saxon.tree.iter.AxisIterator

import net.sf.saxon.tree.iter.EmptyIterator

import net.sf.saxon.tree.util.Navigator

import net.sf.saxon.value.StringValue

import net.sf.saxon.value.UntypedAtomicValue

import java.util.function.Predicate

abstract class AbstractNodeWrapper extends NodeInfo with VirtualNode {

   var treeInfo: TreeInfo = _

  def getTreeInfo(): TreeInfo = treeInfo

  def getRealNode(): AnyRef = getUnderlyingNode

  def getNamePool(): NamePool = getConfiguration.getNamePool

  def atomize(): AtomicSequence = getNodeKind match {
    case Type.COMMENT | Type.PROCESSING_INSTRUCTION =>
      new StringValue(getStringValueCS)
    case _ => new UntypedAtomicValue(getStringValueCS)

  }

  override def equals(other: Any): Boolean = {
    if (!(other.isInstanceOf[AbstractNodeWrapper])) {
      false
    }
    val ow: AbstractNodeWrapper = other.asInstanceOf[AbstractNodeWrapper]
    getUnderlyingNode == ow.getUnderlyingNode
  }

  override def hashCode(): Int = getUnderlyingNode.hashCode

  def getSystemId(): String =
    if (treeInfo.isInstanceOf[GenericTreeInfo]) {
      treeInfo.asInstanceOf[GenericTreeInfo].getSystemId
    } else {
      throw new UnsupportedOperationException()
    }

  def setSystemId(uri: String): Unit = {
    if (treeInfo.isInstanceOf[GenericTreeInfo]) {
      treeInfo.asInstanceOf[GenericTreeInfo].setSystemId(uri)
    } else {
      throw new UnsupportedOperationException()
    }
  }

  def getBaseURI(): String = {
    if (getNodeKind == Type.NAMESPACE) {
      null
    }
    var n: NodeInfo = this
    if (getNodeKind != Type.ELEMENT) {
      n = getParent
    }
    while (n != null) {
      val xmlbase: String = n.getAttributeValue(NamespaceConstant.XML, "base")
      if (xmlbase != null) {
        xmlbase
      }
      n = n.getParent
    }
    getRoot.getSystemId
  }

  override def getLineNumber(): Int = -1

  override def getColumnNumber(): Int = -1

  def saveLocation(): Location = this

  def getStringValue(): String = getStringValueCS.toString

  def getDisplayName(): String = {
    val prefix: String = getPrefix
    val local: String = getLocalPart
    if (prefix.isEmpty) {
      local
    } else {
      prefix + ":" + local
    }
  }

  def getAttributeValue(uri: String, local: String): String = null

  override def iterateAxis(axisNumber: Int): AxisIterator =
    iterateAxis(axisNumber, AnyNodeTest.getInstance)

  def iterateAxis(axisNumber: Int,
                  nodeTest: Predicate[_ >: NodeInfo]): AxisIterator = {
    val nodeKind: Int = getNodeKind
    axisNumber match {
      case AxisInfo.ANCESTOR =>
        if (nodeKind == Type.DOCUMENT) {
          EmptyIterator.ofNodes()
        }
        new Navigator.AxisFilter(
          new Navigator.AncestorEnumeration(this, false),
          nodeTest)
      case AxisInfo.ANCESTOR_OR_SELF =>
        if (nodeKind == Type.DOCUMENT) {
          Navigator.filteredSingleton(this, nodeTest)
        }
        new Navigator.AxisFilter(new Navigator.AncestorEnumeration(this, true),
          nodeTest)
      case AxisInfo.ATTRIBUTE =>
        if (nodeKind != Type.ELEMENT) {
          EmptyIterator.ofNodes()
        }
        iterateAttributes(nodeTest)
      case AxisInfo.CHILD =>
        if (nodeKind == Type.ELEMENT || nodeKind == Type.DOCUMENT) {
          iterateChildren(nodeTest)
        } else {
          EmptyIterator.ofNodes()
        }
      case AxisInfo.DESCENDANT =>
        if (nodeKind == Type.ELEMENT || nodeKind == Type.DOCUMENT) {
          iterateDescendants(nodeTest, false)
        } else {
          EmptyIterator.ofNodes()
        }
      case AxisInfo.DESCENDANT_OR_SELF =>
        if (nodeKind == Type.ELEMENT || nodeKind == Type.DOCUMENT) {
          iterateDescendants(nodeTest, true)
        } else {
          Navigator.filteredSingleton(this, nodeTest)
        }
      case AxisInfo.FOLLOWING =>
        new Navigator.AxisFilter(new Navigator.FollowingEnumeration(this),
          nodeTest)
      case AxisInfo.FOLLOWING_SIBLING =>
        nodeKind match {
          case Type.DOCUMENT | Type.ATTRIBUTE | Type.NAMESPACE =>
            EmptyIterator.ofNodes()
          case _ => iterateSiblings(nodeTest, true)

        }
      case AxisInfo.NAMESPACE =>
        if (nodeKind != Type.ELEMENT) {
          EmptyIterator.ofNodes()
        }
        NamespaceNode.makeIterator(this, nodeTest)
      case AxisInfo.PARENT => Navigator.filteredSingleton(getParent, nodeTest)
      case AxisInfo.PRECEDING =>
        new Navigator.AxisFilter(
          new Navigator.PrecedingEnumeration(this, false),
          nodeTest)
      case AxisInfo.PRECEDING_SIBLING =>
        nodeKind match {
          case Type.DOCUMENT | Type.ATTRIBUTE | Type.NAMESPACE =>
            EmptyIterator.ofNodes()
          case _ => iterateSiblings(nodeTest, false)

        }
      case AxisInfo.SELF => Navigator.filteredSingleton(this, nodeTest)
      case AxisInfo.PRECEDING_OR_ANCESTOR =>
        new Navigator.AxisFilter(
          new Navigator.PrecedingEnumeration(this, true),
          nodeTest)
      case _ =>
        throw new IllegalArgumentException("Unknown axis number " + axisNumber)

    }
  }

   def iterateAttributes(
                                   nodeTest: Predicate[_ >: NodeInfo]): AxisIterator

   def iterateChildren(
                                 nodeTest: Predicate[_ >: NodeInfo]): AxisIterator

   def iterateSiblings(nodeTest: Predicate[_ >: NodeInfo],
                                forwards: Boolean): AxisIterator

   def iterateDescendants(nodeTest: Predicate[_ >: NodeInfo],
                                   includeSelf: Boolean): AxisIterator = {
    var iter: AxisIterator =
      new Navigator.DescendantEnumeration(this, includeSelf, true)
    if (!(nodeTest.isInstanceOf[AnyNodeTest])) {
      iter = new Navigator.AxisFilter(iter, nodeTest)
    }
    iter
  }

  def getDeclaredNamespaces(
                             buffer: Array[NamespaceBinding]): Array[NamespaceBinding] =
    Array.ofDim[NamespaceBinding](0)

  override def getAllNamespaces(): NamespaceMap = {
    if (getNodeKind == Type.ELEMENT) {
      throw new AssertionError("not implemented for " + getClass)
    }
    null
  }

  def getRoot(): NodeInfo = {
    var p: NodeInfo = this
    while (true) {
      val q: NodeInfo = p.getParent
      if (q == null) return p
      p = q
    }
    null
  }

  def hasChildNodes(): Boolean = getNodeKind match {
    case Type.DOCUMENT | Type.ELEMENT =>
      iterateAxis(AxisInfo.CHILD).next() != null
    case _ => false

  }

  override def getFingerprint(): Int =
    throw new UnsupportedOperationException()

  override def hasFingerprint(): Boolean = false

}
