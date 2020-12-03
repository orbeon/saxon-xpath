package org.orbeon.saxon.tree.wrapper

import java.util.function.Predicate

import org.orbeon.saxon.lib.NamespaceConstant
import org.orbeon.saxon.model.Type
import org.orbeon.saxon.om._
import org.orbeon.saxon.pattern.AnyNodeTest
import org.orbeon.saxon.s9api.Location
import org.orbeon.saxon.tree.NamespaceNode
import org.orbeon.saxon.tree.iter.{AxisIterator, EmptyIterator}
import org.orbeon.saxon.tree.util.Navigator
import org.orbeon.saxon.value.{StringValue, UntypedAtomicValue}


/**
 * A node in the XML parse tree representing an XML element, character content, or attribute.
 * <p>This implementation of the NodeInfo interface contains common code used by many "wrapper" implementations
 * for external data models.</p>
 *
 * @author Michael H. Kay
 */
trait AbstractNodeWrapper extends NodeInfo with VirtualNode { // ORBEON: Made this a `trait`

  var treeInfo: TreeInfo = _

  def getTreeInfo: TreeInfo = treeInfo
  def getRealNode: AnyRef = getUnderlyingNode
  def getNamePool: NamePool = getConfiguration.getNamePool

  def atomize(): AtomicSequence = getNodeKind match {
    case Type.COMMENT | Type.PROCESSING_INSTRUCTION =>
      new StringValue(getStringValueCS)
    case _ =>
      new UntypedAtomicValue(getStringValueCS)
  }

  override def equals(other: Any): Boolean =
    other match {
      case ow: AbstractNodeWrapper if getUnderlyingNode == ow.getUnderlyingNode => true
      case _ => false
    }

  override def hashCode: Int = getUnderlyingNode.hashCode

  def getSystemId: String =
    treeInfo match {
      case info: GenericTreeInfo =>
        info.getSystemId
      case _ =>
        throw new UnsupportedOperationException()
    }

  def setSystemId(uri: String): Unit =
    treeInfo match {
      case info: GenericTreeInfo =>
        info.setSystemId(uri)
      case _ =>
        throw new UnsupportedOperationException()
    }

  def getBaseURI: String = {
    if (getNodeKind == Type.NAMESPACE)
      return null
    var n: NodeInfo = this
    if (getNodeKind != Type.ELEMENT)
      n = getParent
    while (n != null) {
      val xmlbase = n.getAttributeValue(NamespaceConstant.XML, "base")
      if (xmlbase != null)
        return xmlbase
      n = n.getParent
    }
    getRoot.getSystemId
  }

  // ORBEON: Overrides `NodeInfo` which also returns `-1`.
  override def getLineNumber: Int = -1
  override def getColumnNumber: Int = -1

  def saveLocation: Location = this

  def getStringValue: String = getStringValueCS.toString

  def getDisplayName: String = {
    val prefix = getPrefix
    val local = getLocalPart
    if (prefix.isEmpty)
      local
    else
      prefix + ":" + local
  }

  def getAttributeValue(uri: String, local: String): String = null

  override def iterateAxis(axisNumber: Int): AxisIterator =
    iterateAxis(axisNumber, AnyNodeTest)

  def iterateAxis(axisNumber: Int, nodeTest: Predicate[_ >: NodeInfo]): AxisIterator = {
    val nodeKind = getNodeKind
    axisNumber match {
      case AxisInfo.ANCESTOR =>
        if (nodeKind == Type.DOCUMENT)
          EmptyIterator.ofNodes
        else
          new Navigator.AxisFilter(new Navigator.AncestorEnumeration(this, false), nodeTest)
      case AxisInfo.ANCESTOR_OR_SELF =>
        if (nodeKind == Type.DOCUMENT)
          Navigator.filteredSingleton(this, nodeTest)
        else
          new Navigator.AxisFilter(new Navigator.AncestorEnumeration(this, true), nodeTest)
      case AxisInfo.ATTRIBUTE =>
        if (nodeKind != Type.ELEMENT)
          EmptyIterator.ofNodes
        else
          iterateAttributes(nodeTest)
      case AxisInfo.CHILD =>
        if (nodeKind == Type.ELEMENT || nodeKind == Type.DOCUMENT)
          iterateChildren(nodeTest)
        else
          EmptyIterator.ofNodes
      case AxisInfo.DESCENDANT =>
        if (nodeKind == Type.ELEMENT || nodeKind == Type.DOCUMENT)
          iterateDescendants(nodeTest, includeSelf = false)
        else
          EmptyIterator.ofNodes
      case AxisInfo.DESCENDANT_OR_SELF =>
        if (nodeKind == Type.ELEMENT || nodeKind == Type.DOCUMENT)
          iterateDescendants(nodeTest, includeSelf = true)
        else
          Navigator.filteredSingleton(this, nodeTest)
      case AxisInfo.FOLLOWING =>
        new Navigator.AxisFilter(new Navigator.FollowingEnumeration(this), nodeTest)
      case AxisInfo.FOLLOWING_SIBLING =>
        nodeKind match {
          case Type.DOCUMENT | Type.ATTRIBUTE | Type.NAMESPACE =>
            EmptyIterator.ofNodes
          case _ =>
            iterateSiblings(nodeTest, forwards = true)
        }
      case AxisInfo.NAMESPACE =>
        if (nodeKind != Type.ELEMENT)
          EmptyIterator.ofNodes
        else
          NamespaceNode.makeIterator(this, nodeTest)
      case AxisInfo.PARENT =>
        Navigator.filteredSingleton(getParent, nodeTest)
      case AxisInfo.PRECEDING =>
        new Navigator.AxisFilter(new Navigator.PrecedingEnumeration(this, false), nodeTest)
      case AxisInfo.PRECEDING_SIBLING =>
        nodeKind match {
          case Type.DOCUMENT | Type.ATTRIBUTE | Type.NAMESPACE =>
            EmptyIterator.ofNodes
          case _ =>
            iterateSiblings(nodeTest, forwards = false)
        }
      case AxisInfo.SELF =>
        Navigator.filteredSingleton(this, nodeTest)
      case AxisInfo.PRECEDING_OR_ANCESTOR =>
        new Navigator.AxisFilter(new Navigator.PrecedingEnumeration(this, true), nodeTest)
      case _ =>
        throw new IllegalArgumentException("Unknown axis number " + axisNumber)
    }
  }

  def iterateAttributes(nodeTest: Predicate[_ >: NodeInfo]): AxisIterator
  def iterateChildren(nodeTest: Predicate[_ >: NodeInfo]): AxisIterator
  def iterateSiblings(nodeTest: Predicate[_ >: NodeInfo], forwards: Boolean): AxisIterator

  def iterateDescendants(nodeTest: Predicate[_ >: NodeInfo], includeSelf: Boolean): AxisIterator = {
    var iter: AxisIterator = new Navigator.DescendantEnumeration(this, includeSelf, true)
    if (! nodeTest.isInstanceOf[AnyNodeTest.type])
      iter = new Navigator.AxisFilter(iter, nodeTest)
    iter
  }

  def getDeclaredNamespaces(buffer: Array[NamespaceBinding]): Array[NamespaceBinding] =
    Array.ofDim[NamespaceBinding](0)

  override def getAllNamespaces: NamespaceMap =
    if (getNodeKind == Type.ELEMENT)
      throw new AssertionError("not implemented for " + getClass)
    else
      null

  def getRoot: NodeInfo = {
    var p: NodeInfo = this
    while (true) {
      val q = p.getParent
      if (q == null)
        return p
      p = q
    }
    null
  }

  def hasChildNodes: Boolean = getNodeKind match {
    case Type.DOCUMENT | Type.ELEMENT => iterateAxis(AxisInfo.CHILD).next() != null
    case _ => false
  }

  def getFingerprint =
    throw new UnsupportedOperationException()

  def hasFingerprint = false
}
