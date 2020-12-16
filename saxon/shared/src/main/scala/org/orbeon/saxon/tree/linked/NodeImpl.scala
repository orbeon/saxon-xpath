package org.orbeon.saxon.tree.linked

import org.orbeon.saxon.utils.Configuration

import org.orbeon.saxon.event.Builder

import org.orbeon.saxon.model._

import org.orbeon.saxon.om._

import org.orbeon.saxon.pattern.AnyNodeTest

import org.orbeon.saxon.pattern.NameTest

import org.orbeon.saxon.s9api.Location

import org.orbeon.saxon.trans.XPathException

import org.orbeon.saxon.tree.NamespaceNode

import org.orbeon.saxon.tree.iter.AxisIterator

import org.orbeon.saxon.tree.iter.EmptyIterator

import org.orbeon.saxon.tree.util.FastStringBuffer

import org.orbeon.saxon.tree.util.Navigator

import org.orbeon.saxon.tree.util.SteppingNavigator

import org.orbeon.saxon.tree.util.SteppingNode

import org.orbeon.saxon.tree.wrapper.SiblingCountingNode

import org.orbeon.saxon.value.UntypedAtomicValue

import java.util.function.Predicate

import NodeImpl._

object NodeImpl {

  val NODE_LETTER: Array[Char] =
    Array('x', 'e', 'a', 't', 'x', 'x', 'x', 'p', 'c', 'r', 'x', 'x', 'x', 'n')

}

abstract class NodeImpl
  extends MutableNodeInfo
    with SteppingNode[NodeImpl]
    with SiblingCountingNode
    with Location {

  private var parent: ParentNodeImpl = _

  private var index: Int = _

  override def head: NodeImpl = this

  def getTreeInfo: TreeInfo = getPhysicalRoot

  def getStringValueCS: CharSequence = getStringValue

  override def getSchemaType: SchemaType = Untyped.getInstance

  override def getColumnNumber(): Int =
    if (parent == null) {
      -1
    } else {
      parent.getColumnNumber
    }

  def getSiblingPosition: Int = index

  def setSiblingPosition(index: Int): Unit = {
    this.index = index
  }

  def atomize(): AtomicSequence = {
    val stype: SchemaType = getSchemaType
    if (stype == Untyped.getInstance || stype == BuiltInAtomicType.UNTYPED_ATOMIC) {
      new UntypedAtomicValue(getStringValueCS)
    } else {
      stype.atomize(this)
    }
  }

  def setSystemId(uri: String): Unit = {
    val p: NodeInfo = getParent
    if (p != null) {
      p.setSystemId(uri)
    }
  }

  def equals(other: NodeInfo): Boolean = this eq other

  def getNodeName: NodeName = null

  override def hasFingerprint: Boolean = true

  def getFingerprint: Int = {
    val name: NodeName = getNodeName
    if (name == null) {
      -1
    } else {
      name.obtainFingerprint(getConfiguration.getNamePool)
    }
  }

  override def attributes: AttributeMap = EmptyAttributeMap.getInstance

  def generateId(buffer: FastStringBuffer): Unit = {
    val seq: Long = getSequenceNumber
    if (seq == -1L) {
      getPhysicalRoot.generateId(buffer)
      buffer.cat(NODE_LETTER(getNodeKind))
      buffer.append(java.lang.Long.toString(seq) + "h" + hashCode)
    } else {
      parent.generateId(buffer)
      buffer.cat(NODE_LETTER(getNodeKind))
      buffer.append(java.lang.Integer.toString(index))
    }
  }

  def getSystemId: String = parent.getSystemId

  def getBaseURI: String = parent.getBaseURI

  def getSequenceNumber: Long = {
    var prev: NodeImpl = this
    var i: Int = 0
    while (true) {
      if (prev.isInstanceOf[ParentNodeImpl]) {
        val prevseq: Long = prev.getSequenceNumber
        return if (prevseq == -1L) prevseq else prevseq + 0x10000 + i
      }
      assert(prev != null)
      prev = prev.getPreviousInDocument
      i += 1
    }
    0
  }

  def compareOrder(other: NodeInfo): Int = {
    if (other.isInstanceOf[NamespaceNode]) {
     return 0 - other.compareOrder(this)
    }
    val a: Long = getSequenceNumber
    val b: Long = other.asInstanceOf[NodeImpl].getSequenceNumber
    if (a == -1L || b == -1L) {
     return Navigator.compareOrder(this, other.asInstanceOf[NodeImpl])
    }
    java.lang.Long.compare(a, b)
  }

  override def getConfiguration: Configuration = getPhysicalRoot.getConfiguration

  def getNamePool: NamePool = getPhysicalRoot.getNamePool

  def getPrefix: String = {
    val qName: NodeName = getNodeName
    if (qName == null) "" else qName.getPrefix
  }

  def getURI: String = {
    val qName: NodeName = getNodeName
    if (qName == null) "" else qName.getURI
  }

  def getDisplayName: String = {
    val qName: NodeName = getNodeName
    if (qName == null) "" else qName.getDisplayName
  }

  def getLocalPart: String = {
    val qName: NodeName = getNodeName
    if (qName == null) "" else qName.getLocalPart
  }

  override def getLineNumber: Int = parent.getLineNumber

  def saveLocation(): Location = this

  def getParent: NodeImpl = {
    if (parent.isInstanceOf[DocumentImpl] && parent
      .asInstanceOf[DocumentImpl]
      .isImaginary) {
      return null
    }
    parent
  }

  def getRawParent: ParentNodeImpl = parent

  def setRawParent(parent: ParentNodeImpl): Unit = {
    this.parent = parent
  }

  def getPreviousSibling: NodeImpl = {
    if (parent == null) {
      return null
    }
    parent.getNthChild(index - 1)
  }

  def getNextSibling: NodeImpl = {
    if (parent == null) {
      return null
    }
    parent.getNthChild(index + 1)
  }

  def getFirstChild: NodeImpl = null

  def getLastChild: NodeInfo = null

  override def iterateAxis(axisNumber: Int): AxisIterator =
    if (axisNumber == AxisInfo.CHILD) {
      if (this.isInstanceOf[ParentNodeImpl]) {
        this.asInstanceOf[ParentNodeImpl].iterateChildren(null)
      } else {
        EmptyIterator.ofNodes
      }
    } else {
      iterateAxis(axisNumber, AnyNodeTest)
    }

  def iterateAxis(axisNumber: Int,
                  nodeTest: Predicate[_ >: NodeInfo]): AxisIterator =
    axisNumber match {
      case AxisInfo.ANCESTOR => new AncestorEnumeration(this, nodeTest, false)
      case AxisInfo.ANCESTOR_OR_SELF =>
        new AncestorEnumeration(this, nodeTest, true)
      case AxisInfo.ATTRIBUTE =>
        if (getNodeKind != Type.ELEMENT) {
          EmptyIterator.ofNodes
        } else {
          this.asInstanceOf[ElementImpl].iterateAttributes(nodeTest)
        }
      case AxisInfo.CHILD =>
        if (this.isInstanceOf[ParentNodeImpl]) {
          this.asInstanceOf[ParentNodeImpl].iterateChildren(nodeTest)
        } else {
          EmptyIterator.ofNodes
        }
      case AxisInfo.DESCENDANT =>
        if (getNodeKind == Type.DOCUMENT && nodeTest.isInstanceOf[NameTest] &&
          nodeTest.asInstanceOf[NameTest].getPrimitiveType == Type.ELEMENT) {
          this
            .asInstanceOf[DocumentImpl]
            .getAllElements(nodeTest.asInstanceOf[NameTest].getFingerprint)
        } else if (hasChildNodes) {
          new SteppingNavigator.DescendantAxisIterator(this, false, nodeTest)
        } else {
          EmptyIterator.ofNodes
        }
      case AxisInfo.DESCENDANT_OR_SELF =>
        new SteppingNavigator.DescendantAxisIterator(this, true, nodeTest)
      case AxisInfo.FOLLOWING => new FollowingEnumeration(this, nodeTest)
      case AxisInfo.FOLLOWING_SIBLING =>
        new FollowingSiblingEnumeration(this, nodeTest)
      case AxisInfo.NAMESPACE =>
        if (getNodeKind != Type.ELEMENT) {
          EmptyIterator.ofNodes
        }
        NamespaceNode.makeIterator(this, nodeTest)
      case AxisInfo.PARENT =>
        var parent: NodeInfo = getParent
        if (parent == null) {
          return EmptyIterator.ofNodes
        }
        Navigator.filteredSingleton(parent, nodeTest)
      case AxisInfo.PRECEDING => new PrecedingEnumeration(this, nodeTest)
      case AxisInfo.PRECEDING_SIBLING =>
        new PrecedingSiblingEnumeration(this, nodeTest)
      case AxisInfo.SELF => Navigator.filteredSingleton(this, nodeTest)
      case AxisInfo.PRECEDING_OR_ANCESTOR =>
        new PrecedingOrAncestorEnumeration(this, nodeTest)
      case _ =>
        throw new IllegalArgumentException("Unknown axis number " + axisNumber)

    }

  def getAttributeValue(uri: String, localName: String): String = null

  def getRoot: NodeInfo = {
    val parent: NodeInfo = getParent
    if (parent == null) {
      this
    } else {
      parent.getRoot
    }
  }

  def getPhysicalRoot: DocumentImpl = {
    var up: ParentNodeImpl = parent
    while (up != null && !(up.isInstanceOf[DocumentImpl])) up = up.getRawParent
    up.asInstanceOf[DocumentImpl]
  }

  def getNextInDocument(anchor: NodeImpl): NodeImpl = {
    var next: NodeImpl = getFirstChild
    if (next != null) {
      return next
    }
    if (this == anchor) {
      return null
    }
    next = getNextSibling
    if (next != null) {
      return next
    }
    var parent: NodeImpl = this
    while (true) {
      parent = parent.getParent
      if (parent == null) {
        return null
      }
      if (parent == anchor) {
        return null
      }
      next = parent.getNextSibling
      if (next != null) {
        return next
      }
    }
    null
  }

  def getSuccessorElement(anchor: NodeImpl,
                          uri: String,
                          local: String): NodeImpl = {
    var next: NodeImpl = getNextInDocument(anchor)
    while (next != null &&
      !(next.getNodeKind == Type.ELEMENT && (uri == null || uri == next.getURI) &&
        (local == null || local == next.getLocalPart))) next =
      next.getNextInDocument(anchor)
    next
  }

  def getPreviousInDocument: NodeImpl = {
    val prev: NodeImpl = getPreviousSibling
    if (prev != null) {
      prev.getLastDescendantOrSelf
    }
    getParent
  }

  private def getLastDescendantOrSelf: NodeImpl = {
    val last: NodeImpl = getLastChild.asInstanceOf[NodeImpl]
    if (last == null) {
      return this
    }
    last.getLastDescendantOrSelf
  }

  def getDeclaredNamespaces(
                             buffer: Array[NamespaceBinding]): Array[NamespaceBinding] = null

  override def getAllNamespaces: NamespaceMap = null

  def hasChildNodes: Boolean = getFirstChild != null

  def setTypeAnnotation(`type`: SchemaType): Unit = ()

  def delete(): Unit = {
    if (parent != null) {
      parent.removeChild(this)
      val newRoot: DocumentImpl = new DocumentImpl()
      newRoot.setConfiguration(getConfiguration)
      newRoot.setImaginary(true)
      parent = newRoot
    }
    index = -1
  }

  def isDeleted: Boolean =
    index == -1 || (parent != null && parent.isDeleted)

  override def setAttributes(attributes: AttributeMap): Unit = {
    throw new UnsupportedOperationException(
      "setAttributes() applies only to element nodes")
  }

  def removeAttribute(attribute: NodeInfo): Unit = ()

  def addAttribute(name: NodeName,
                   attType: SimpleType,
                   value: CharSequence,
                   properties: Int): Unit = ()

  def rename(newNameCode: NodeName): Unit = ()

  def addNamespace(nscode: NamespaceBinding): Unit = ()

  def replace(replacement: Array[NodeInfo], inherit: Boolean): Unit = {
    if (isDeleted) {
      throw new IllegalStateException("Cannot replace a deleted node")
    }
    if (getParent == null) {
      throw new IllegalStateException("Cannot replace a parentless node")
    }
    assert(parent != null)
    parent.replaceChildrenAt(replacement, index, inherit)
    parent = null
    index = -1
  }

  def insertChildren(source: Array[NodeInfo],
                     atStart: Boolean,
                     inherit: Boolean): Unit = ()

  def insertSiblings(source: Array[NodeInfo],
                     before: Boolean,
                     inherit: Boolean): Unit = {
    if (parent == null) {
      throw new IllegalStateException(
        "Cannot add siblings if there is no parent")
    }
    parent.insertChildrenAt(source, if (before) index else index + 1, inherit)
  }

  def removeTypeAnnotation(): Unit = ()

  def newBuilder(): Builder = getPhysicalRoot.newBuilder()

  override def effectiveBooleanValue: Boolean = true

}