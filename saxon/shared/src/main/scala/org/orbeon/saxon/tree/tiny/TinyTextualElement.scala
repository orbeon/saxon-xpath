package org.orbeon.saxon.tree.tiny

import org.orbeon.saxon.event.CopyInformee

import org.orbeon.saxon.event.CopyNamespaceSensitiveException

import org.orbeon.saxon.event.Receiver

import org.orbeon.saxon.event.ReceiverOption

import org.orbeon.saxon.model.SchemaType

import org.orbeon.saxon.model.Type

import org.orbeon.saxon.model.Untyped

import org.orbeon.saxon.om._

import org.orbeon.saxon.s9api.Location

import org.orbeon.saxon.trans.XPathException

import org.orbeon.saxon.tree.iter._

import org.orbeon.saxon.tree.util.FastStringBuffer

import org.orbeon.saxon.tree.util.Navigator

import org.orbeon.saxon.value.UntypedAtomicValue

import org.orbeon.saxon.z.IntValuePredicate

import javax.xml.transform.SourceLocator

import java.util.ArrayList

import java.util.List

import java.util.function.IntPredicate

import java.util.function.Predicate

import scala.beans.{BeanProperty, BooleanBeanProperty}


class TinyTextualElement(tree: TinyTree, nodeNr: Int) extends TinyElementImpl(tree, nodeNr) {

  @BeanProperty
  lazy val textNode: TinyTextualElementText = new TinyTextualElementText()

  override def getDeclaredNamespaces(buffer: Array[NamespaceBinding]): Array[NamespaceBinding] =
    NamespaceBinding.EMPTY_ARRAY

  override def getAllNamespaces: NamespaceMap = {
    val parent: TinyNodeImpl = getParent
    if (parent.isInstanceOf[TinyElementImpl]) {
      parent.getAllNamespaces
    } else {
      NamespaceMap.emptyMap
    }
  }

  override def getAttributeValue(uri: String, local: String): String = null

  override def getAttributeValue(fp: Int): String = null

  override def copy(receiver: Receiver,
                    copyOptions: Int,
                    location: Location): Unit = {
    var lLocation = location
    val typed: Boolean =
      CopyOptions.includes(copyOptions, CopyOptions.TYPE_ANNOTATIONS)
    val `type`: SchemaType = if (typed) getSchemaType else Untyped.getInstance
    val disallowNamespaceSensitiveContent: Boolean = ((copyOptions & CopyOptions.TYPE_ANNOTATIONS) != 0) &&
      ((copyOptions & CopyOptions.ALL_NAMESPACES) == 0)
    if (disallowNamespaceSensitiveContent) {
      try checkNotNamespaceSensitiveElement(`type`, nodeNr)
      catch {
        case e: CopyNamespaceSensitiveException => {
          e.setErrorCode(
            if (receiver.getPipelineConfiguration.isXSLT) "XTTE0950"
            else "XQTY0086")
          throw e
        }

      }
    }
    val informee: CopyInformee[_] = receiver.getPipelineConfiguration
      .getComponent(classOf[CopyInformee[_]].getName)
      .asInstanceOf[CopyInformee[_]]
    if (informee != null) {
      val o: Any = informee.notifyElementNode(this)
      if (o.isInstanceOf[Location]) {
        lLocation = o.asInstanceOf[Location]
      }
    }
    var namespaces: NamespaceMap = null
    namespaces =
      if ((copyOptions & CopyOptions.ALL_NAMESPACES) != 0) getAllNamespaces
      else NamespaceMap.emptyMap
    receiver.startElement(NameOfNode.makeName(this),
      `type`,
      EmptyAttributeMap.getInstance,
      namespaces,
      lLocation,
      ReceiverOption.NONE)
    receiver.characters(getStringValueCS, lLocation, ReceiverOption.NONE)
    receiver.endElement()
  }

  override def hasChildNodes: Boolean = true

  override def getStringValueCS: CharSequence =
    TinyTextImpl.getStringValue(tree, nodeNr)

  override def getStringValue: String =
    TinyTextImpl.getStringValue(tree, nodeNr).toString

  override def iterateAxis(axisNumber: Int): AxisIterator = axisNumber match {
    case AxisInfo.ATTRIBUTE => EmptyIterator.ofNodes
    case AxisInfo.CHILD | AxisInfo.DESCENDANT =>
      SingleNodeIterator.makeIterator(getTextNode)
    case AxisInfo.DESCENDANT_OR_SELF =>
      var list: List[NodeInfo] = new ArrayList[NodeInfo](2)
      list.add(this)
      list.add(getTextNode)
      new ListIterator.OfNodes(list)
    case _ => super.iterateAxis(axisNumber)

  }

  override def iterateAxis(axisNumber: Int,
                           nodeTest: Predicate[_ >: NodeInfo]): AxisIterator =
    axisNumber match {
      case AxisInfo.ATTRIBUTE => EmptyIterator.ofNodes
      case AxisInfo.CHILD | AxisInfo.DESCENDANT =>
        Navigator.filteredSingleton(getTextNode, nodeTest)
      case AxisInfo.DESCENDANT_OR_SELF =>
        var list: List[NodeInfo] = new ArrayList[NodeInfo](2)
        if (nodeTest.test(this)) {
          list.add(this)
        }
        if (nodeTest.test(getTextNode)) {
          list.add(getTextNode)
        }
        new ListIterator.OfNodes(list)
      case _ => super.iterateAxis(axisNumber, nodeTest)

    }

  override def isAncestorOrSelf(d: TinyNodeImpl): Boolean = this == d

  class TinyTextualElementText extends NodeInfo with SourceLocator {

    override def hasFingerprint: Boolean = true

    def getTreeInfo: TreeInfo = TinyTextualElement.this.getTreeInfo

    def setSystemId(systemId: String): Unit = ()

    def getNodeKind: Int = Type.TEXT

    def getStringValue: String = getStringValueCS.toString

    def getStringValueCS: CharSequence =
      TinyTextualElement.this.getStringValueCS

    override def equals(other: Any): Boolean = other match {
      case other: TinyTextualElementText => getParent == other.getParent
      case _ => false

    }

    def generateId(buffer: FastStringBuffer): Unit = {
      TinyTextualElement.this.generateId(buffer)
      buffer.append("T")
    }

    def getSystemId: String = TinyTextualElement.this.getSystemId

    def getBaseURI: String = TinyTextualElement.this.getBaseURI

    def compareOrder(other: NodeInfo): Int =
      if (other == this) {
        0
      } else if (other == getParent) {
        1
      } else {
        getParent.compareOrder(other)
      }

    def getFingerprint: Int = -1

    def getPrefix: String = ""

    def getURI: String = ""

    def getDisplayName: String = ""

    def getLocalPart: String = ""

    def hasChildNodes: Boolean = false

    def getAttributeValue(uri: String, local: String): String = null

    override def getLineNumber: Int = getParent.getLineNumber

    private var isNewline: IntPredicate = new IntValuePredicate(10)

    override def getColumnNumber(): Int = getParent.getColumnNumber

    def saveLocation(): Location = this

    override def getSchemaType: SchemaType = null

    def getDeclaredNamespaces(
                               buffer: Array[NamespaceBinding]): Array[NamespaceBinding] = null

    override def getAllNamespaces: NamespaceMap = null

    def atomize(): AtomicSequence = new UntypedAtomicValue(getStringValueCS)

    override def iterateAxis(axisNumber: Int): AxisIterator = axisNumber match {
      case AxisInfo.ANCESTOR =>
        TinyTextualElement.this.iterateAxis(AxisInfo.ANCESTOR_OR_SELF)
      case AxisInfo.PRECEDING_OR_ANCESTOR =>
        new Navigator.PrecedingEnumeration(this, true)
      case AxisInfo.ANCESTOR_OR_SELF =>
        new PrependAxisIterator(
          this,
          getParent.iterateAxis(AxisInfo.ANCESTOR_OR_SELF))
      case AxisInfo.FOLLOWING => new Navigator.FollowingEnumeration(this)
      case AxisInfo.PRECEDING =>
        new Navigator.PrecedingEnumeration(this, false)
      case AxisInfo.PARENT => SingleNodeIterator.makeIterator(getParent)
      case AxisInfo.ATTRIBUTE | AxisInfo.CHILD | AxisInfo.DESCENDANT |
           AxisInfo.FOLLOWING_SIBLING | AxisInfo.NAMESPACE |
           AxisInfo.PRECEDING_SIBLING =>
        EmptyIterator.ofNodes
      case AxisInfo.SELF | AxisInfo.DESCENDANT_OR_SELF =>
        SingleNodeIterator.makeIterator(this)
      case _ =>
        throw new IllegalArgumentException("Unknown axis number " + axisNumber)

    }

    def iterateAxis(axisNumber: Int,
                    nodeTest: Predicate[_ >: NodeInfo]): AxisIterator =
      axisNumber match {
        case AxisInfo.ANCESTOR =>
          getParent.iterateAxis(AxisInfo.ANCESTOR_OR_SELF, nodeTest)
        case AxisInfo.PRECEDING_OR_ANCESTOR =>
          new Navigator.AxisFilter(
            new Navigator.PrecedingEnumeration(this, true),
            nodeTest)
        case AxisInfo.ANCESTOR_OR_SELF =>
          new Navigator.AxisFilter(
            new PrependAxisIterator(
              this,
              getParent.iterateAxis(AxisInfo.ANCESTOR_OR_SELF)),
            nodeTest)
        case AxisInfo.FOLLOWING =>
          new Navigator.AxisFilter(new Navigator.FollowingEnumeration(this),
            nodeTest)
        case AxisInfo.PRECEDING =>
          new Navigator.AxisFilter(
            new Navigator.PrecedingEnumeration(this, false),
            nodeTest)
        case AxisInfo.PARENT =>
          Navigator.filteredSingleton(getParent, nodeTest)
        case AxisInfo.ATTRIBUTE | AxisInfo.CHILD | AxisInfo.DESCENDANT |
             AxisInfo.FOLLOWING_SIBLING | AxisInfo.NAMESPACE |
             AxisInfo.PRECEDING_SIBLING =>
          EmptyIterator.ofNodes
        case AxisInfo.SELF | AxisInfo.DESCENDANT_OR_SELF =>
          Navigator.filteredSingleton(this, nodeTest)
        case _ =>
          throw new IllegalArgumentException(
            "Unknown axis number " + axisNumber)

      }

    def getParent: NodeInfo = TinyTextualElement.this

    def getRoot: NodeInfo = getParent.getRoot

    override def copy(out: Receiver, copyOptions: Int, locationId: Location): Unit = {
      out.characters(getStringValueCS, locationId, ReceiverOption.NONE)
    }

  }

}