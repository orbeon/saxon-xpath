package org.orbeon.saxon.value

import java.util.function.Predicate
import java.util.{Collections, Iterator}

import javax.xml.transform.SourceLocator
import org.orbeon.saxon.event.{Receiver, ReceiverOption}
import org.orbeon.saxon.model.{SchemaType, Type, Untyped}
import org.orbeon.saxon.om._
import org.orbeon.saxon.s9api.Location
import org.orbeon.saxon.tree.iter.{ArrayIterator, AxisIterator, EmptyIterator, SingleNodeIterator}
import org.orbeon.saxon.tree.linked.DocumentImpl
import org.orbeon.saxon.tree.util.{FastStringBuffer, Navigator}
import org.orbeon.saxon.utils.Configuration

import scala.beans.BeanProperty


object TextFragmentValue {

  def makeTextFragment(config: Configuration,
                       value: CharSequence,
                       baseURI: String): NodeInfo =
    if (value.length == 0) {
      val doc: DocumentImpl = new DocumentImpl()
      doc.setSystemId(baseURI)
      doc.setBaseURI(baseURI)
      doc.setConfiguration(config)
      doc
    } else {
      new TextFragmentValue(config, value, baseURI)
    }
}

class TextFragmentValue(config: Configuration,
                        value: CharSequence,
                        @BeanProperty var baseURI: String)
  extends NodeInfo
    with SourceLocator {

  private val text: CharSequence = value
  private var documentURI: String = _

  @BeanProperty
  val treeInfo: GenericTreeInfo = new GenericTreeInfo(config)

  @BeanProperty
  lazy val textNode: TextFragmentTextNode = new TextFragmentTextNode

  this.treeInfo.setRootNode(this)

  def getRootNode: NodeInfo = this
  def isTyped: Boolean = false
  def getNodeKind: Int = Type.DOCUMENT
  def getStringValue: String = text.toString
  def getStringValueCS: CharSequence = text

  override def equals(other: Any): Boolean =
    other match {
      case o: TextFragmentValue if this eq o => true
      case _ => false
    }

  override def hasFingerprint: Boolean = true

  def generateId(buffer: FastStringBuffer): Unit = {
    buffer.append("tt")
    buffer.append(java.lang.Long.toString(treeInfo.getDocumentNumber))
  }

  def setSystemId(systemId: String): Unit =
    documentURI = systemId

  def getSystemId: String = documentURI

  def compareOrder(other: NodeInfo): Int = {
    if (this eq other)
      return 0
    -1
  }

  def getFingerprint: Int = -1
  def getPrefix: String = ""
  def getURI: String = ""
  def getDisplayName: String = ""
  def getLocalPart: String = ""
  def hasChildNodes: Boolean = text.length != 0
  def saveLocation: Location = this

  override def getSchemaType: SchemaType = Untyped.getInstance

  def getDeclaredNamespaces(
                             buffer: Array[NamespaceBinding]): Array[NamespaceBinding] = null

  override def getAllNamespaces: NamespaceMap = null

  def atomize(): AtomicSequence = new UntypedAtomicValue(text)

  def getAttributeValue(uri: String, local: String): String = null

  override def iterateAxis(axisNumber: Int): AxisIterator = axisNumber match {
    case AxisInfo.ANCESTOR | AxisInfo.ATTRIBUTE | AxisInfo.FOLLOWING |
         AxisInfo.FOLLOWING_SIBLING | AxisInfo.NAMESPACE | AxisInfo.PARENT |
         AxisInfo.PRECEDING | AxisInfo.PRECEDING_SIBLING |
         AxisInfo.PRECEDING_OR_ANCESTOR =>
      EmptyIterator.ofNodes
    case AxisInfo.SELF | AxisInfo.ANCESTOR_OR_SELF =>
      SingleNodeIterator.makeIterator(this)
    case AxisInfo.CHILD | AxisInfo.DESCENDANT =>
      SingleNodeIterator.makeIterator(getTextNode)
    case AxisInfo.DESCENDANT_OR_SELF =>
      val nodes: Array[NodeInfo] = Array(this, getTextNode)
      new ArrayIterator.OfNodes(nodes)
    case _ =>
      throw new IllegalArgumentException("Unknown axis number " + axisNumber)

  }

  def iterateAxis(axisNumber: Int, nodeTest: Predicate[_ >: NodeInfo]): AxisIterator =
    axisNumber match {
      case AxisInfo.ANCESTOR | AxisInfo.ATTRIBUTE | AxisInfo.FOLLOWING |
           AxisInfo.FOLLOWING_SIBLING | AxisInfo.NAMESPACE | AxisInfo.PARENT |
           AxisInfo.PRECEDING | AxisInfo.PRECEDING_SIBLING |
           AxisInfo.PRECEDING_OR_ANCESTOR =>
        EmptyIterator.ofNodes
      case AxisInfo.SELF | AxisInfo.ANCESTOR_OR_SELF =>
        Navigator.filteredSingleton(this, nodeTest)
      case AxisInfo.CHILD | AxisInfo.DESCENDANT =>
        Navigator.filteredSingleton(getTextNode, nodeTest)
      case AxisInfo.DESCENDANT_OR_SELF =>
        val b1        = nodeTest.test(this)
        val textNode2 = getTextNode
        val b2        = nodeTest.test(textNode2)
        if (b1) {
          val res = if (b2) {
            val pair = Array(this, textNode2)
            new ArrayIterator.OfNodes(pair)
          } else {
            SingleNodeIterator.makeIterator(this)
          }
          res
        } else {
          if (b2)
            SingleNodeIterator.makeIterator(textNode2)
          else
            EmptyIterator.ofNodes
        }
      case _ =>
        throw new IllegalArgumentException("Unknown axis number " + axisNumber)
    }

  def getParent: NodeInfo = null
  def getRoot: NodeInfo = this

  override def copy(out: Receiver, copyOptions: Int, locationId: Location): Unit =
    out.characters(text, locationId, ReceiverOption.NONE)

  def selectID(id: String, getParent: Boolean): NodeInfo = null

  def getUnparsedEntityNames: Iterator[String] = Collections.emptyIterator[String]

  def getUnparsedEntity(name: String): Array[String] = null

  class TextFragmentTextNode extends NodeInfo with SourceLocator {

    def hasFingerprint: Boolean = true
    def getTreeInfo: TreeInfo = treeInfo
    def setSystemId(systemId: String): Unit = ()
    def getNodeKind: Int = Type.TEXT
    def getStringValue: String = text.toString
    def getStringValueCS: CharSequence = text
    def equals(other: NodeInfo): Boolean = this eq other

    def generateId(buffer: FastStringBuffer): Unit = {
      buffer.append("tt")
      buffer.append(java.lang.Long.toString(treeInfo.getDocumentNumber))
      buffer.append("t1")
    }

    def getSystemId: String = null
    def getBaseURI: String = baseURI

    def compareOrder(other: NodeInfo): Int = {
      if (this eq other)
        return 0
      +1
    }

    def getFingerprint: Int = -1
    def getPrefix: String = ""
    def getURI: String = ""
    def getDisplayName: String = ""
    def getLocalPart: String = ""
    def hasChildNodes: Boolean = false
    def getAttributeValue(uri: String, local: String): String = null
    def saveLocation: Location = this

    override def getSchemaType: SchemaType = null

    def getDeclaredNamespaces(
                               buffer: Array[NamespaceBinding]): Array[NamespaceBinding] = null

    override def getAllNamespaces: NamespaceMap = null

    def atomize(): AtomicSequence = new UntypedAtomicValue(text)

    override def iterateAxis(axisNumber: Int): AxisIterator = axisNumber match {
      case AxisInfo.ANCESTOR | AxisInfo.PARENT | AxisInfo.PRECEDING_OR_ANCESTOR =>
        SingleNodeIterator.makeIterator(TextFragmentValue.this)
      case AxisInfo.ANCESTOR_OR_SELF =>
        val nodes: Array[NodeInfo] = Array(this, TextFragmentValue.this)
        new ArrayIterator.OfNodes(nodes)
      case AxisInfo.ATTRIBUTE | AxisInfo.CHILD | AxisInfo.DESCENDANT |
           AxisInfo.FOLLOWING | AxisInfo.FOLLOWING_SIBLING |
           AxisInfo.NAMESPACE | AxisInfo.PRECEDING |
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
        case AxisInfo.ANCESTOR | AxisInfo.PARENT |
             AxisInfo.PRECEDING_OR_ANCESTOR =>
          Navigator.filteredSingleton(TextFragmentValue.this, nodeTest)
        case AxisInfo.ANCESTOR_OR_SELF =>
          val matchesDoc: Boolean = nodeTest.test(TextFragmentValue.this)
          val matchesText: Boolean = nodeTest.test(this)
          if (matchesDoc && matchesText) {
            val nodes: Array[NodeInfo] = Array(this, TextFragmentValue.this)
            new ArrayIterator.OfNodes(nodes)
          } else if (matchesDoc) {
            SingleNodeIterator.makeIterator(TextFragmentValue.this)
          } else if (matchesText) {
            SingleNodeIterator.makeIterator(this)
          } else {
            EmptyIterator.ofNodes
          }
        case AxisInfo.ATTRIBUTE | AxisInfo.CHILD | AxisInfo.DESCENDANT |
             AxisInfo.FOLLOWING | AxisInfo.FOLLOWING_SIBLING |
             AxisInfo.NAMESPACE | AxisInfo.PRECEDING |
             AxisInfo.PRECEDING_SIBLING =>
          EmptyIterator.ofNodes
        case AxisInfo.SELF | AxisInfo.DESCENDANT_OR_SELF =>
          Navigator.filteredSingleton(this, nodeTest)
        case _ =>
          throw new IllegalArgumentException(
            "Unknown axis number " + axisNumber)

      }

    def getParent: NodeInfo = TextFragmentValue.this
    def getRoot: NodeInfo = TextFragmentValue.this

    override def copy(out: Receiver, copyOptions: Int, locationId: Location): Unit =
      out.characters(text, locationId, ReceiverOption.NONE)
  }
}