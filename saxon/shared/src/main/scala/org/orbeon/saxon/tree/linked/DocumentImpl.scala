package org.orbeon.saxon.tree.linked

import java.{util => ju}

import org.orbeon.saxon.event.{Builder, Receiver}
import org.orbeon.saxon.model.{AnyType, SchemaType, Type, Untyped}
import org.orbeon.saxon.om._
import org.orbeon.saxon.s9api.Location
import org.orbeon.saxon.tree.iter.{AxisIterator, ListIterator}
import org.orbeon.saxon.tree.util.FastStringBuffer
import org.orbeon.saxon.utils.Configuration
import org.orbeon.saxon.value.Whitespace
import org.orbeon.saxon.z.IntHashMap

import scala.beans.{BeanProperty, BooleanBeanProperty}
import scala.jdk.CollectionConverters._


class DocumentImpl
  extends ParentNodeImpl
    with TreeInfo
    with MutableDocumentInfo {

  @BeanProperty
  var documentElement: ElementImpl = _
  private var idTable: ju.HashMap[String, NodeInfo] = _
  @BeanProperty
  var documentNumber: Long = _
  private var baseURI: String = _
  private var entityTable: ju.HashMap[String, Array[String]] = _
  private var nilledElements: ju.Set[ElementImpl] = _
  private var topWithinEntityElements: ju.Set[ElementImpl] = _
  private var elementList: IntHashMap[ju.List[NodeInfo]] = _
  private var userData: ju.HashMap[String, Any] = _
  private var config: Configuration = _
  private var lineNumberMap: LineNumberMap = _
  private var systemIdMap: SystemIdMap = new SystemIdMap()

  @BooleanBeanProperty
  var imaginary: Boolean = _

  var mutable: Boolean = _

  override def isMutable: Boolean = mutable

  def setMutable(mute: Boolean): Unit = this.mutable = mute

  var spaceStrippingRule: SpaceStrippingRule = NoElementsSpaceStrippingRule.getInstance

  this.setRawParent(null)

  def getRootNode: NodeInfo = this

  override def setSpaceStrippingRule(rule: SpaceStrippingRule): Unit = this.spaceStrippingRule = rule

  def getSpaceStrippingRule: SpaceStrippingRule = spaceStrippingRule

  def setConfiguration(config: Configuration): Unit = {
    this.config = config
    documentNumber = config.getDocumentNumberAllocator.allocateDocumentNumber()
  }

  override def getConfiguration: Configuration = config

  override def getNamePool: NamePool = config.getNamePool

  override def newBuilder(): Builder = {
    val builder: LinkedTreeBuilder = new LinkedTreeBuilder(
      config.makePipelineConfiguration)
    builder.setAllocateSequenceNumbers(false)
    builder
  }

  override def isTyped: Boolean =
    documentElement != null &&
      documentElement.getSchemaType != Untyped.getInstance

  def graftLocationMap(original: DocumentImpl): Unit = {
    systemIdMap = original.systemIdMap
    lineNumberMap = original.lineNumberMap
  }

  override def setSystemId(uri: String): Unit = {
    var lUri = uri
    if (lUri == null) {
      lUri = ""
    }
    systemIdMap.setSystemId(getRawSequenceNumber, lUri)
  }

  override def getSystemId: String = systemIdMap.getSystemId(getRawSequenceNumber)

  def setBaseURI(uri: String): Unit = {
    baseURI = uri
  }

  override def getBaseURI: String = {
    if (baseURI != null) {
      return baseURI
    }
    getSystemId
  }

  def setSystemId(seq: Int, uri: String): Unit = {
    var lUri = uri
    if (lUri == null) {
      lUri = ""
    }
    systemIdMap.setSystemId(seq, lUri)
  }

  def getSystemId(seq: Int): String = systemIdMap.getSystemId(seq)

  def setLineNumbering(): Unit = {
    lineNumberMap = new LineNumberMap()
    lineNumberMap.setLineAndColumn(getRawSequenceNumber, 0, -1)
  }

  def setLineAndColumn(sequence: Int, line: Int, column: Int): Unit = {
    if (lineNumberMap != null && sequence >= 0) {
      lineNumberMap.setLineAndColumn(sequence, line, column)
    }
  }

  def getLineNumber(sequence: Int): Int = {
    if (lineNumberMap != null && sequence >= 0) {
      return lineNumberMap.getLineNumber(sequence)
    }
    -1
  }

  def getColumnNumber(sequence: Int): Int = {
    if (lineNumberMap != null && sequence >= 0) {
     return lineNumberMap.getColumnNumber(sequence)
    }
    -1
  }

  def addNilledElement(element: ElementImpl): Unit = {
    if (nilledElements == null)
      nilledElements = new ju.HashSet
    nilledElements.add(element)
  }

  def isNilledElement(element: ElementImpl): Boolean =
    nilledElements != null && nilledElements.contains(element)

  def markTopWithinEntity(element: ElementImpl): Unit = {
    if (topWithinEntityElements == null)
      topWithinEntityElements = new ju.HashSet
    topWithinEntityElements.add(element)
  }

  def isTopWithinEntity(element: ElementImpl): Boolean =
    topWithinEntityElements != null && topWithinEntityElements.contains(
      element)

  override def getLineNumber: Int = 0

  def getNodeKind: Int = Type.DOCUMENT

  override def getNextSibling: NodeImpl = null

  override def getPreviousSibling: NodeImpl = null

  override def getRoot: NodeInfo = this

  override def getPhysicalRoot: DocumentImpl = this

  override def generateId(buffer: FastStringBuffer): Unit = {
    buffer.cat('d')
    buffer.append(java.lang.Long.toString(documentNumber))
  }

  def getAllElements(fingerprint: Int): AxisIterator = {
    if (elementList == null) {
      elementList = new IntHashMap(500)
    }
    val eList: IntHashMap[ju.List[NodeInfo]] = elementList
    var list: ju.List[NodeInfo] = eList.get(fingerprint)
    if (list == null) {
      list = new ju.ArrayList(500)
      var next: NodeImpl = getNextInDocument(this)
      while (next != null) {
        if (next.getNodeKind == Type.ELEMENT && next.getFingerprint == fingerprint) {
          list.add(next)
        }
        next = next.getNextInDocument(this)
      }
      eList.put(fingerprint, list)
    }
    new ListIterator.OfNodes(list)
  }

  def deIndex(node: NodeImpl): Unit = {
    node match {
      case _: ElementImpl =>
        val eList: IntHashMap[ju.List[NodeInfo]] = elementList
        if (eList != null) {
          val list: ju.List[NodeInfo] = eList.get(node.getFingerprint)
          if (list == null)
            return
          list.remove(node)
        }
        if (node.isId)
          deregisterID(node.getStringValue)
      case _: AttributeImpl =>
        if (node.isId)
          deregisterID(node.getStringValue)
      case _ =>
    }
  }

  private def indexIDs(): Unit = {
    if (idTable != null) {
      return
    }
    idTable = new ju.HashMap(256)
    var curr: NodeImpl = this
    val root: NodeImpl = curr
    while (curr != null) {
      if (curr.getNodeKind == Type.ELEMENT) {
        val e: ElementImpl = curr.asInstanceOf[ElementImpl]
        if (e.isId) {
          registerID(e, Whitespace.trim(e.getStringValueCS))
        }
        val atts: AttributeMap = e.attributes
        for (att <- atts.iterator.asScala if att.isId &&
          NameChecker.isValidNCName(Whitespace.trim(att.getValue))) {
          registerID(e, Whitespace.trim(att.getValue))
        }
      }
      curr = curr.getNextInDocument(root)
    }
  }

  def registerID(e: NodeInfo, id: String): Unit = {
    if (idTable == null)
      idTable = new ju.HashMap(256)

    if (idTable.get(id) eq null)
      idTable.put(id, e)
  }

  def selectID(id: String, getParent: Boolean): NodeInfo = {
    if (idTable == null) {
      indexIDs()
    }
    assert(idTable != null)
    var node: NodeInfo = idTable.get(id)
    if (node != null && getParent && node.isId && node.getStringValue == id) {
      node = node.getParent
    }
    node
  }

  def deregisterID(id: String): Unit = {
    var lId = id
    lId = Whitespace.trim(lId)
    if (idTable != null) {
      idTable.remove(lId)
    }
  }

  def setUnparsedEntity(name: String, uri: String, publicId: String): Unit = {
    if (entityTable == null)
      entityTable = new ju.HashMap(10)
    val ids: Array[String] = Array.ofDim[String](2)
    ids(0) = uri
    ids(1) = publicId
    entityTable.put(name, ids)
  }

  def getUnparsedEntityNames: ju.Iterator[String] =
    if (entityTable == null) {
      val ls: ju.List[String] = ju.Collections.emptyList()
      ls.iterator
    } else {
      entityTable.keySet.iterator
    }

  def getUnparsedEntity(name: String): Array[String] = {
    if (entityTable == null) {
      return null
    }
    entityTable.get(name)
  }

  override def getSchemaType: SchemaType =
    if (documentElement == null ||
      documentElement.getSchemaType == Untyped.getInstance) {
      Untyped.getInstance
    } else {
      AnyType.getInstance
    }

  override def copy(out: Receiver, copyOptions: Int, locationId: Location): Unit = {
    out.startDocument(CopyOptions.getStartDocumentProperties(copyOptions))
    val names: ju.Iterator[String] = getUnparsedEntityNames
    while (names.hasNext) {
      val name: String = names.next()
      val details: Array[String] = getUnparsedEntity(name)
      assert(details != null)
      out.setUnparsedEntity(name, details(0), details(1))
    }
    var next: NodeImpl = getFirstChild
    while (next != null) {
      next.copy(out, copyOptions, locationId)
      next = next.getNextSibling
    }
    out.endDocument()
  }

  def replaceStringValue(stringValue: CharSequence): Unit =
    throw new UnsupportedOperationException("Cannot replace the value of a document node")

  def resetIndexes(): Unit = {
    idTable = null
    elementList = null
  }

  override def setUserData(key: String, value: Any): Unit = {
    if (userData == null)
      userData = new ju.HashMap(4)
    if (value == null)
      userData.remove(key)
    else
      userData.put(key, value)
  }

  def getUserData(key: String): Any =
    if (userData == null)
      null
    else
      userData.get(key)

}