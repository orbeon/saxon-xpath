package org.orbeon.saxon.tree.tiny

import java.util.{ArrayList, List}

import org.orbeon.saxon.event.Receiver
import org.orbeon.saxon.model.{AnyType, SchemaType, Type, Untyped}
import org.orbeon.saxon.om.{AtomicSequence, AxisInfo, CopyOptions, NodeInfo}
import org.orbeon.saxon.pattern.NodeKindTest
import org.orbeon.saxon.s9api.Location
import org.orbeon.saxon.tree.iter.AxisIterator
import org.orbeon.saxon.tree.util.FastStringBuffer
import org.orbeon.saxon.utils.Configuration
import org.orbeon.saxon.value.UntypedAtomicValue
import org.orbeon.saxon.z.IntHashMap

//import scala.collection.compat._
import scala.jdk.CollectionConverters._

class TinyDocumentImpl(treeImpl: TinyTree) extends TinyParentNodeImpl {

  private var elementList: IntHashMap[List[NodeInfo]] = _
  private var baseURI: String = _

  this.tree = treeImpl

  nodeNr = 0

  override def getTree: TinyTree = tree

  def getRootNode: NodeInfo = this

  override def getConfiguration: Configuration = tree.getConfiguration

  override def setSystemId(uri: String): Unit = {
    tree.setSystemId(nodeNr, uri)
  }

  override def getSystemId: String = tree.getSystemId(nodeNr)

  def setBaseURI(uri: String): Unit =
    baseURI = uri

  override def getBaseURI: String = {
    if (baseURI != null) {
      return baseURI
    }
    getSystemId
  }

  override def getLineNumber: Int = 0

  def isTyped: Boolean = tree.getTypeArray != null

  def getNodeKind: Int = Type.DOCUMENT

  override def getParent: TinyNodeImpl = null

  override def getRoot: NodeInfo = this

  override def generateId(buffer: FastStringBuffer): Unit = {
    buffer.cat('d')
    buffer.append(java.lang.Long.toString(getTreeInfo.getDocumentNumber))
  }

  def atomize(): AtomicSequence = new UntypedAtomicValue(getStringValueCS)

  def getAllElements(fingerprint: Int): AxisIterator = {
    if (elementList == null)
      elementList = new IntHashMap[List[NodeInfo]](20)
    var list = elementList.get(fingerprint)
    if (list == null) {
      list = makeElementList(fingerprint)
      elementList.put(fingerprint, list)
    }
    new org.orbeon.saxon.tree.iter.ListIterator.OfNodes(list)
  }

  def makeElementList(fingerprint: Int): List[NodeInfo] = {
    var size: Int = tree.getNumberOfNodes / 20
    if (size > 100) {
      size = 100
    }
    if (size < 20) {
      size = 20
    }
    val list: ArrayList[NodeInfo] = new ArrayList[NodeInfo](size)
    var i: Int = nodeNr + 1
    try while (tree.depth(i) != 0) {
      val kind: Byte = tree.nodeKind(i)
      if ((kind & 0x0f) == Type.ELEMENT && (tree.nameCode(i) & 0xfffff) == fingerprint) {
        list.add(tree.getNode(i))
      }
      { i += 1; i - 1 }
    } catch {
      case e: ArrayIndexOutOfBoundsException => return list

    }
    list.trimToSize()
    list
  }

  override def getSchemaType: SchemaType = {
    val children = iterateAxis(AxisInfo.CHILD, NodeKindTest.ELEMENT)
    val node     = children.next()
    if (node == null || node.getSchemaType == Untyped.getInstance)
      Untyped.getInstance
    else
      AnyType.getInstance
  }

  override def copy(out: Receiver, copyOptions: Int, locationId: Location): Unit = {
    out.startDocument(CopyOptions.getStartDocumentProperties(copyOptions))
    if (tree.entityTable != null) {
      for ((key, value) <- tree.entityTable.asScala) {
        val name: String = key
        val details: Array[String] = value
        val systemId: String = details(0)
        val publicId: String = details(1)
        out.setUnparsedEntity(name, systemId, publicId)
      }
    }
    for (child <- children)
      child.copy(out, copyOptions, locationId)
    out.endDocument()
  }

  def showSize(): Unit =
    tree.showSize()

  override def hashCode: Int = tree.getDocumentNumber.toInt
}