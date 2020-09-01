package net.sf.saxon.tree.linked

import net.sf.saxon.event._

import net.sf.saxon.expr.parser.Loc

import net.sf.saxon.lib.NamespaceConstant

import net.sf.saxon.model.SchemaType

import net.sf.saxon.om._

import net.sf.saxon.s9api.Location

import net.sf.saxon.trans.XPathException

import net.sf.saxon.value.Whitespace

import java.util.ArrayList

import java.util.Arrays

import java.util.Stack

import LinkedTreeBuilder._


object LinkedTreeBuilder {

  object DefaultNodeFactory {

    var THE_INSTANCE: DefaultNodeFactory = new DefaultNodeFactory()

  }

  class DefaultNodeFactory extends NodeFactory {

    def makeElementNode(parent: NodeInfo,
                        nodeName: NodeName,
                        elementType: SchemaType,
                        isNilled: Boolean,
                        attlist: AttributeMap,
                        namespaces: NamespaceMap,
                        pipe: PipelineConfiguration,
                        locationId: Location,
                        sequenceNumber: Int): ElementImpl = {
      val e: ElementImpl = new ElementImpl()
      e.setNamespaceMap(namespaces)
      e.initialise(nodeName, elementType, attlist, parent, sequenceNumber)
      if (isNilled) {
        e.setNilled()
      }
      if (locationId != Loc.NONE && sequenceNumber >= 0) {
        val baseURI: String = locationId.getSystemId
        val lineNumber: Int = locationId.getLineNumber
        val columnNumber: Int = locationId.getColumnNumber
        e.setLocation(baseURI, lineNumber, columnNumber)
      }
      e
    }

    def makeTextNode(parent: NodeInfo, content: CharSequence): TextImpl =
      new TextImpl(content.toString)

  }

}

class LinkedTreeBuilder(pipe: PipelineConfiguration) extends Builder(pipe) {

  private var currentNode: ParentNodeImpl = _

  private var nodeFactory: NodeFactory = DefaultNodeFactory.THE_INSTANCE

  private var size: Array[Int] = new Array[Int](100)

  private var depth: Int = 0

  private var arrays: ArrayList[Array[NodeImpl]] = new ArrayList(20)

  private var namespaceStack: Stack[NamespaceMap] = new Stack()

  private var allocateSequenceNumbers: Boolean = true

  private var nextNodeNumber: Int = 1

  private var mutable: Boolean = _

  def this(pipe: PipelineConfiguration, mutable: Boolean) = {
    this(???) /* TODO: Scala does not allow multiple super constructor calls
     * Change this code to call a constructor of the current class instead.
     * For your convenience, here is the invalid super constructor call:
     * }super(pipe)
     */

    this.mutable = mutable
    nodeFactory = DefaultNodeFactory.THE_INSTANCE
  }

  override def getCurrentRoot(): NodeInfo = {
    val physicalRoot: NodeInfo = currentRoot
    if (physicalRoot.isInstanceOf[DocumentImpl] &&
      physicalRoot.asInstanceOf[DocumentImpl].isImaginary) {
      physicalRoot.asInstanceOf[DocumentImpl].getDocumentElement
    } else {
      physicalRoot
    }
  }

  override def reset(): Unit = {
    super.reset()
    currentNode = null
    nodeFactory = DefaultNodeFactory.THE_INSTANCE
    depth = 0
    allocateSequenceNumbers = true
    nextNodeNumber = 1
  }

  def setAllocateSequenceNumbers(allocate: Boolean): Unit = {
    allocateSequenceNumbers = allocate
  }

  def setNodeFactory(factory: NodeFactory): Unit = {
    nodeFactory = factory
  }

  override def open(): Unit = {
    started = true
    depth = 0
    size(depth) = 0
    if (arrays == null) {
      arrays = new ArrayList[Array[NodeImpl]](20)
    }
    super.open()
  }

  def startDocument(properties: Int): Unit = {
    val doc: DocumentImpl = new DocumentImpl()
    doc.setMutable(mutable)
    currentRoot = doc
    doc.setSystemId(getSystemId)
    doc.setBaseURI(getBaseURI)
    doc.setConfiguration(config)
    currentNode = doc
    depth = 0
    size(depth) = 0
    if (arrays == null) {
      arrays = new ArrayList(20)
    }
    doc.setRawSequenceNumber(0)
    if (lineNumbering) {
      doc.setLineNumbering()
    }
  }

  def endDocument(): Unit = {
    currentNode.compact(size(depth))
  }

  override def close(): Unit = {
    if (currentNode == null) {
      return
    }
    currentNode.compact(size(depth))
    currentNode = null
    arrays = null
    super.close()
    nodeFactory = DefaultNodeFactory.THE_INSTANCE
  }

  def startElement(elemName: NodeName,
                   `type`: SchemaType,
                   suppliedAttributes: AttributeMap,
                   namespaces: NamespaceMap,
                   location: Location,
                   properties: Int): Unit = {
    var lSuppliedAttributes = suppliedAttributes
    if (currentNode == null) {
      startDocument(ReceiverOption.NONE)
      currentRoot.asInstanceOf[DocumentImpl].setImaginary(true)
    }
    val isNilled: Boolean =
      ReceiverOption.contains(properties, ReceiverOption.NILLED_ELEMENT)
    namespaceStack.push(namespaces)
    val isTopWithinEntity: Boolean = location
      .isInstanceOf[ReceivingContentHandler.LocalLocator] &&
      location
        .asInstanceOf[ReceivingContentHandler.LocalLocator]
        .levelInEntity ==
        0
    val xmlId: AttributeInfo =
      lSuppliedAttributes.get(NamespaceConstant.XML, "id")
    if (xmlId != null && Whitespace.containsWhitespace(xmlId.getValue)) {
      lSuppliedAttributes = lSuppliedAttributes.put(
        new AttributeInfo(xmlId.getNodeName,
          xmlId.getType,
          Whitespace.trim(xmlId.getValue),
          xmlId.getLocation,
          xmlId.getProperties))
    }
    val elem: ElementImpl = nodeFactory.makeElementNode(
      currentNode,
      elemName,
      `type`,
      isNilled,
      lSuppliedAttributes,
      namespaceStack.peek(),
      pipe,
      location,
      if (allocateSequenceNumbers) { nextNodeNumber += 1; nextNodeNumber - 1 } else
        -1
    )
    while (depth >= arrays.size) arrays.add(Array.ofDim[NodeImpl](20))
    elem.setChildren(arrays.get(depth))
    currentNode.addChild(elem, { size(depth) += 1; size(depth) - 1 })
    if (depth >= size.length - 1) {
      size = Arrays.copyOf(size, size.length * 2)
    }
    size(depth) = 0
    if (currentNode.isInstanceOf[TreeInfo]) {
      currentNode.asInstanceOf[DocumentImpl].setDocumentElement(elem)
    }
    if (isTopWithinEntity) {
      currentNode.getPhysicalRoot.markTopWithinEntity(elem)
    }
    currentNode = elem
  }

  def endElement(): Unit = {
    currentNode.compact(size(depth))
    depth = depth - 1
    currentNode = currentNode.getParent.asInstanceOf[ParentNodeImpl]
    namespaceStack.pop()
  }

  def characters(chars: CharSequence,
                 locationId: Location,
                 properties: Int): Unit = {
    if (chars.length > 0) {
      val prev: NodeInfo = currentNode.getNthChild(size(depth) - 1)
      if (prev.isInstanceOf[TextImpl]) {
        prev.asInstanceOf[TextImpl].appendStringValue(chars.toString)
      } else {
        val n: TextImpl = nodeFactory.makeTextNode(currentNode, chars)
        currentNode.addChild(n, { size(depth) += 1; size(depth) - 1 })
      }
    }
  }

  def processingInstruction(name: String,
                            remainder: CharSequence,
                            locationId: Location,
                            properties: Int): Unit = {
    val pi: ProcInstImpl = new ProcInstImpl(name, remainder.toString)
    currentNode.addChild(pi, { size(depth) += 1; size(depth) - 1 })
    pi.setLocation(locationId.getSystemId,
      locationId.getLineNumber,
      locationId.getColumnNumber)
  }

  def comment(chars: CharSequence,
              locationId: Location,
              properties: Int): Unit = {
    val comment: CommentImpl = new CommentImpl(chars.toString)
    currentNode.addChild(comment, { size(depth) += 1; size(depth) - 1 })
    comment.setLocation(locationId.getSystemId,
      locationId.getLineNumber,
      locationId.getColumnNumber)
  }

  def getCurrentParentNode: ParentNodeImpl = currentNode

  def getCurrentLeafNode: NodeImpl = currentNode.getLastChild

  def graftElement(element: ElementImpl): Unit = {
    currentNode.addChild(element, { size(depth) += 1; size(depth) - 1 })
  }

  def setUnparsedEntity(name: String, uri: String, publicId: String): Unit = {
    if (currentRoot.asInstanceOf[DocumentImpl].getUnparsedEntity(name) ==
      null) {
      currentRoot
        .asInstanceOf[DocumentImpl]
        .setUnparsedEntity(name, uri, publicId)
    }
  }

  override def getBuilderMonitor(): BuilderMonitor = new LinkedBuilderMonitor(this)

}