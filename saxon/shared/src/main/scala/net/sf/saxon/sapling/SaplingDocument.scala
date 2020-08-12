package net.sf.saxon.sapling

import net.sf.saxon.event._
import net.sf.saxon.ma.trie.ImmutableList
import net.sf.saxon.model.Type
import net.sf.saxon.om.NodeInfo
import net.sf.saxon.om.TreeModel
import net.sf.saxon.s9api._
import net.sf.saxon.serialize.SerializationProperties
import javax.xml.transform.Source
import net.sf.saxon.utils.Configuration

import scala.jdk.CollectionConverters._

class SaplingDocument extends SaplingNode with Source {

  private var baseUri: String = _

  private var reversedChildren: ImmutableList[SaplingNode] =
    ImmutableList.empty()

  def this(baseUri: String) = {
    this()
    this.baseUri = baseUri
  }

  override def setSystemId(systemId: String): Unit = {
    throw new UnsupportedOperationException()
  }

  override def getSystemId(): String = baseUri

  override def getNodeKind(): Int = Type.DOCUMENT

  private def copy(): SaplingDocument = {
    val d2: SaplingDocument = new SaplingDocument(baseUri)
    d2.reversedChildren = reversedChildren
    d2
  }

  def withChild(children: SaplingNode*): SaplingDocument = {
    val e2: SaplingDocument = copy()
    for (node <- children) {
      node.getNodeKind match {
        case Type.DOCUMENT =>
          throw new IllegalArgumentException(
            "Cannot add document child to a document node")
        case Type.ELEMENT | Type.TEXT | Type.COMMENT |
             Type.PROCESSING_INSTRUCTION =>
          e2.reversedChildren = e2.reversedChildren.prepend(node)

      }
    }
    e2
  }

  override def sendTo(receiver: Receiver): Unit = {
    var rec = receiver
    rec = new NamespaceReducer(rec)
    rec.open()
    rec.setSystemId(baseUri)
    rec.startDocument(ReceiverOption.NONE)
    val children: ImmutableList[SaplingNode] = reversedChildren.reverse()
    for (node <- children.asScala) {
      node.sendTo(rec)
    }
    rec.endDocument()
    rec.close()
  }

  def toNodeInfo(config: Configuration): NodeInfo = {
    val pipe: PipelineConfiguration = config.makePipelineConfiguration
    val treeModel: TreeModel = config.getParseOptions.getModel
    val builder: Builder = treeModel.makeBuilder(pipe)
    builder.open()
    sendTo(builder)
    builder.close()
    builder.getCurrentRoot
  }

  def toXdmNode(processor: Processor): XdmNode =
    XdmValue
      .wrap(toNodeInfo(processor.getUnderlyingConfiguration))
      .asInstanceOf[XdmNode]

  def serialize(serializer: Serializer): Unit = {
    val proc: Processor = serializer.getProcessor
    send(proc, serializer)
  }

  def send(processor: Processor, destination: Destination): Unit = {
    val pipe: PipelineConfiguration =
      processor.getUnderlyingConfiguration.makePipelineConfiguration
    sendTo(destination.getReceiver(pipe, new SerializationProperties()))
  }

}
