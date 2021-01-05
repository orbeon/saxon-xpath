package org.orbeon.saxon.sapling

import org.orbeon.saxon.event._
import org.orbeon.saxon.ma.trie.ImmutableList
import org.orbeon.saxon.model.Type
import org.orbeon.saxon.om.NodeInfo
import org.orbeon.saxon.om.TreeModel
import org.orbeon.saxon.s9api._
import org.orbeon.saxon.serialize.SerializationProperties
import javax.xml.transform.Source
import org.orbeon.saxon.utils.Configuration

//import scala.collection.compat._
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
    throw new UnsupportedOperationException
  }

  override def getSystemId: String = baseUri

  override def getNodeKind: Int = Type.DOCUMENT

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

  // ORBEON: s9api
//  def toXdmNode(processor: Processor): XdmNode =
//    XdmValue
//      .wrap(toNodeInfo(processor.getUnderlyingConfiguration))
//      .asInstanceOf[XdmNode]
//
//  def serialize(serializer: Serializer): Unit = {
//    val proc: Processor = serializer.getProcessor
//    send(proc, serializer)
//  }
//
//  def send(processor: Processor, destination: Destination): Unit = {
//    val pipe: PipelineConfiguration =
//      processor.getUnderlyingConfiguration.makePipelineConfiguration
//    sendTo(destination.getReceiver(pipe, new SerializationProperties()))
//  }

}
