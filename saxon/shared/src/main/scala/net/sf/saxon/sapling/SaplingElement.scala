package net.sf.saxon.sapling

import scala.jdk.CollectionConverters._
import net.sf.saxon.event.Builder
import net.sf.saxon.event.PipelineConfiguration
import net.sf.saxon.event.Receiver
import net.sf.saxon.event.ReceiverOption
import net.sf.saxon.expr.parser.Loc
import net.sf.saxon.ma.trie.ImmutableHashTrieMap
import net.sf.saxon.ma.trie.ImmutableList
import net.sf.saxon.ma.trie.ImmutableMap
import net.sf.saxon.ma.trie.Tuple2
import net.sf.saxon.model.BuiltInAtomicType
import net.sf.saxon.model.Type
import net.sf.saxon.model.Untyped
import net.sf.saxon.om._
import net.sf.saxon.s9api._
import java.util.Objects

import net.sf.saxon.utils.Configuration

class SaplingElement(name: String) extends SaplingNode {

  private var nodeName: StructuredQName = StructuredQName.fromEQName(name)

  private var reversedChildren: ImmutableList[SaplingNode] =
    ImmutableList.empty()

  private var attributes: ImmutableMap[StructuredQName, String] =
    ImmutableHashTrieMap.empty()

  private var namespaces: NamespaceMap = NamespaceMap.emptyMap

  Objects.requireNonNull(name)

  def this(name: QName) = {
    this("")
    Objects.requireNonNull(name)
    nodeName = name.getStructuredQName
    if (!nodeName.getPrefix.isEmpty && nodeName.getURI.isEmpty) {
      throw new IllegalArgumentException(
        "No namespace URI for prefixed element name: " + name)
    }
    namespaces = NamespaceMap.of(nodeName.getPrefix, nodeName.getURI)
  }

  def this(name: StructuredQName) = {
    this("")
    this.nodeName = name
  }

  override def getNodeKind(): Int = Type.ELEMENT

  private def copy(): SaplingElement = {
    val e2: SaplingElement = new SaplingElement(nodeName)
    e2.reversedChildren = reversedChildren
    e2.attributes = attributes
    e2.namespaces = namespaces
    e2
  }

  def withChild(children: SaplingNode*): SaplingElement = {
    val e2: SaplingElement = copy()
    for (child <- children) {
      child.getNodeKind match {
        case Type.DOCUMENT =>
          throw new IllegalArgumentException(
            "Cannot add document node as a child of an element node")
        case Type.ELEMENT | Type.TEXT | Type.COMMENT |
             Type.PROCESSING_INSTRUCTION =>
          e2.reversedChildren = e2.reversedChildren.prepend(child)

      }
    }
    e2
  }

  def withText(value: String): SaplingElement =
    withChild(new SaplingText(value))

  private def withAttribute(name: StructuredQName,
                            value: String): SaplingElement = {
    val e2: SaplingElement = copy()
    e2.attributes = e2.attributes.put(name, value)
    e2
  }

  def withAttr(name: String, value: String): SaplingElement =
    withAttribute(new StructuredQName("", "", name), value)

  def withAttr(name: QName, value: String): SaplingElement = {
    val attName: StructuredQName = name.getStructuredQName
    if (attName.getPrefix.isEmpty && !attName.getURI.isEmpty) {
      throw new IllegalArgumentException(
        "An attribute whose name is in a namespace must have a prefix")
    }
    withNamespace(attName.getPrefix, attName.getURI)
    withAttribute(attName, value)
  }

  def withNamespace(prefix: String, uri: String): SaplingElement = {
    if (uri.isEmpty) {
      if (prefix.isEmpty) {
        this
      } else {
        throw new IllegalArgumentException(
          "Cannot bind non-empty prefix to empty URI")
      }
    }
    val existingURI: String = namespaces.getURI(prefix)
    if (existingURI != null) {
      if (existingURI == uri) {
        this
      } else {
        throw new IllegalStateException(
          "Inconsistent namespace bindings for prefix '" + prefix +
            "'")
      }
    }
    val e2: SaplingElement = copy()
    e2.namespaces = namespaces.put(prefix, uri)
    e2
  }

   override def sendTo(receiver: Receiver): Unit = {
    val config: Configuration =
      receiver.getPipelineConfiguration.getConfiguration
    val namePool: NamePool = config.getNamePool
    var ns: NamespaceMap = namespaces
    if (!nodeName.getURI.isEmpty) {
      ns = ns.put(nodeName.getPrefix, nodeName.getURI)
    }
    var atts: AttributeMap = EmptyAttributeMap.getInstance
    for (attribute <- attributes.asScala) {
      atts = atts.put(
        new AttributeInfo(new FingerprintedQName(attribute._1, namePool),
          BuiltInAtomicType.UNTYPED_ATOMIC,
          attribute._2,
          Loc.NONE,
          ReceiverOption.NONE))
      if (!attribute._1.getURI.isEmpty) {
        ns = ns.put(attribute._1.getPrefix, attribute._1.getURI)
      }
    }
    receiver.startElement(new FingerprintedQName(nodeName, namePool),
      Untyped.getInstance,
      atts,
      ns,
      Loc.NONE,
      ReceiverOption.NONE)
    val children: ImmutableList[SaplingNode] = reversedChildren.reverse()
    for (node <- children.asScala) {
      node.sendTo(receiver)
    }
    receiver.endElement()
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

}
