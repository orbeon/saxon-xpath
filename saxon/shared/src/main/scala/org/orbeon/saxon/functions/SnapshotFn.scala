package org.orbeon.saxon.functions

import org.orbeon.saxon.event.{BuilderMonitor, Receiver, ReceiverOption}
import org.orbeon.saxon.expr.{Expression, ItemMappingFunction, ItemMappingIterator, XPathContext}
import org.orbeon.saxon.expr.parser.Loc
import org.orbeon.saxon.model.{AnyType, SchemaType, Type, Untyped}
import org.orbeon.saxon.om._

import org.orbeon.saxon.functions.SnapshotFn._
import org.orbeon.saxon.tree.tiny.TinyBuilder
import org.orbeon.saxon.tree.wrapper.{SnapshotNode, VirtualCopy}

import java.util.{ArrayList, List}
import scala.jdk.CollectionConverters._


object SnapshotFn {

  def snapshotSequence(nodes: SequenceIterator, context: XPathContext): SequenceIterator =
    new ItemMappingIterator(nodes, getMappingFunction)

  def getMappingFunction: ItemMappingFunction = SnapshotFn.snapshotSingle

  def snapshotSingle(origin: Item): Item =
    origin match {
      case nodeInfo: NodeInfo =>
        if (nodeInfo.getParent == null) {
          val vc = VirtualCopy.makeVirtualCopy(nodeInfo)
          vc.getTreeInfo.setCopyAccumulators(true)
          vc
        } else {
          SnapshotNode.makeSnapshot(nodeInfo)
        }
      case _ =>
        origin
    }

  def makeAncestorList(origin: NodeInfo): List[NodeInfo] = {
    val ancestors = new ArrayList[NodeInfo](20)
    origin.iterateAxis(AxisInfo.ANCESTOR).forEachNode(res => ancestors.add(res))
    ancestors
  }

  def openAncestors(origin: NodeInfo,
                    ancestors: List[NodeInfo],
                    context: XPathContext): BuilderMonitor = {
    val root    = origin.getRoot
    val builder = new TinyBuilder(context.getController.makePipelineConfiguration)
    builder.setStatistics(context.getConfiguration.getTreeStatistics.TEMPORARY_TREE_STATISTICS)
    builder.setSystemId(root.getSystemId)
    builder.setTiming(false)
    val bm = builder.getBuilderMonitor
    bm.open()
    val source           = root.getTreeInfo
    val unparsedEntities = source.getUnparsedEntityNames
    while (unparsedEntities.hasNext) {
      val name       = unparsedEntities.next()
      val properties = source.getUnparsedEntity(name)
      builder.setUnparsedEntity(name, properties(0), properties(1))
    }
    val ancestorType =
      if (context.getController.getExecutable.isSchemaAware)
        AnyType.getInstance
      else
        Untyped.getInstance
    var i = ancestors.size - 1
    while (i >= 0) {
      val anc: NodeInfo = ancestors.get(i)
      val kind: Int = anc.getNodeKind
      kind match {
        case Type.ELEMENT =>
          bm.startElement(NameOfNode.makeName(anc),
            ancestorType,
            anc.attributes,
            anc.getAllNamespaces,
            Loc.NONE,
            ReceiverOption.NONE)
        case Type.DOCUMENT =>
          bm.startDocument(ReceiverOption.NONE)
        case _ =>
          throw new IllegalStateException("Unknown ancestor node kind " + anc.getNodeKind)
      }
      i -= 1
    }
    bm
  }

  def closeAncestors(ancestors: List[NodeInfo], bm: Receiver): Unit = {
    for (anc <- ancestors.asScala) {
      anc.getNodeKind match {
        case Type.ELEMENT =>
          bm.endElement()
        case Type.DOCUMENT =>
          bm.endDocument()
        case _ =>
          throw new IllegalStateException("Unknown ancestor node kind " + anc.getNodeKind)
      }
    }
    bm.close()
  }
}

class SnapshotFn extends SystemFunction {

  override def getCardinality(arguments: Array[Expression]): Int =
    arguments(0).getCardinality

  def call(context: XPathContext, arguments: Array[Sequence]): Sequence = {
    val in: Sequence =
      if (arguments.length == 0) context.getContextItem else arguments(0)
    val iter: SequenceIterator = snapshotSequence(in.iterate(), context)
    new LazySequence(iter)
  }

  override def getStreamerName: String = "SnapshotFn"
}
