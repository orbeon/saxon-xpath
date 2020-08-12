package net.sf.saxon.functions

import net.sf.saxon.event.BuilderMonitor

import net.sf.saxon.event.Receiver

import net.sf.saxon.event.ReceiverOption

import net.sf.saxon.expr.Expression

import net.sf.saxon.expr.ItemMappingFunction

import net.sf.saxon.expr.ItemMappingIterator

import net.sf.saxon.expr.XPathContext

import net.sf.saxon.expr.parser.Loc

import net.sf.saxon.model.AnyType

import net.sf.saxon.model.SchemaType

import net.sf.saxon.model.Type

import net.sf.saxon.model.Untyped

import net.sf.saxon.om._

import scala.jdk.CollectionConverters._

import net.sf.saxon.tree.tiny.TinyBuilder

import net.sf.saxon.tree.wrapper.SnapshotNode

import net.sf.saxon.tree.wrapper.VirtualCopy

import java.util.ArrayList

import java.util.Iterator

import java.util.List

import SnapshotFn._

object SnapshotFn {

  def snapshotSequence(nodes: SequenceIterator,
                       context: XPathContext): SequenceIterator =
    new ItemMappingIterator(nodes, getMappingFunction)

  def getMappingFunction(): ItemMappingFunction = SnapshotFn.snapshotSingle

  def snapshotSingle(origin: Item): Item =
    if (origin.isInstanceOf[NodeInfo]) {
      if (origin.asInstanceOf[NodeInfo].getParent == null) {
        val vc: VirtualCopy =
          VirtualCopy.makeVirtualCopy(origin.asInstanceOf[NodeInfo])
        vc.getTreeInfo.setCopyAccumulators(true)
        vc
      } else {
        SnapshotNode.makeSnapshot(origin.asInstanceOf[NodeInfo])
      }
    } else {
      origin
    }

  def makeAncestorList(origin: NodeInfo): List[NodeInfo] = {
    val ancestors: List[NodeInfo] = new ArrayList[NodeInfo](20)
    origin.iterateAxis(AxisInfo.ANCESTOR).forEachNode(res => ancestors.add(res))
    ancestors
  }

  def openAncestors(origin: NodeInfo,
                    ancestors: List[NodeInfo],
                    context: XPathContext): BuilderMonitor = {
    val root: NodeInfo = origin.getRoot
    val builder: TinyBuilder = new TinyBuilder(
      context.getController.makePipelineConfiguration)
    builder.setStatistics(
      context.getConfiguration.getTreeStatistics.TEMPORARY_TREE_STATISTICS)
    builder.setSystemId(root.getSystemId)
    builder.setTiming(false)
    val bm: BuilderMonitor = builder.getBuilderMonitor
    bm.open()
    val source: TreeInfo = root.getTreeInfo
    val unparsedEntities: Iterator[String] = source.getUnparsedEntityNames
    while (unparsedEntities.hasNext) {
      val name: String = unparsedEntities.next()
      val properties: Array[String] = source.getUnparsedEntity(name)
      builder.setUnparsedEntity(name, properties(0), properties(1))
    }
    val ancestorType: SchemaType =
      if (context.getController.getExecutable.isSchemaAware)
        AnyType.getInstance
      else Untyped.getInstance
    var i: Int = ancestors.size - 1
    while (i >= 0) {
      val anc: NodeInfo = ancestors.get(i)
      val kind: Int = anc.getNodeKind
      kind match {
        case Type.ELEMENT => {
          bm.startElement(NameOfNode.makeName(anc),
            ancestorType,
            anc.attributes(),
            anc.getAllNamespaces,
            Loc.NONE,
            ReceiverOption.NONE)
          //break
        }
        case Type.DOCUMENT => {
          bm.startDocument(ReceiverOption.NONE)
          //break
        }
        case _ =>
          throw new IllegalStateException(
            "Unknown ancestor node kind " + anc.getNodeKind)

      }
      { i -= 1; i + 1 }
    }
    bm
  }

  def closeAncestors(ancestors: List[NodeInfo], bm: Receiver): Unit = {
    for (anc <- ancestors.asScala) {
      anc.getNodeKind match {
        case Type.ELEMENT => {
          bm.endElement()
          //break
        }
        case Type.DOCUMENT => {
          bm.endDocument()
          //break
        }
        case _ =>
          throw new IllegalStateException(
            "Unknown ancestor node kind " + anc.getNodeKind)

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

  override def getStreamerName(): String = "SnapshotFn"

}
