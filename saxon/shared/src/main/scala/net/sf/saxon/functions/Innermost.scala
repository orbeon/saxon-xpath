package net.sf.saxon.functions

import net.sf.saxon.expr.Expression

import net.sf.saxon.expr.StaticProperty

import net.sf.saxon.expr.XPathContext

import net.sf.saxon.expr.parser.ContextItemStaticInfo

import net.sf.saxon.expr.parser.ExpressionVisitor

import net.sf.saxon.expr.sort.DocumentOrderIterator

import net.sf.saxon.expr.sort.GlobalOrderComparer

import net.sf.saxon.om.NodeInfo

import net.sf.saxon.om.Sequence

import net.sf.saxon.om.SequenceIterator

import net.sf.saxon.om.SequenceTool

import net.sf.saxon.trace.ExpressionPresenter

import net.sf.saxon.trans.XPathException

import net.sf.saxon.tree.util.Navigator

import java.util.Properties

class Innermost extends SystemFunction {

  var presorted: Boolean = false

  override def getSpecialProperties(arguments: Array[Expression]): Int =
    StaticProperty.ORDERED_NODESET | StaticProperty.PEER_NODESET

  override def makeOptimizedFunctionCall(
                                          visitor: ExpressionVisitor,
                                          contextInfo: ContextItemStaticInfo,
                                          arguments: Expression*): Expression = {
    if ((arguments(0).getSpecialProperties & StaticProperty.PEER_NODESET) !=
      0) {
      arguments(0)
    }
    if ((arguments(0).getSpecialProperties & StaticProperty.ORDERED_NODESET) !=
      0) {
      presorted = true
    }
    super.makeOptimizedFunctionCall(visitor, contextInfo, arguments: _*)
  }

  def call(context: XPathContext, arguments: Array[Sequence]): Sequence =
    SequenceTool.toLazySequence(innermost(arguments(0).iterate()))

  def innermost(in: SequenceIterator): SequenceIterator = {
    var seqIn = in
    if (!presorted) {
      seqIn = new DocumentOrderIterator(seqIn, GlobalOrderComparer.getInstance)
    }
    new InnermostIterator(seqIn)
  }

  override def exportAttributes(out: ExpressionPresenter): Unit = {
    super.exportAttributes(out)
    if (presorted) {
      out.emitAttribute("flags", "p")
    }
  }

  override def importAttributes(attributes: Properties): Unit = {
    super.importAttributes(attributes)
    val flags: String = attributes.getProperty("flags")
    if (flags != null && flags.contains("p")) {
      presorted = true
    }
  }

  private class InnermostIterator(var in: SequenceIterator)
    extends SequenceIterator {

    var pending: NodeInfo = in.next().asInstanceOf[NodeInfo]

    var position: Int = 0

    def next(): NodeInfo =
      if (pending == null) {
        position = -1
        null
      } else {
        while (true) {
          val next: NodeInfo = in.next().asInstanceOf[NodeInfo]
          if (next == null) {
            val current: NodeInfo = pending
            position += 1
            pending = null
            return current
          }
          if (Navigator.isAncestorOrSelf(pending, next)) {
            pending = next
          } else {
            position += 1
            val current: NodeInfo = pending
            pending = next
            return current
          }
        }
        null
      }

    override def close(): Unit = {
      in.close()
    }

  }

}
