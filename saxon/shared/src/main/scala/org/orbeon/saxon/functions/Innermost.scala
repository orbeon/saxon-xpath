package org.orbeon.saxon.functions

import org.orbeon.saxon.expr.Expression

import org.orbeon.saxon.expr.StaticProperty

import org.orbeon.saxon.expr.XPathContext

import org.orbeon.saxon.expr.parser.ContextItemStaticInfo

import org.orbeon.saxon.expr.parser.ExpressionVisitor

import org.orbeon.saxon.expr.sort.DocumentOrderIterator

import org.orbeon.saxon.expr.sort.GlobalOrderComparer

import org.orbeon.saxon.om.NodeInfo

import org.orbeon.saxon.om.Sequence

import org.orbeon.saxon.om.SequenceIterator

import org.orbeon.saxon.om.SequenceTool

import org.orbeon.saxon.trace.ExpressionPresenter

import org.orbeon.saxon.trans.XPathException

import org.orbeon.saxon.tree.util.Navigator

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
      return arguments(0)
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
