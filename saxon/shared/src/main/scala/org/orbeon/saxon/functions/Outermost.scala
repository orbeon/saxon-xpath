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

class Outermost extends SystemFunction {

  var presorted: Boolean = false

  override def makeOptimizedFunctionCall(visitor: ExpressionVisitor,
                                          contextInfo: ContextItemStaticInfo,
                                          arguments: Expression*): Expression = {
    if ((arguments(0).getSpecialProperties & StaticProperty.PEER_NODESET) !=
      0) {
      arguments(0)
    }
    presorted = (arguments(0).getSpecialProperties & StaticProperty.ORDERED_NODESET) !=
      0
    null
  }

  override def getSpecialProperties(arguments: Array[Expression]): Int =
    StaticProperty.ORDERED_NODESET | StaticProperty.PEER_NODESET

  def call(context: XPathContext, arguments: Array[Sequence]): Sequence = {
    var in: SequenceIterator = arguments(0).iterate()
    if (!presorted) {
      in = new DocumentOrderIterator(in, GlobalOrderComparer.getInstance)
    }
    val out: SequenceIterator = new OutermostIterator(in)
    SequenceTool.toLazySequence(out)
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

  private class OutermostIterator(var in: SequenceIterator)
    extends SequenceIterator {

    var current: NodeInfo = null

    var position: Int = 0

    def next(): NodeInfo = {
      while (true) {
        val next: NodeInfo = in.next().asInstanceOf[NodeInfo]
        if (next == null) {
          current = null
          position = -1
          return null
        }
        if (current == null || !Navigator.isAncestorOrSelf(current, next)) {
          current = next
          position += 1
          return current
        }
      }
      null
    }

    override def close(): Unit = {
      in.close()
    }

  }

  override def getStreamerName: String = "Outermost"

}
