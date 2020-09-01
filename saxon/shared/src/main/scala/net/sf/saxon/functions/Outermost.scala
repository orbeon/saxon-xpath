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
