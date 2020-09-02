package net.sf.saxon.functions.hof

import net.sf.saxon.expr.{Callable, LastPositionFinder, XPathContext}
import net.sf.saxon.om.Sequence
import net.sf.saxon.trans.XPathException
import net.sf.saxon.tree.iter.ManualIterator

class CallableWithBoundFocus(private var target: Callable,
                             context: XPathContext)
  extends Callable {

  private var boundContext: XPathContext = context.newContext()

  if (context.getCurrentIterator == null) {
    boundContext.setCurrentIterator(null)
  } else {
    val iter: ManualIterator = new ManualIterator(
      context.getContextItem,
      context.getCurrentIterator.position)
    iter.setLastPositionFinder(context.getLast.asInstanceOf[LastPositionFinder])
    boundContext.setCurrentIterator(iter)
  }

  def call(context: XPathContext, arguments: Array[Sequence]): Sequence =
    target.call(boundContext, arguments)

}
