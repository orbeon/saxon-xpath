package org.orbeon.saxon.functions.hof

import org.orbeon.saxon.expr.{Callable, LastPositionFinder, XPathContext}
import org.orbeon.saxon.om.Sequence
import org.orbeon.saxon.tree.iter.ManualIterator

class CallableWithBoundFocus(private var target: Callable,
                             context: XPathContext)
  extends Callable {

  private val boundContext: XPathContext = context.newContext()

  if (context.getCurrentIterator == null) {
    boundContext.setCurrentIterator(null)
  } else {
    val iter = new ManualIterator(
      context.getContextItem,
      context.getCurrentIterator.position
    )
    iter.setLastPositionFinder(context.getLast.asInstanceOf[LastPositionFinder])
    boundContext.setCurrentIterator(iter)
  }

  def call(context: XPathContext, arguments: Array[Sequence]): Sequence =
    target.call(boundContext, arguments)
}
