package net.sf.saxon.functions

import net.sf.saxon.expr.XPathContext
import net.sf.saxon.om.{Item, Sequence}

import scala.util.control.Breaks._

abstract class FoldingFunction extends SystemFunction {

  def getFold(context: XPathContext, additionalArguments: Sequence*): Fold

  def call(context: XPathContext, arguments: Sequence*): Sequence = {
    val additionalArgs = Array.ofDim[Sequence](arguments.length - 1)
    System.arraycopy(arguments, 1, additionalArgs, 0, additionalArgs.length)
    val fold = getFold(context, additionalArgs.toIndexedSeq:_*)
    val iter= arguments(0).iterate()
    var item: Item = null
    breakable {
      while ( {
        item = iter.next()
        item
      } != null) {
        fold.processItem(item)
        if (fold.isFinished) {
          break()
        }
      }
    }
    fold.result()
  }

  override def getStreamerName(): String = "Fold"
}
