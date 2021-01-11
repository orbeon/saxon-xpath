package org.orbeon.saxon.functions

import org.orbeon.saxon.expr.{Atomizer, Expression, XPathContext}
import org.orbeon.saxon.om.{Item, Sequence, SequenceTool}


class Data_1 extends SystemFunction {

  /**
   * Make an expression that either calls this function, or that is equivalent to a call
   * on this function
   *
   * @param arguments the supplied arguments to the function call
   * @return either a function call on this function, or an expression that delivers
   *         the same result
   */
  override def makeFunctionCall(arguments: Expression*): Expression =
    Atomizer.makeAtomizer(arguments(0), null)

  /**
   * Evaluate the expression. (Used for run-time evaluation only)
   *
   * @param context   the dynamic evaluation context
   * @param arguments the values of the arguments, supplied as Sequences
   * @return the result of the evaluation, in the form of a Sequence
   * @throws net.sf.saxon.trans.XPathException
   * if a dynamic error occurs during the evaluation of the expression
   */
  def call(context: XPathContext, arguments: Array[Sequence]): Sequence = {
    val arg = arguments(0)
    arg match {
      case item: Item =>
        item.atomize()
      case _  =>
        val a = Atomizer.getAtomizingIterator(arg.iterate(), oneToOne = false)
        SequenceTool.toLazySequence(a)
    }
  }
}
