////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
package net.sf.saxon.functions

import net.sf.saxon.expr.Atomizer

import net.sf.saxon.expr.Expression

import net.sf.saxon.expr.XPathContext

import net.sf.saxon.om.Item

import net.sf.saxon.om.Sequence

import net.sf.saxon.om.SequenceIterator

import net.sf.saxon.om.SequenceTool

import net.sf.saxon.trans.XPathException


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

  /*    Evaluate the expression. (Used for run-time evaluation only)

    @param context   the dynamic evaluation context
   @param arguments the values of the arguments, supplied as Sequences
     @return the result of the evaluation, in the form of a Sequence
     @throws net.sf.saxon.trans.XPathException
              if a dynamic error occurs during the evaluation of the expression*/
  def call(context: XPathContext, arguments: Array[Sequence]): Sequence = {
    val arg: Sequence = arguments(0)
    if (arg.isInstanceOf[Item]) {
      arg.asInstanceOf[Item].atomize()
    } else {
      val a: SequenceIterator =
        Atomizer.getAtomizingIterator(arg.iterate(), oneToOne = false)
      SequenceTool.toLazySequence(a)
    }
  }

}
