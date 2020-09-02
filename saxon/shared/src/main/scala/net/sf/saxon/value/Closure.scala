////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
package net.sf.saxon.value

import net.sf.saxon.expr._

import net.sf.saxon.expr.instruct.SlotManager

import net.sf.saxon.om._

import net.sf.saxon.trans.XPathException

import net.sf.saxon.tree.iter.ManualIterator

import Closure._




object Closure {

  /*@NotNull*/

  def make(expression: Expression, context: XPathContext, ref: Int): Sequence =
    context.getConfiguration.makeClosure(expression, ref, context)

}

class Closure extends Sequence with ContextOriginator {

   var expression: Expression = _

  /*@Nullable*/

   var savedXPathContext: XPathContextMajor = _

   var depth: Int = 0

   var inputIterator: SequenceIterator = _

  def saveContext(expression: Expression, context: XPathContext): Unit = {
    if ((expression.getDependencies & StaticProperty.DEPENDS_ON_LOCAL_VARIABLES) !=
          0) {
      val localStackFrame: StackFrame = context.getStackFrame
      val local: Array[Sequence] = localStackFrame.getStackFrameValues
// computed on first call
      val slotsUsed: Array[Int] = expression.getSlotsUsed
      if (local != null) {
        val stackFrameMap: SlotManager = localStackFrame.getStackFrameMap
        val savedStackFrame: Array[Sequence] =
          Array.ofDim[Sequence](stackFrameMap.getNumberOfVariables)
        for (i <- slotsUsed) {
          if (local(i).isInstanceOf[Closure]) {
            val cdepth: Int = local(i).asInstanceOf[Closure].depth
            if (cdepth >= 10) {
              local(i) = local(i).iterate().materialize()
            } else if (cdepth + 1 > depth) {
              depth = cdepth + 1
            }
          }
          savedStackFrame(i) = local(i)
        }
        savedXPathContext.setStackFrame(stackFrameMap, savedStackFrame)
      }
    }
// Make a copy of the context item
    val currentIterator: FocusIterator = context.getCurrentIterator
    if (currentIterator != null) {
      val contextItem: Item = currentIterator.current
      val single: ManualIterator = new ManualIterator(contextItem)
      savedXPathContext.setCurrentIterator(single)
    }
// we don't save position and last() because we have no way
// of restoring them. So the caller must ensure that a Closure is not
// created if the expression depends on position or last()
// we don't save position and last() because we have no way
// of restoring them. So the caller must ensure that a Closure is not
// created if the expression depends on position or last()
  }
// Make a copy of all local variables. If the value of any local variable is a closure
// whose depth exceeds a certain threshold, we evaluate the closure eagerly to avoid
// creating deeply nested lists of Closures, which consume memory unnecessarily
// We only copy the local variables if the expression has dependencies on local variables.
// What's more, we only copy those variables that the expression actually depends on.
// Make a copy of all local variables. If the value of any local variable is a closure
// whose depth exceeds a certain threshold, we evaluate the closure eagerly to avoid
// creating deeply nested lists of Closures, which consume memory unnecessarily
// We only copy the local variables if the expression has dependencies on local variables.
// What's more, we only copy those variables that the expression actually depends on.

  /**
    * Get the first item in the sequence.
    *
    * @return the first item in the sequence if there is one, or null if the sequence
    *         is empty
    * @throws net.sf.saxon.trans.XPathException
    *          in the situation where the sequence is evaluated lazily, and
    *          evaluation of the first item causes a dynamic error.
    */
  def head: Item = iterate().next()

  def getExpression: Expression = expression

  /*@Nullable*/

  def getSavedXPathContext: XPathContextMajor = savedXPathContext

  def setExpression(expression: Expression): Unit = {
    this.expression = expression
  }

  def setSavedXPathContext(savedXPathContext: XPathContextMajor): Unit = {
    this.savedXPathContext = savedXPathContext
  }

  /*@NotNull*/

  def iterate(): SequenceIterator =
    if (inputIterator == null) {
      inputIterator = expression.iterate(savedXPathContext)
      inputIterator
    } else {
//return inputIterator.getAnother();
      throw new IllegalStateException("A Closure can only be read once")
    }
// In an ideal world this shouldn't happen: if the value is needed more than once, we should
// have chosen a MemoClosure.
// Changed Feb 2015 to throw an exception. Processing the expression more than once is not only
// inefficient, it is wrong in the case where node-identity affects the outcome. See for example
// test case fn-fold-left-018
// In an ideal world this shouldn't happen: if the value is needed more than once, we should
// have chosen a MemoClosure.
// Changed Feb 2015 to throw an exception. Processing the expression more than once is not only
// inefficient, it is wrong in the case where node-identity affects the outcome. See for example
// test case fn-fold-left-018

  def reduce(): GroundedValue = iterate().materialize()

  override def makeRepeatable(): Sequence = materialize()

}

// Copyright (c) 2018-2020 Saxonica Limited
// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is "Incompatible With Secondary Licenses", as defined by the Mozilla Public License, v. 2.0.
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
/**
  * A Closure represents a value that has not yet been evaluated: the value is represented
  * by an expression, together with saved values of all the context variables that the
  * expression depends on.
  * <p>This Closure is designed for use when the value is only read once. If the value
  * is read more than once, a new iterator over the underlying expression is obtained
  * each time: this may (for example in the case of a filter expression) involve
  * significant re-calculation.</p>
  * <p>The expression may depend on local variables and on the context item; these values
  * are held in the saved XPathContext object that is kept as part of the Closure, and they
  * will always be read from that object. The expression may also depend on global variables;
  * these are unchanging, so they can be read from the Bindery in the normal way. Expressions
  * that depend on other contextual information, for example the values of position, last(),
  * current, current-group(), should not be evaluated using this mechanism: they should
  * always be evaluated eagerly. This means that the Closure does not need to keep a copy
  * of these context variables.</p>
  */
