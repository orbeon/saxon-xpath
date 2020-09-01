////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
package net.sf.saxon.value

import net.sf.saxon.expr.Expression

import net.sf.saxon.expr.XPathContext

import net.sf.saxon.om.Item

import net.sf.saxon.om.Sequence

import net.sf.saxon.om.ZeroOrOne

import net.sf.saxon.trans.XPathException

import net.sf.saxon.tree.iter.SingletonIterator

import net.sf.saxon.tree.iter.UnfailingIterator


class SingletonClosure(exp: Expression, context: XPathContext)
  extends Closure
    with Sequence {

  private var built: Boolean = false

  /*@Nullable*/

  private var value: Item = null

  expression = exp

  savedXPathContext = context.newContext()

  saveContext(exp, context)

  /*@NotNull*/

  override def iterate(): UnfailingIterator = SingletonIterator.makeIterator(asItem())

  /*@Nullable*/

  def asItem(): Item = {
    if (!built) {
      value = expression.evaluateItem(savedXPathContext)
      built = true
      // release variables saved in the context to the garbage collector
      savedXPathContext = null
    }
    value
  }

  /*@Nullable*/

  def itemAt(n: Int): Item = {
    if (n != 0) {
      return null
    }
    asItem()
  }

  def getLength: Int = if (asItem() == null) 0 else 1

  override def materialize(): ZeroOrOne[Item] = new ZeroOrOne(asItem())

  override def makeRepeatable(): SingletonClosure = this

}

// Copyright (c) 2018-2020 Saxonica Limited
// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is "Incompatible With Secondary Licenses", as defined by the Mozilla Public License, v. 2.0.
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
/**
 * A SingletonClosure represents a value that has not yet been evaluated: the value is represented
 * by an expression, together with saved values of all the context variables that the
 * expression depends on. The value of a SingletonClosure is always either a single item
 * or an empty sequence.
 * <p>The expression may depend on local variables and on the context item; these values
 * are held in the saved XPathContext object that is kept as part of the Closure, and they
 * will always be read from that object. The expression may also depend on global variables;
 * these are unchanging, so they can be read from the Bindery in the normal way. Expressions
 * that depend on other contextual information, for example the values of position(), last(),
 * current(), current-group(), should not be evaluated using this mechanism: they should
 * always be evaluated eagerly. This means that the Closure does not need to keep a copy
 * of these context variables.</p>
 */
