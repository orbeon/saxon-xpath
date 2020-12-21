////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// Copyright (c) 2018-2020 Saxonica Limited
// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is "Incompatible With Secondary Licenses", as defined by the Mozilla Public License, v. 2.0.
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

package org.orbeon.saxon.z

import java.util.function.IntPredicate


object IntSetPredicate {
  val ALWAYS_TRUE  : IntPredicate = _ => true
  val ALWAYS_FALSE : IntPredicate = _ => false
}

/**
  * An implementation of IntPredicate that tests whether a given integer is a member
  * of some IntSet
  */
class IntSetPredicate(protected val set: IntSet) extends IntPredicate {

  require(set ne null)

  /**
    * Ask whether a given value matches this predicate
    *
    * @param value the value to be tested
    * @return true if the value matches; false if it does not
    */
  def test(value: Int): Boolean = set.contains(value)

  /**
    * Returns a composed predicate that represents a short-circuiting logical
    * OR of this predicate and another.  When evaluating the composed
    * predicate, if this predicate is `true`, then the `other`
    * predicate is not evaluated.
    *
    * Any exceptions thrown during evaluation of either predicate are relayed
    * to the caller; if evaluation of this predicate throws an exception, the
    * `other` predicate will not be evaluated.
    *
    * @param other a predicate that will be logically-ORed with this
    *              predicate
    * @return a composed predicate that represents the short-circuiting logical
    *         OR of this predicate and the `other` predicate
    * @throws NullPointerException if other is null
    */
  override def or(other: IntPredicate): IntPredicate =
    other match {
      case predicate: IntSetPredicate =>
        new IntSetPredicate(set.union(predicate.set))
      case _ =>
        super.or(other)
    }

  def getIntSet: IntSet = set

  override def toString: String = "in {" + set + "}"
}
