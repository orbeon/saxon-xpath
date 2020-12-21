////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
package org.orbeon.saxon.expr

import org.orbeon.saxon.expr.parser.RoleDiagnostic

import org.orbeon.saxon.om.Item

import org.orbeon.saxon.om.SequenceIterator

import org.orbeon.saxon.s9api.Location

import org.orbeon.saxon.trans.XPathException

import org.orbeon.saxon.tree.iter.ArrayIterator

import org.orbeon.saxon.value.Cardinality


class CardinalityCheckingIterator(private var base: SequenceIterator,
                                  requiredCardinality: Int,
                                  role: RoleDiagnostic,
                                  private var locator: Location)
  extends SequenceIterator {

  /*@Nullable*/

  private var first: Item = base.next()

  private var second: Item = null

  private var position: Int = 0

  if (first == null) {
    if (!Cardinality.allowsZero(requiredCardinality)) {
      typeError("An empty sequence is not allowed as the " + role.getMessage,
        role.getErrorCode)
    }
  } else {
    if (requiredCardinality == StaticProperty.EMPTY) {
      typeError("The only value allowed for the " + role.getMessage +
        " is an empty sequence",
        role.getErrorCode)
    }
    second = base.next()
    if (second != null && !Cardinality.allowsMany(requiredCardinality)) {
      val leaders: Array[Item] = Array(first, second)
      typeError(
        "A sequence of more than one item is not allowed as the " +
          role.getMessage +
          CardinalityChecker.depictSequenceStart(new ArrayIterator(leaders),
            2),
        role.getErrorCode
      )
    }
  }

  def next(): Item = {
    if (position < 2) {
      if (position == 0) {
        val current: Item = first
        position = if (first == null) -1 else 1
        return current
      } else if (position == 1) {
        val current: Item = second
        position = if (second == null) -1 else 2
        return current
      } else {
        // position == -1
        return null
      }
    }
    val current: Item = base.next()
    if (current == null) {
      position = -1
    } else {
      position += 1
    }
    current
  }

  override def close(): Unit = {
    base.close()
  }

  private def typeError(message: String, errorCode: String): Unit = {
    val e: XPathException = new XPathException(message, errorCode, locator)
    e.setIsTypeError(!errorCode.startsWith("FORG"))
    throw e
  }

}

// Copyright (c) 2018-2020 Saxonica Limited
// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is "Incompatible With Secondary Licenses", as defined by the Mozilla Public License, v. 2.0.
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
/**
 * CardinalityCheckingIterator returns the items in an underlying sequence
 * unchanged, but checks that the number of items conforms to the required
 * cardinality. Because cardinality checks are required to take place even
 * if the consumer of the sequence does not require all the items, we read
 * the first two items at initialization time. This is sufficient to perform
 * the checks; after that we can simply return the items from the base sequence.
 */
