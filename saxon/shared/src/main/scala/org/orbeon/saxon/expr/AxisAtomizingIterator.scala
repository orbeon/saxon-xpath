////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
package org.orbeon.saxon.expr

import org.orbeon.saxon.om.AtomicSequence

import org.orbeon.saxon.om.AtomizedValueIterator

import org.orbeon.saxon.om.SequenceIterator

import org.orbeon.saxon.trans.XPathException

import org.orbeon.saxon.value.AtomicValue


class AxisAtomizingIterator(private var base: AtomizedValueIterator) extends SequenceIterator {

  private var results: AtomicSequence = null

  private var atomicPosition: Int = 0

  def next(): AtomicValue = {
    while (true) {
      if (results != null) {
        if (atomicPosition < results.getLength) {
          return results.itemAt({
            atomicPosition += 1; atomicPosition - 1
          })
        } else {
          results = null
          //continue
        }
      }
      val atomized: AtomicSequence = base.nextAtomizedValue()
      if (atomized == null) {
        results = null
        return null
      }
      if (atomized.isInstanceOf[AtomicValue]) {
        // common case (the atomized value of the node is a single atomic value)
        results = null
        return atomized.asInstanceOf[AtomicValue]
      } else {
        results = atomized
        atomicPosition = 0
      }
    }
    null
  }

  override def close(): Unit = {
    base.close()
  }

}

// Copyright (c) 2018-2020 Saxonica Limited
// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is "Incompatible With Secondary Licenses", as defined by the Mozilla Public License, v. 2.0.
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
/**
 * This iterator returns a sequence of atomic values, the result of atomizing the sequence
 * of nodes returned by an underlying SequenceIterator.
 */
