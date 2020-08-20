////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
package net.sf.saxon.expr

import net.sf.saxon.om.AtomicSequence

import net.sf.saxon.om.AtomizedValueIterator

import net.sf.saxon.om.SequenceIterator

import net.sf.saxon.trans.XPathException

import net.sf.saxon.value.AtomicValue


class AxisAtomizingIterator(private var base: AtomizedValueIterator) extends SequenceIterator {

  private var results: AtomicSequence = null

  private var atomicPosition: Int = 0

  def next(): AtomicValue = {
    while (true) {
      if (results != null) {
        if (atomicPosition < results.getLength) {
          results.itemAt({
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
        atomized.asInstanceOf[AtomicValue]
      } else {
        results = atomized
        atomicPosition = 0
      }
    }
    results.asInstanceOf[AtomicValue]
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
