////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
package net.sf.saxon.expr

import net.sf.saxon.om.Item

import net.sf.saxon.om.SequenceIterator

import net.sf.saxon.trans.XPathException




/**
  * A SequenceIterator that throws an exception as soon as its next() method is called. Used when
  * the method that returns the iterator isn't allowed to throw a checked exception itself.
  */
class ErrorIterator(private var exception: XPathException)
    extends SequenceIterator {

  def next(): Item = throw exception

  override def close(): Unit = ()

}

// Copyright (c) 2018-2020 Saxonica Limited
// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is "Incompatible With Secondary Licenses", as defined by the Mozilla Public License, v. 2.0.
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
