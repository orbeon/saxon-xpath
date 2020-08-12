////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
package net.sf.saxon.om

import net.sf.saxon.trans.XPathException




@FunctionalInterface
trait ItemConsumer[T <: Item] {

  /**
    * Process one item
    * @param item the item to be processed
    * @throws XPathException if the processing fails
    */
  def accept(item: T): Unit

}

// Copyright (c) 2018-2020 Saxonica Limited
// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is "Incompatible With Secondary Licenses", as defined by the Mozilla Public License, v. 2.0.
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
/**
  * A FunctionalInterface for any method that processes individual items, and which
  * may generate exceptions.
  * @param <T> the type of items to be processed
  */
