////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// Copyright (c) 2018-2020 Saxonica Limited
// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is "Incompatible With Secondary Licenses", as defined by the Mozilla Public License, v. 2.0.
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
package org.orbeon.saxon.expr.sort

import org.orbeon.saxon.value.AtomicValue


/**
  * This class represents a member of a sequence that is being sorted. The sequence may contain
  * items, tuples, groups, or anything else. An instance of this class holds the object itself, the
  * values of the sort keys, and the original position of the item in the input sequence (which is needed
  * to achieve stable sorting.)
  */
class ObjectToBeSorted[T](numberOfSortKeys: Int) {

  var value: T = _

  var sortKeyValues: Array[AtomicValue] =
    new Array[AtomicValue](numberOfSortKeys)

  var originalPosition: Int = _
}

