////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
package org.orbeon.saxon.expr.sort

import org.orbeon.saxon.om.AtomicSequence

import org.orbeon.saxon.om.Item

import org.orbeon.saxon.om.MemoSequence




/**
  * This class is a specialization of ObjectToBeSorted for use when the sequence
  * being sorted is a sequence of groups. The group is represented by its initial
  * item, but the object holds in addition the value of the grouping key, and an
  * iterator over the items in the group.
  */
class GroupToBeSorted(numberOfSortKeys: Int)
    extends ObjectToBeSorted[Item](numberOfSortKeys) {

  /*@Nullable*/

  var currentGroupingKey: AtomicSequence = _

  var currentGroup: MemoSequence = _

}

// Copyright (c) 2018-2020 Saxonica Limited
// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is "Incompatible With Secondary Licenses", as defined by the Mozilla Public License, v. 2.0.
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
