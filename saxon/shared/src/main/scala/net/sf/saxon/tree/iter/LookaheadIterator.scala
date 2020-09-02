////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// Copyright (c) 2018-2020 Saxonica Limited
// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is "Incompatible With Secondary Licenses", as defined by the Mozilla Public License, v. 2.0.
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
/**
  * A SequenceIterator is used to iterate over a sequence. A LookaheadIterator
  * is one that supports a hasNext method to determine if there are more nodes
  * after the current node.
  */

package net.sf.saxon.tree.iter

import net.sf.saxon.om.SequenceIterator


trait LookaheadIterator extends SequenceIterator {
  def hasNext: Boolean
}
