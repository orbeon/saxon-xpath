////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
package net.sf.saxon.event

import net.sf.saxon.om.SequenceIterator

import net.sf.saxon.trans.XPathException




object SequenceCopier {

  def copySequence(in: SequenceIterator, out: Receiver): Unit = {
    out.open()
    in.forEachOrFail(out.append)
    out.close()
  }

}

// Copyright (c) 2018-2020 Saxonica Limited
// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is "Incompatible With Secondary Licenses", as defined by the Mozilla Public License, v. 2.0.
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
