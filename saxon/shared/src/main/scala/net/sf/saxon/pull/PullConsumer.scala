////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
package net.sf.saxon.pull

import net.sf.saxon.trans.XPathException




class PullConsumer(private var in: PullProvider) {

  def consume(): Unit = {
    while (true) if (in.next() == PullProvider.Event.END_OF_INPUT) {
      return
    }
  }

}

// Copyright (c) 2018-2020 Saxonica Limited
// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is "Incompatible With Secondary Licenses", as defined by the Mozilla Public License, v. 2.0.
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
/**
  * A PullConsumer consumes all the events supplied by a PullProvider, doing nothing
  * with them. The class exists so that PullFilters on the pipeline can produce side-effects.
  * For example, this class can be used to validate a document, where the side effects are
  * error messages.
  */
