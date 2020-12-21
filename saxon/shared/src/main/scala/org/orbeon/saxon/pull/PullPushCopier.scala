////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// Copyright (c) 2018-2020 Saxonica Limited
// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is "Incompatible With Secondary Licenses", as defined by the Mozilla Public License, v. 2.0.
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
package org.orbeon.saxon.pull

import org.orbeon.saxon.event.Receiver


/**
  * This class copies a document by using the pull interface to read the input document,
  * and the push interface to write the output document.
  */
class PullPushCopier(private var in: PullProvider, private var out: Receiver) {

  def copy(): Unit = {
    out.open()
    val tee = new PullPushTee(in, out)
    new PullConsumer(tee).consume()
    out.close()
  }

  def append(): Unit = {
    val tee = new PullPushTee(in, out)
    new PullConsumer(tee).consume()
  }
}
