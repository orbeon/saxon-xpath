////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
package net.sf.saxon.s9api

import java.net.URI

import java.util.function.Consumer




/**
  * An abstract class providing reusable code for implementing the {@link Destination} interface}
  */
abstract class AbstractDestination extends Destination {

   var helper: DestinationHelper = new DestinationHelper(this)

  private var baseURI: URI = _

  def setDestinationBaseURI(baseURI: URI): Unit = {
    this.baseURI = baseURI
  }

  def getDestinationBaseURI(): URI = baseURI

  def onClose(listener: Action): Unit = {
    helper.onClose(listener)
  }

  def closeAndNotify(): Unit = {
    helper.closeAndNotify()
  }

}

// Copyright (c) 2018-2020 Saxonica Limited
// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is "Incompatible With Secondary Licenses", as defined by the Mozilla Public License, v. 2.0.
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
