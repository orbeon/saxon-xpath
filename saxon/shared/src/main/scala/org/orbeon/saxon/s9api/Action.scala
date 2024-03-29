////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
package org.orbeon.saxon.s9api




@FunctionalInterface
trait Action {

  /**
    * Perform the requested action
    * @throws SaxonApiException if the action fails for any reason
    */
  def act(): Unit

}

// Copyright (c) 2018-2020 Saxonica Limited
// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is "Incompatible With Secondary Licenses", as defined by the Mozilla Public License, v. 2.0.
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
/**
  * A completely general functional interface for functions that take
  * no arguments, return void, and potentially throw a {@link SaxonApiException}.
  */
