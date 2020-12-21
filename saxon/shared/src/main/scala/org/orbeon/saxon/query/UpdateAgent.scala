////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
package org.orbeon.saxon.query

import org.orbeon.saxon.utils.Controller

import org.orbeon.saxon.om.NodeInfo

import org.orbeon.saxon.trans.XPathException




/**
  * An UpdateAgent is a callback class that is called to handle a document after it has been updated.
  * Typically the UpdateAgent might take responsibility for writing the updated document back to
  * persistent storage.
  */
trait UpdateAgent {

  def update(node: NodeInfo, controller: Controller): Unit

}

// Copyright (c) 2018-2020 Saxonica Limited
// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is "Incompatible With Secondary Licenses", as defined by the Mozilla Public License, v. 2.0.
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
