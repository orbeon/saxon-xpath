////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
package org.orbeon.saxon.lib

import org.orbeon.saxon.om.Sequence

import org.orbeon.saxon.trans.XPathException

import javax.xml.transform.Source




/**
  * Interface for reporting validation errors found during validation of an instance document
  * against a schema.
  */
trait InvalidityHandler {

  def startReporting(systemId: String): Unit

  def reportInvalidity(failure: Invalidity): Unit

  def endReporting(): Sequence

}

// Copyright (c) 2018-2020 Saxonica Limited
// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is "Incompatible With Secondary Licenses", as defined by the Mozilla Public License, v. 2.0.
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
