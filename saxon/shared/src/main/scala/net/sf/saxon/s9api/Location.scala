////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// Copyright (c) 2018-2020 Saxonica Limited
// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is "Incompatible With Secondary Licenses", as defined by the Mozilla Public License, v. 2.0.
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
/**
  * Saxon interface to represent a location, typically the location of an expression within a query
  * or stylesheet. The interface combines the two JAXP interfaces SourceLocator and Locator.
  */

package net.sf.saxon.s9api

import javax.xml.transform.SourceLocator
import org.xml.sax.Locator


trait Location extends SourceLocator with Locator {
  def getSystemId: String
  def getPublicId: String
  def getLineNumber: Int
  def getColumnNumber(): Int
  def saveLocation(): Location
}
