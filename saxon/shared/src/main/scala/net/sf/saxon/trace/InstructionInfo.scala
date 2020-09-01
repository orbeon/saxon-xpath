////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
package net.sf.saxon.trace

import net.sf.saxon.om.StructuredQName

import net.sf.saxon.s9api.Location

import java.util.Iterator




trait InstructionInfo extends Location {

  /*@Nullable*/

  def getObjectName: StructuredQName

  /*@Nullable*/

  def getProperty(name: String): AnyRef

  def getProperties: Iterator[String]

}

// Copyright (c) 2018-2020 Saxonica Limited
// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is "Incompatible With Secondary Licenses", as defined by the Mozilla Public License, v. 2.0.
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
/**
  * Information about an instruction in the stylesheet or a construct in a Query, made
  * available at run-time to a TraceListener. Although the class is mainly used to provide
  * information for diagnostics, it also has a role in detecting circularities among
  * global variables.
  */
