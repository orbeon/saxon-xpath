////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
package org.orbeon.saxon.s9api

import org.orbeon.saxon.utils.Configuration
object HostLanguage extends Enumeration {

  val XSLT: HostLanguage = new HostLanguage()

  val XQUERY: HostLanguage = new HostLanguage()

  val XML_SCHEMA: HostLanguage = new HostLanguage()

  val XPATH: HostLanguage = new HostLanguage()

  val XSLT_PATTERN: HostLanguage = new HostLanguage()

  class HostLanguage extends Val

  implicit def convertValue(v: Value): HostLanguage =
    v.asInstanceOf[HostLanguage]

}

// Copyright (c) 2018-2020 Saxonica Limited
// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is "Incompatible With Secondary Licenses", as defined by the Mozilla Public License, v. 2.0.
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
/**
  * Identifies a host language in which XPath expressions appear. Generally used when different error codes
  * need to be returned depending on the host language.
  * @since 10.0; previously an integer constant in class {@link Configuration} was used
  */
