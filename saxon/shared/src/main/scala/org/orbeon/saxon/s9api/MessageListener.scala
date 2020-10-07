////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
package org.orbeon.saxon.s9api

import javax.xml.transform.SourceLocator




/**
  * A user-written implementation of the MessageListener interface may be registered with the XsltTransformer
  * to receive notification of xsl:message output. Each xsl:message instruction that is evaluated results in
  * a single call to the MessageListener
  */
trait MessageListener {

  def message(content: XdmNode,
              terminate: Boolean,
              locator: SourceLocator): Unit

}

// Copyright (c) 2018-2020 Saxonica Limited
// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is "Incompatible With Secondary Licenses", as defined by the Mozilla Public License, v. 2.0.
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
