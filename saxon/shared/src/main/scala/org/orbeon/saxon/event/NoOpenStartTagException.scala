////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// Copyright (c) 2018-2020 Saxonica Limited
// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is "Incompatible With Secondary Licenses", as defined by the Mozilla Public License, v. 2.0.
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
package org.orbeon.saxon.event

import org.orbeon.saxon.lib.StandardDiagnostics
import org.orbeon.saxon.model.Type
import org.orbeon.saxon.s9api.HostLanguage._
import org.orbeon.saxon.s9api.{HostLanguage, Location}
import org.orbeon.saxon.trans.XPathException

/**
  * Exception indicating that an attribute or namespace node has been written when
  * there is no open element to write it to
  */
object NoOpenStartTagException {

  def makeNoOpenStartTagException(
      nodeKind: Int,
      name: String,
      hostLanguage: HostLanguage,
      parentIsDocument: Boolean,
      startElementLocationId: Location): NoOpenStartTagException = {
    var message: String = null
    var errorCode: String = null
    if (parentIsDocument) {
      val kind: String =
        if (nodeKind == Type.ATTRIBUTE) "an attribute" else "a namespace"
      message = "Cannot create " + kind + " node (" + name + ") whose parent is a document node"
      errorCode =
        if (hostLanguage == HostLanguage.XSLT) "XTDE0420" else "XPTY0004"
    } else {
      val kind: String =
        if (nodeKind == Type.ATTRIBUTE) "An attribute" else "A namespace"
      message = kind + " node (" + name +
          ") cannot be created after a child of the containing element"
      errorCode =
        if (hostLanguage == HostLanguage.XSLT) "XTDE0410" else "XQTY0024"
    }
    if (startElementLocationId != null && startElementLocationId.getLineNumber != -1) {
      message += ". Most recent element start tag was output at line " +
        startElementLocationId.getLineNumber +
        " of module " +
        new StandardDiagnostics()
          .abbreviateLocationURI(startElementLocationId.getSystemId)
    }
    val err: NoOpenStartTagException = new NoOpenStartTagException(message)
    err.setErrorCode(errorCode)
    err
  }

}

class NoOpenStartTagException(message: String) extends XPathException(message)

