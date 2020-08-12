////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
package net.sf.saxon.event

import net.sf.saxon.expr.parser.Loc

import net.sf.saxon.om.NodeInfo

import net.sf.saxon.s9api.Location




class LocationCopier(private var wholeDocument: Boolean)
    extends CopyInformee[Location] {

  def notifyElementNode(element: NodeInfo): Location = {
    val systemId: String =
      if (wholeDocument) element.getSystemId else element.getBaseURI
// but XDM doesn't work that way.
    val lineNumber: Int = element.getLineNumber
    val columnNumber: Int = element.getColumnNumber
    new Loc(systemId, lineNumber, columnNumber)
  }
// The logic behind this is that if we are copying the whole document, we will be copying all
// the relevant xml:base attributes; so retaining the systemId values is sufficient to enable
// the base URIs of the nodes to be preserved. But if we only copy an element (for example
// an xsl:import-schema element - see test schema091 - then its base URI might be affected
// by xml:base attributes that aren't being copied. Ideally we would have two separate properties,
// The logic behind this is that if we are copying the whole document, we will be copying all
// the relevant xml:base attributes; so retaining the systemId values is sufficient to enable
// the base URIs of the nodes to be preserved. But if we only copy an element (for example
// an xsl:import-schema element - see test schema091 - then its base URI might be affected
// by xml:base attributes that aren't being copied. Ideally we would have two separate properties,

}

// Copyright (c) 2018-2020 Saxonica Limited
// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is "Incompatible With Secondary Licenses", as defined by the Mozilla Public License, v. 2.0.
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
/**
  * A Receiver that can be inserted into an event pipeline to copy location information.
  * The class is used when it is necessary to copy a subtree along with its location information;
  * for example, when copying an inline schema within a stylesheet to a separate schema document.
  * <p><i>Note: prior to 9.2, the LocationCopier was a ProxyReceiver that passed all events on the
  * pipeline unchanged. It no longer does this, instead it is found as the LocationProvider on a
  * pipeline, but does not itself see the pipeline events.</i></p>
  */
