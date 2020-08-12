////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
package net.sf.saxon.trans.packages

import net.sf.saxon.om.NodeInfo

import net.sf.saxon.style.StylesheetPackage

import net.sf.saxon.trans.XPathException

import javax.xml.transform.Source




trait IPackageLoader {

  def loadPackageDoc(doc: NodeInfo): StylesheetPackage

  def loadPackage(source: Source): StylesheetPackage

}

// Copyright (c) 2018-2020 Saxonica Limited
// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is "Incompatible With Secondary Licenses", as defined by the Mozilla Public License, v. 2.0.
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
