////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
package org.orbeon.saxon.expr

import org.orbeon.saxon.om.MutableNodeInfo

import org.orbeon.saxon.om.NodeInfo

import org.orbeon.saxon.trans.XPathException

import java.util.Set




/**
  * A PendingUpdateList is created by updating expressions in XQuery Update.
  * <p>The implementation of this interface is in Saxon-EE.</p>
  */
trait PendingUpdateList {

  def apply(context: XPathContext, validationMode: Int): Unit

  def getAffectedTrees: Set[MutableNodeInfo]

  def addPutAction(node: NodeInfo, uri: String, originator: Expression): Unit

}

// Copyright (c) 2018-2020 Saxonica Limited
// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is "Incompatible With Secondary Licenses", as defined by the Mozilla Public License, v. 2.0.
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
