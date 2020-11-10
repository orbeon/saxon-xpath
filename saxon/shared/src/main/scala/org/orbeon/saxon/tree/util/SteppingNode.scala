////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// Copyright (c) 2018-2020 Saxonica Limited
// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is "Incompatible With Secondary Licenses", as defined by the Mozilla Public License, v. 2.0.
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
package org.orbeon.saxon.tree.util

import org.orbeon.saxon.om.NodeInfo


trait SteppingNode[N <: SteppingNode[N]] extends NodeInfo {
  def getParent: N
  def getNextSibling: N
  def getPreviousSibling: N
  def getFirstChild: N
  def getSuccessorElement(anchor: N, uri: String, local: String): N
}
