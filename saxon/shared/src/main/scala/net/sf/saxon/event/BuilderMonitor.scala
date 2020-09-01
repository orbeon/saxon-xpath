////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
package net.sf.saxon.event

import net.sf.saxon.om.NodeInfo




/**
  * A BuilderMonitor can be inserted into a pipeline immediately in front of a Builder. During tree construction,
  * the method markNextNode() can be called to request that the next node to be created is treated specially by
  * remembering the current position on the tree; on completion of the tree construction, the method getMarkedNode()
  * can be called to return the NodeInfo that was created immediately after calling markNextNode().
  */
abstract class BuilderMonitor(next: Receiver) extends ProxyReceiver(next) {

  def markNextNode(nodeKind: Int): Unit

  def getMarkedNode: NodeInfo

}

// Copyright (c) 2018-2020 Saxonica Limited
// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is "Incompatible With Secondary Licenses", as defined by the Mozilla Public License, v. 2.0.
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
