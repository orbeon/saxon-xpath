////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
package org.orbeon.saxon.tree.wrapper

import org.orbeon.saxon.om.NodeInfo




/**
  * Callback to create a VirtualNode that wraps a given NodeInfo
  */
trait WrappingFunction {

  /*@NotNull*/

  def makeWrapper(node: NodeInfo, parent: VirtualNode): VirtualNode

}

// Copyright (c) 2018-2020 Saxonica Limited
// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is "Incompatible With Secondary Licenses", as defined by the Mozilla Public License, v. 2.0.
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
