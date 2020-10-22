////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// Copyright (c) 2018-2020 Saxonica Limited
// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is "Incompatible With Secondary Licenses", as defined by the Mozilla Public License, v. 2.0.
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
package org.orbeon.saxon.tree.wrapper

import org.orbeon.saxon.om.NodeInfo


/**
  * This interface is implemented by NodeInfo implementations that act as wrappers
  * on some underlying tree. It provides a method to access the real node underlying
  * the virtual node, for use by applications that need to drill down to the
  * underlying data.
  */
trait VirtualNode extends NodeInfo {
  def getUnderlyingNode: AnyRef
  def getRealNode: AnyRef
}
