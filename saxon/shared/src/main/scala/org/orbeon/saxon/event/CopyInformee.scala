////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
package org.orbeon.saxon.event

import org.orbeon.saxon.om.NodeInfo




/**
  * A CopyInformee is an agent that receives extra information while a tree is being copied. Specifically,
  * each time an element node is copied to the receiver, before calling the startElement() method, the copying
  * code will first call notifyElementNode(), giving the informee extra information about the element currently
  * being copied.
  */
trait CopyInformee[T <: AnyRef] {

  def notifyElementNode(element: NodeInfo): T

}

// Copyright (c) 2018-2020 Saxonica Limited
// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is "Incompatible With Secondary Licenses", as defined by the Mozilla Public License, v. 2.0.
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
