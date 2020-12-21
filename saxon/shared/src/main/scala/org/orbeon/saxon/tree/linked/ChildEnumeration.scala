////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// Copyright (c) 2018-2020 Saxonica Limited
// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is "Incompatible With Secondary Licenses", as defined by the Mozilla Public License, v. 2.0.
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
package org.orbeon.saxon.tree.linked

import java.util.function.Predicate

import org.orbeon.saxon.om.NodeInfo


class ChildEnumeration(node: NodeImpl, nodeTest: Predicate[_ >: NodeInfo])
    extends TreeEnumeration(node, nodeTest) {

  nextImpl = node.getFirstChild

  while (! conforms(nextImpl))
    step()

   def step(): Unit =
    nextImpl = nextImpl.getNextSibling
}
