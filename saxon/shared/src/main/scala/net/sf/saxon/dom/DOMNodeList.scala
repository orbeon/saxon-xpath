////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
package net.sf.saxon.dom

import org.w3c.dom.Node

import java.util.List




class DOMNodeList(private var sequence: List[Node])
    extends org.w3c.dom.NodeList {

  def getLength(): Int = sequence.size

  /*@Nullable*/

  def item(index: Int): Node =
    if (index < 0 || index >= sequence.size) {
      null
    } else {
      sequence.get(index)
    }

}

// Copyright (c) 2018-2020 Saxonica Limited
// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is "Incompatible With Secondary Licenses", as defined by the Mozilla Public License, v. 2.0.
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
/**
  * This class wraps a list of nodes as a DOM NodeList
  */
