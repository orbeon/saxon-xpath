////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
package org.orbeon.saxon.tree.tiny

import org.orbeon.saxon.model.Type

import org.orbeon.saxon.om.NodeInfo

import org.orbeon.saxon.pattern.NodeTest




/**
  * Interface defining methods common to the TinyTree and the Domino tree model. These two models are
  * recognized by the {@link NodeTest} class, which is able to match nodes without actually instantiating
  * the NodeInfo object
  */
trait NodeVectorTree {

  /**
    * Ask whether the tree contains non-trivial type information (from schema validation)
    * @return true if type information is present
    */
  def isTyped: Boolean = false

  def getNode(nodeNr: Int): NodeInfo

  def getNodeKind(nodeNr: Int): Int

  def getFingerprint(nodeNr: Int): Int

  def getNodeKindArray: Array[Byte]

  def getNameCodeArray: Array[Int]

}

// Copyright (c) 2018-2020 Saxonica Limited
// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is "Incompatible With Secondary Licenses", as defined by the Mozilla Public License, v. 2.0.
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// Copyright (c) 2018-2020 Saxonica Limited
