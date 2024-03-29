////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
package org.orbeon.saxon.dom

import NodeOverNodeInfo._

import org.w3c.dom.ProcessingInstruction




class PIOverNodeInfo extends NodeOverNodeInfo with ProcessingInstruction {

  /**
    * The target of this processing instruction. XML defines this as being
    * the first token following the markup that begins the processing
    * instruction.
    */
  def getTarget(): String = node.getLocalPart

  /**
    * The content of this processing instruction. This is from the first non
    * white space character after the target to the character immediately
    * preceding the <code>?&gt;</code>.
    */
  def getData(): String = node.getStringValue

  /**
    * The content of this processing instruction. This is from the first non
    * white space character after the target to the character immediately
    * preceding the <code>?&gt;</code>.
    *
    * @throws org.w3c.dom.DOMException NO_MODIFICATION_ALLOWED_ERR: Raised when the node is readonly.
    */
  def setData(data: String): Unit = {
    disallowUpdate()
  }

}

// Copyright (c) 2018-2020 Saxonica Limited
// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is "Incompatible With Secondary Licenses", as defined by the Mozilla Public License, v. 2.0.
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
/**
  * This class is an implementation of the DOM ProcessingInstruction interface that wraps a Saxon NodeInfo
  * representation of a text or comment node.
  */
