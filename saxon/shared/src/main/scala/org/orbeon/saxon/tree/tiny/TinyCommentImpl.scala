////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
package org.orbeon.saxon.tree.tiny

import org.orbeon.saxon.event.Receiver

import org.orbeon.saxon.event.ReceiverOption

import org.orbeon.saxon.model.Type

import org.orbeon.saxon.om.AtomicSequence

import org.orbeon.saxon.s9api.Location

import org.orbeon.saxon.trans.XPathException

import org.orbeon.saxon.value.StringValue




class TinyCommentImpl() extends TinyNodeImpl {

  def this(tree: TinyTree, nodeNr: Int) {
    this()
    this.tree = tree
    this.nodeNr = nodeNr
  }

  def getStringValue: String = {
    val start: Int = tree.alpha(nodeNr)
    val len = tree.beta(nodeNr)
    if (len == 0) return ""
    val dest: Array[Char] = Array.ofDim[Char](len)
    tree.commentBuffer.getChars(start, start + len, dest, 0)
    new String(dest, 0, len)
  }

  def atomize(): AtomicSequence = new StringValue(getStringValue)

  def getNodeKind: Int = Type.COMMENT

  override def copy(out: Receiver, copyOptions: Int, locationId: Location): Unit = {
    out.comment(getStringValue, locationId, ReceiverOption.NONE)
  }

}

// Copyright (c) 2018-2020 Saxonica Limited
// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is "Incompatible With Secondary Licenses", as defined by the Mozilla Public License, v. 2.0.
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
/**
  * TinyCommentImpl is an implementation of CommentInfo
  *
  * @author Michael H. Kay
  */
