////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
package org.orbeon.saxon.tree.tiny

import org.orbeon.saxon.event.Receiver

import org.orbeon.saxon.event.ReceiverOption

import org.orbeon.saxon.model.Type

import org.orbeon.saxon.om.AtomicSequence

import org.orbeon.saxon.s9api.Location

import org.orbeon.saxon.trans.XPathException

import org.orbeon.saxon.value.UntypedAtomicValue

import TinyTextImpl._




object TinyTextImpl {

  def getStringValue(tree: TinyTree, nodeNr: Int): CharSequence = {
    val start: Int = tree.alpha(nodeNr)
    val len = tree.beta(nodeNr)
    tree.charBuffer.subSequence(start, start + len)
  }

}

class TinyTextImpl() extends TinyNodeImpl {

  def this (tree: TinyTree, nodeNr: Int) {
    this()
    this.tree = tree
    this.nodeNr = nodeNr
  }
  def getStringValue: String = getStringValueCS.toString

  override def getStringValueCS: CharSequence = {
    val start: Int = tree.alpha(nodeNr)
    val len = tree.beta(nodeNr)
    tree.charBuffer.subSequence(start, start + len)
  }

  def getNodeKind: Int = Type.TEXT

  override def copy(out: Receiver, copyOptions: Int, locationId: Location): Unit = {
    out.characters(getStringValueCS, locationId, ReceiverOption.NONE)
  }

  /*@NotNull*/

  def atomize(): AtomicSequence = new UntypedAtomicValue(getStringValueCS)

}

// Copyright (c) 2018-2020 Saxonica Limited
// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is "Incompatible With Secondary Licenses", as defined by the Mozilla Public License, v. 2.0.
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
/**
  * A node in the XML parse tree representing character content
  *
  * @author Michael H. Kay
  */
