////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
package net.sf.saxon.tree.linked

import net.sf.saxon.event.Receiver

import net.sf.saxon.event.ReceiverOption

import net.sf.saxon.model.Type

import net.sf.saxon.s9api.Location

import net.sf.saxon.trans.XPathException




class TextImpl(private var content: String) extends NodeImpl {

  def appendStringValue(content: String): Unit = {
    this.content = this.content + content
  }

  def getStringValue: String = content

  def getNodeKind(): Int = Type.TEXT

  override def copy(out: Receiver, copyOptions: Int, locationId: Location): Unit = {
    out.characters(content, locationId, ReceiverOption.NONE)
  }

  def replaceStringValue(stringValue: CharSequence): Unit = {
    if (stringValue.length == 0) {
      delete()
    } else {
      content = stringValue.toString
    }
  }

}

// Copyright (c) 2018-2020 Saxonica Limited
// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is "Incompatible With Secondary Licenses", as defined by the Mozilla Public License, v. 2.0.
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
/**
  * A node in the XML parse tree representing character content.
  *
  * @author Michael H. Kay
  */
