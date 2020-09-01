////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
package net.sf.saxon.sapling

import net.sf.saxon.event.Receiver

import net.sf.saxon.event.ReceiverOption

import net.sf.saxon.expr.parser.Loc

import net.sf.saxon.model.Type

import net.sf.saxon.trans.XPathException

import java.util.Objects




class SaplingText(private var value: String) extends SaplingNode {

  Objects.requireNonNull(value)

  override def getNodeKind(): Int = Type.TEXT

  def getStringValue: String = value

   override def sendTo(receiver: Receiver): Unit = {
    receiver.characters(value, Loc.NONE, ReceiverOption.NONE)
  }

}

// Copyright (c) 2018-2020 Saxonica Limited
// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is "Incompatible With Secondary Licenses", as defined by the Mozilla Public License, v. 2.0.
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
