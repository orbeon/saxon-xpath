////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
package org.orbeon.saxon.sapling

import org.orbeon.saxon.event.Receiver

import org.orbeon.saxon.event.ReceiverOption

import org.orbeon.saxon.expr.parser.Loc

import org.orbeon.saxon.model.Type

import org.orbeon.saxon.trans.XPathException

import java.util.Objects

import scala.beans.{BeanProperty, BooleanBeanProperty}




class SaplingProcessingInstruction(@BeanProperty var name: String,
                                   private var value: String)
    extends SaplingNode {

  Objects.requireNonNull(name)

  Objects.requireNonNull(value)

  def getStringValue: String = value

  override def getNodeKind: Int = Type.PROCESSING_INSTRUCTION

   override def sendTo(receiver: Receiver): Unit = {
    receiver.processingInstruction(name, value, Loc.NONE, ReceiverOption.NONE)
  }

}

// Copyright (c) 2018-2020 Saxonica Limited
// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is "Incompatible With Secondary Licenses", as defined by the Mozilla Public License, v. 2.0.
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
/**
  * A processing-instruction node in a sapling tree
  */
