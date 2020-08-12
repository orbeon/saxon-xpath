////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
package net.sf.saxon.sapling

import net.sf.saxon.s9api.QName




object Saplings {

  def doc(): SaplingDocument = new SaplingDocument()

  def doc(baseUri: String): SaplingDocument = new SaplingDocument(baseUri)

  def elem(name: String): SaplingElement = new SaplingElement(name)

  def elem(qName: QName): SaplingElement = new SaplingElement(qName)

  def text(value: String): SaplingText = new SaplingText(value)

  def comment(value: String): SaplingComment = new SaplingComment(value)

  def pi(target: String, data: String): SaplingProcessingInstruction =
    new SaplingProcessingInstruction(target, data)

}

// Copyright (c) 2018-2020 Saxonica Limited
// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is "Incompatible With Secondary Licenses", as defined by the Mozilla Public License, v. 2.0.
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
/**
  * This is a non-instantiable class holding a number of convenience methods for creating
  * sapling nodes of different kinds.
  *
  * @see SaplingNode
  */
