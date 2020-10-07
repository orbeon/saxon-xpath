////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
package org.orbeon.saxon.functions

import org.orbeon.saxon.expr.XPathContext

import org.orbeon.saxon.lib.StringCollator

import org.orbeon.saxon.om.Item

import org.orbeon.saxon.om.Sequence

import org.orbeon.saxon.om.SequenceIterator

import org.orbeon.saxon.trans.XPathException

import org.orbeon.saxon.tree.iter.UnfailingIterator

import org.orbeon.saxon.value.BooleanValue

import org.orbeon.saxon.value.StringValue

import org.orbeon.saxon.value.Whitespace

import ContainsToken._


object ContainsToken {

  private def containsToken(arg0: SequenceIterator,
                            arg1: StringValue,
                            collator: StringCollator): Boolean = {
    if (arg1 == null) {
      return false
    }
    val search: String = Whitespace.trim(arg1.getPrimitiveStringValue.toString)
    if (search.isEmpty) {
      return false
    }
    var item: Item = null
    while (({
      item = arg0.next()
      item
    }) != null) {
      val tokens: UnfailingIterator =
        new Whitespace.Tokenizer(item.getStringValueCS)
      var token: Item = null
      while (({
        token = tokens.next()
        token
      }) != null) if (collator.comparesEqual(
        search,
        token.getStringValue)) {
        tokens.close()
        arg0.close()
        true
      }
    }
    false
  }

}

/**
 * Implements the fn:contains-token() function with the collation already bound.
 * This function was introduced in XPath 3.1
 */
class ContainsToken extends CollatingFunctionFixed {

  override def isSubstringMatchingFunction(): Boolean = true

  def call(context: XPathContext, arguments: Array[Sequence]): BooleanValue =
    BooleanValue.get(
      containsToken(arguments(0).iterate(),
        arguments(1).head.asInstanceOf[StringValue],
        getStringCollator))

}

// Copyright (c) 2018-2020 Saxonica Limited
// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is "Incompatible With Secondary Licenses", as defined by the Mozilla Public License, v. 2.0.
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
