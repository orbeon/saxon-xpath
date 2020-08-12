////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
package net.sf.saxon.functions

import net.sf.saxon.expr.XPathContext

import net.sf.saxon.lib.StringCollator

import net.sf.saxon.om.Item

import net.sf.saxon.om.Sequence

import net.sf.saxon.om.SequenceIterator

import net.sf.saxon.trans.XPathException

import net.sf.saxon.tree.iter.UnfailingIterator

import net.sf.saxon.value.BooleanValue

import net.sf.saxon.value.StringValue

import net.sf.saxon.value.Whitespace

import ContainsToken._




object ContainsToken {

  private def containsToken(arg0: SequenceIterator,
                            arg1: StringValue,
                            collator: StringCollator): Boolean = {
    if (arg1 == null) {
      false
    }
    val search: String = Whitespace.trim(arg1.getPrimitiveStringValue.toString)
    if (search.isEmpty) {
      false
    }
    var item: Item = null
    while ((item = arg0.next()) != null) {
      val tokens: UnfailingIterator =
        new Whitespace.Tokenizer(item.getStringValueCS)
      var token: Item = null
      while ((token = tokens.next()) != null) if (collator.comparesEqual(
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
                    arguments(1).head().asInstanceOf[StringValue],
                    getStringCollator))

}

// Copyright (c) 2018-2020 Saxonica Limited
// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is "Incompatible With Secondary Licenses", as defined by the Mozilla Public License, v. 2.0.
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
