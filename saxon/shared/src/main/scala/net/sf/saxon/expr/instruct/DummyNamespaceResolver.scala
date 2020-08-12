////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
package net.sf.saxon.expr.instruct

import net.sf.saxon.lib.NamespaceConstant

import net.sf.saxon.om.NamespaceResolver

import net.sf.saxon.tree.jiter.PairIterator

import java.util.Iterator

import DummyNamespaceResolver._




object DummyNamespaceResolver {

  private val THE_INSTANCE: DummyNamespaceResolver =
    new DummyNamespaceResolver()

  def getInstance(): DummyNamespaceResolver = THE_INSTANCE

}

class DummyNamespaceResolver private () extends NamespaceResolver {

  def getURIForPrefix(prefix: String, useDefault: Boolean): String =
    if (prefix.isEmpty) {
      NamespaceConstant.NULL
    } else if ("xml" == prefix) {
      NamespaceConstant.XML
    } else {
// this is a dummy namespace resolver, we don't actually know the URI
      NamespaceConstant.NULL
    }

  def iteratePrefixes(): Iterator[String] = new PairIterator[String]("", "xml")

}

// Copyright (c) 2018-2020 Saxonica Limited
// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is "Incompatible With Secondary Licenses", as defined by the Mozilla Public License, v. 2.0.
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
/**
  * A dummy namespace resolver used when validating QName-valued attributes written to
  * the result tree. The namespace node might be created after the initial validation
  * of the attribute, so in the first round of validation we only check the lexical form
  * of the value, and we defer prefix checks until later.
  */
