////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
package org.orbeon.saxon.expr

import org.orbeon.saxon.om.SequenceIterator

import org.orbeon.saxon.regex.RegexIterator

import org.orbeon.saxon.trans.XPathException

import org.orbeon.saxon.tree.iter.EmptyIterator




class AnalyzeMappingFunction(private var base: RegexIterator,
                             private var c2: XPathContext,
                             private var nonMatchExpr: Expression,
                             private var matchingExpr: Expression)
    extends ContextMappingFunction {

  def map(context: XPathContext): SequenceIterator = {
    if (base.isMatching) {
      if (matchingExpr != null) {
        matchingExpr.iterate(c2)
      }
    } else {
      if (nonMatchExpr != null) {
        nonMatchExpr.iterate(c2)
      }
    }
    EmptyIterator.getInstance
  }

}

// Copyright (c) 2018-2020 Saxonica Limited
// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is "Incompatible With Secondary Licenses", as defined by the Mozilla Public License, v. 2.0.
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
/**
  * Mapping function that maps the sequence of matching/non-matching strings to the
  * sequence delivered by applying the matching-substring and non-matching-substring
  * expressions respectively to each such string
  */
