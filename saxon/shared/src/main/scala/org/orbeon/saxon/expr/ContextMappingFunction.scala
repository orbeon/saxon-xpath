////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
package org.orbeon.saxon.expr

import org.orbeon.saxon.om.SequenceIterator

import org.orbeon.saxon.trans.XPathException




trait ContextMappingFunction {

  def map(context: XPathContext): SequenceIterator

}

// Copyright (c) 2018-2020 Saxonica Limited
// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is "Incompatible With Secondary Licenses", as defined by the Mozilla Public License, v. 2.0.
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
/**
  * ContextMappingFunction is an interface that must be satisfied by an object passed to a
  * ContextMappingIterator. It represents an object which, given an Item, can return a
  * SequenceIterator that delivers a sequence of zero or more Items.
  * <p>This is a specialization of the more general MappingFunction class: it differs in that
  * each item being processed becomes the context item while it is being processed.</p>
  *
  */
