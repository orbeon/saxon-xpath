////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
package net.sf.saxon.expr

import net.sf.saxon.om.Item

import net.sf.saxon.trans.XPathException




trait ItemMappingFunction {

  /*@Nullable*/

  def mapItem(item: Item): Item

}

// Copyright (c) 2018-2020 Saxonica Limited
// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is "Incompatible With Secondary Licenses", as defined by the Mozilla Public License, v. 2.0.
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
/**
  * ItemMappingFunction is an interface that must be satisfied by an object passed to a
  * ItemMappingIterator. It represents an object which, given an Item, can return either
  * another Item, or null.
  */
