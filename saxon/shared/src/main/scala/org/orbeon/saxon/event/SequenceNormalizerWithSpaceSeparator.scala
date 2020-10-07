////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
package org.orbeon.saxon.event

import org.orbeon.saxon.om.Item

import org.orbeon.saxon.s9api.Location

import org.orbeon.saxon.trans.XPathException




class SequenceNormalizerWithSpaceSeparator(next: Receiver)
    extends SequenceNormalizer(next) {

  /**
    * Append an arbitrary item (node or atomic value) to the output
    * @param item           the item to be appended
    * @param locationId     the location of the calling instruction, for diagnostics
    * @param copyNamespaces if the item is an element node, this indicates whether its namespaces
    *                       need to be copied. Values are {@link ReceiverOption#ALL_NAMESPACES}; the default (0) means
    */
  override def append(item: Item,
                      locationId: Location,
                      copyNamespaces: Int): Unit = {
    decompose(item, locationId, copyNamespaces)
  }

   override def getErrorCodeForDecomposingFunctionItems(): String =
    "SENR0001"

}

// Copyright (c) 2018-2020 Saxonica Limited
// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is "Incompatible With Secondary Licenses", as defined by the Mozilla Public License, v. 2.0.
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
/**
  * Implement the "sequence normalization" logic as defined in the XSLT 3.0/XQuery 3.0
  * serialization spec, for the case where there is no item-separator.
  *
  * <p>This class is used only if no ItemSeparator is specified. Its effect is to insert
  * a single space as a separator between adjacent atomic values appearing in the top level
  * sequence.</p>
  */
