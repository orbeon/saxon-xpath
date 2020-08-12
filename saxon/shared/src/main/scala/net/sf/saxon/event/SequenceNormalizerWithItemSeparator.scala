////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
package net.sf.saxon.event

import net.sf.saxon.expr.parser.Loc

import net.sf.saxon.ma.arrays.ArrayItem

import net.sf.saxon.model.SchemaType

import net.sf.saxon.om.AttributeMap

import net.sf.saxon.om.Item

import net.sf.saxon.om.NamespaceMap

import net.sf.saxon.om.NodeName

import net.sf.saxon.s9api.Location

import net.sf.saxon.trans.XPathException

import net.sf.saxon.value.AtomicValue




class SequenceNormalizerWithItemSeparator(next: Receiver,
                                          private var separator: String)
    extends SequenceNormalizer(next) {

  private var first: Boolean = true

  /**
    * Start of event stream
    */
  override def open(): Unit = {
    first = true
    super.open()
  }

  /**
    * Start of a document node.
    */
  override def startDocument(properties: Int): Unit = {
    sep()
    super.startDocument(properties)
  }

  /**
    * Notify the start of an element
    */
  override def startElement(elemName: NodeName,
                            `type`: SchemaType,
                            attributes: AttributeMap,
                            namespaces: NamespaceMap,
                            location: Location,
                            properties: Int): Unit = {
    sep()
    super.startElement(elemName,
                       `type`,
                       attributes,
                       namespaces,
                       location,
                       properties)
  }

  /**
    * Character data
    */
  override def characters(chars: CharSequence,
                          locationId: Location,
                          properties: Int): Unit = {
    sep()
    super.characters(chars, locationId, properties)
  }

  /**
    * Processing Instruction
    */
  override def processingInstruction(target: String,
                                     data: CharSequence,
                                     locationId: Location,
                                     properties: Int): Unit = {
    sep()
    super.processingInstruction(target, data, locationId, properties)
  }

  /**
    * Output a comment
    */
  override def comment(chars: CharSequence,
                       locationId: Location,
                       properties: Int): Unit = {
    sep()
    super.comment(chars, locationId, properties)
  }

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
    if (item.isInstanceOf[ArrayItem]) {
      flatten(item.asInstanceOf[ArrayItem], locationId, copyNamespaces)
    } else {
      if (item.isInstanceOf[AtomicValue]) {
        sep()
        nextReceiver.characters(item.getStringValueCS,
                                locationId,
                                ReceiverOption.NONE)
      } else {
        decompose(item, locationId, copyNamespaces)
      }
    }
  }

  /**
    * End of output. Note that closing this receiver also closes the rest of the
    * pipeline.
    */
  override def close(): Unit = {
//getNextReceiver().endDocument();
    super.close()
  }

  private def sep(): Unit = {
    if (level == 0 && !first) {
      super.characters(separator, Loc.NONE, ReceiverOption.NONE)
    } else {
      first = false
    }
  }

}

// Copyright (c) 2018-2020 Saxonica Limited
// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is "Incompatible With Secondary Licenses", as defined by the Mozilla Public License, v. 2.0.
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
/**
  * Implement the "sequence normalization" logic as defined in the XSLT 3.0/XQuery 3.0
  * serialization spec.
  *
  * <p>This class is used only if an ItemSeparator is specified. In the absence of an ItemSeparator,
  * the insertion of a single space performed by the ComplexContentOutputter serves the purpose.</p>
  */
