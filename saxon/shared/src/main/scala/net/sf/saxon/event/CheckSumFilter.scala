////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
package net.sf.saxon.event

import net.sf.saxon.model.SchemaType

import net.sf.saxon.om._

import net.sf.saxon.s9api.Location

import net.sf.saxon.trans.XPathException

import net.sf.saxon.value.Whitespace

import CheckSumFilter._

import scala.beans.{BeanProperty, BooleanBeanProperty}




object CheckSumFilter {

  private val DEBUG: Boolean = false

  val SIGMA: String = "Σ"

}

class CheckSumFilter(nextReceiver: Receiver)
    extends ProxyReceiver(nextReceiver) {

  @BeanProperty
  var checksum: Int = 0

  private var sequence: Int = 0

  private var checkExistingChecksum: Boolean = false

  private var checksumCorrect: Boolean = false

  @BooleanBeanProperty
  var checksumFound: Boolean = false

  def setCheckExistingChecksum(check: Boolean): Unit = {
    this.checkExistingChecksum = check
  }

  override def startDocument(properties: Int): Unit = {
    if (DEBUG) {
      System.err.println("CHECKSUM - START DOC")
    }
    super.startDocument(properties)
  }

  /**
    * Append an arbitrary item (node or atomic value) to the output
    * @param item           the item to be appended
    * @param locationId     the location of the calling instruction, for diagnostics
    * @param copyNamespaces if the item is an element node, this indicates whether its namespaces
    *                       need to be copied. Values are {@link ReceiverOption#ALL_NAMESPACES};
    *                            the default (0) means
    */
  override def append(item: Item,
                      locationId: Location,
                      copyNamespaces: Int): Unit = {
    checksum ^= hash(item.toString, { sequence += 1; sequence - 1 })
    if (DEBUG) {
      System.err.println(
        "After append: " + java.lang.Integer.toHexString(checksum))
    }
    super.append(item, locationId, copyNamespaces)
  }

  /**
    * Character data
    */
  override def characters(chars: CharSequence,
                          locationId: Location,
                          properties: Int): Unit = {
    if (!Whitespace.isWhite(chars)) {
      checksum ^= hash(chars, { sequence += 1; sequence - 1 })
      if (DEBUG) {
        System.err.println(
          "After characters " + chars + ": " + java.lang.Integer
            .toHexString(checksum))
      }
    }
    super.characters(chars, locationId, properties)
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
    checksum ^= hash(elemName, { sequence += 1; sequence - 1 })
    if (DEBUG) {
      System.err.println(
        "After startElement " + elemName.getDisplayName + ": " +
          checksum)
    }
    checksumCorrect = false
    for (att <- attributes) {
      checksum ^= hash(att.getNodeName, sequence)
      checksum ^= hash(att.getValue, sequence)
      if (DEBUG) {
        System.err.println(
          "After attribute " + att.getNodeName.getDisplayName +
            ": " +
            checksum +
            "(" +
            hash(att.getNodeName, sequence) +
            "," +
            hash(att.getValue, sequence) +
            ")")
      }
    }
    super.startElement(elemName,
                       `type`,
                       attributes,
                       namespaces,
                       location,
                       properties)
  }

  /**
    * End of element
    */
  override def endElement(): Unit = {
    checksum ^= 1
    if (DEBUG) {
      System.err.println("After endElement: " + checksum)
    }
    super.endElement()
  }

  /**
    * Processing Instruction
    */
  override def processingInstruction(target: String,
                                     data: CharSequence,
                                     locationId: Location,
                                     properties: Int): Unit = {
    if (target == SIGMA) {
      checksumFound = true
      if (checkExistingChecksum) {
        try {
          val found: Int = java.lang.Long.parseLong("0" + data, 16).toInt
          checksumCorrect = found == checksum
        } catch {
          case e: NumberFormatException => checksumCorrect = false

        }
      }
    }
    super.processingInstruction(target, data, locationId, properties)
  }

  def isChecksumCorrect: Boolean =
    checksumCorrect || "skip" == System.getProperty("saxon-checksum")

  private def hash(s: CharSequence, sequence: Int): Int = {
    var h: Int = sequence << 8
    for (i <- 0 until s.length) {
      h = (h << 1) + s.charAt(i)
    }
    h
  }

  private def hash(n: NodeName, sequence: Int): Int = //System.err.println("hash(" + n.getLocalPart + ") " + hash(n.getLocalPart, sequence) + "/" + hash(n.getURI, sequence));
    hash(n.getLocalPart, sequence) ^ hash(n.getURI, sequence)

}

// Copyright (c) 2018-2020 Saxonica Limited
// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is "Incompatible With Secondary Licenses", as defined by the Mozilla Public License, v. 2.0.
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
/**
  * A filter to go on a Receiver pipeline and calculate a checksum of the data passing through the pipeline.
  * Optionally the filter will also check any checksum (represented by a processing instruction with name
  * SIGMA) found in the file.
  *
  * <p>The checksum takes account of element, attribute, and text nodes only. The order of attributes
  * within an element makes no difference.</p>
  */
