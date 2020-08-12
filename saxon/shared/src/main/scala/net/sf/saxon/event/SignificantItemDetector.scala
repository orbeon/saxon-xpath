////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// Copyright (c) 2018-2020 Saxonica Limited
// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is "Incompatible With Secondary Licenses", as defined by the Mozilla Public License, v. 2.0.
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
package net.sf.saxon.event

import net.sf.saxon.ma.arrays.ArrayItem
import net.sf.saxon.model.SchemaType
import net.sf.saxon.model.SimpleType
import net.sf.saxon.model.Type
import net.sf.saxon.om._
import net.sf.saxon.s9api.Location
import net.sf.saxon.trans.XPathException
import net.sf.saxon.value.AtomicValue

/**
 * This receiver is inserted into the output pipeline whenever on-empty or on-non-empty is used (XSLT 3.0).
 * It passes all events to the underlying receiver unchanged, but invokes a callback action when the
 * first item is written.
 */
object SignificantItemDetector {
  def isSignificant(item: Item): Boolean = {
    if (item.isInstanceOf[NodeInfo]) {
      val node = item.asInstanceOf[NodeInfo]
      return (node.getNodeKind != Type.TEXT || !node.getStringValue.isEmpty) && (node.getNodeKind != Type.DOCUMENT || node.hasChildNodes)
    }
    else if (item.isInstanceOf[AtomicValue]) return !item.getStringValue.isEmpty
    else if (item.isInstanceOf[ArrayItem]) if (item.asInstanceOf[ArrayItem].isEmpty) return true
    else {

      for (mem <- item.asInstanceOf[ArrayItem].members) {
        try {
          val memIter = mem.iterate
          var it: Item = null
          while ( {
            (it = memIter.next) != null
          }) if (isSignificant(it)) return true
        } catch {
          case e: XPathException =>
            return true
        }
      }
      return false
    }
    true
  }
}

class SignificantItemDetector(val next: Outputter, var trigger: Action) extends ProxyOutputter(next) {
  private var level = 0
  private var empty = true

  @throws[XPathException]
  private def start() = if ( /*level==0 && */ empty ) {
    trigger.doAction()
    empty = false
  }

  /**
   * Start of a document node.
   */
  @throws[XPathException]
  override def startDocument(properties: Int) = if ( {
    level += 1; level - 1
  } != 0) getNextOutputter.startDocument(properties)

  @throws[XPathException]
  override def startElement(elemName: NodeName, `type`: SchemaType, location: Location, properties: Int) = {
    start()
    level += 1
    getNextOutputter.startElement(elemName, `type`, location, properties)
  }

  /**
   * Notify the start of an element, supplying all attributes and namespaces
   */
  @throws[XPathException]
  override def startElement(elemName: NodeName, `type`: SchemaType, attributes: AttributeMap, namespaces: NamespaceMap, location: Location, properties: Int) = {
    start()
    level += 1
    getNextOutputter.startElement(elemName, `type`, attributes, namespaces, location, properties)
  }

  /**
   * Notify a namespace binding.
   */
  @throws[XPathException]
  override def namespace(prefix: String, namespaceUri: String, properties: Int) = getNextOutputter.namespace(prefix, namespaceUri, properties)

  /**
   * Notify an attribute.
   */
  @throws[XPathException]
  override def attribute(attName: NodeName, typeCode: SimpleType, value: CharSequence, location: Location, properties: Int) = getNextOutputter.attribute(attName, typeCode, value, location, properties)

  /**
   * Notify the start of the content, that is, the completion of all attributes and namespaces.
   */
  @throws[XPathException]
  override def startContent() = getNextOutputter.startContent()

  @throws[XPathException]
  override def characters(chars: CharSequence, locationId: Location, properties: Int) = {
    if (chars.length > 0) start()
    getNextOutputter.characters(chars, locationId, properties)
  }

  @throws[XPathException]
  override def processingInstruction(target: String, data: CharSequence, locationId: Location, properties: Int) = {
    start()
    getNextOutputter.processingInstruction(target, data, locationId, properties)
  }

  @throws[XPathException]
  override def comment(chars: CharSequence, locationId: Location, properties: Int) = {
    start()
    getNextOutputter.comment(chars, locationId, properties)
  }

  @throws[XPathException]
  override def append(item: Item, locationId: Location, copyNamespaces: Int) = {
    if (SignificantItemDetector.isSignificant(item)) start()
    super.append(item, locationId, copyNamespaces)
  }

  /**
   * Notify the end of a document node
   */
  @throws[XPathException]
  override def endDocument() = if ( {
    level -= 1; level
  } != 0) getNextOutputter.endDocument()

  /**
   * End of element
   */
  @throws[XPathException]
  override def endElement() = {
    level -= 1
    getNextOutputter.endElement()
  }

  /**
   * Ask if the sequence that has been written so far is considered empty
   *
   * @return true if no significant items have been written (or started)
   */
  def isEmpty = empty
}