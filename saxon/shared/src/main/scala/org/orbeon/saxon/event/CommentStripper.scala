////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// Copyright (c) 2018-2020 Saxonica Limited
// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is "Incompatible With Secondary Licenses", as defined by the Mozilla Public License, v. 2.0.
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
package org.orbeon.saxon.event

import org.orbeon.saxon.expr.parser.Loc
import org.orbeon.saxon.model.SchemaType
import org.orbeon.saxon.om.{AttributeMap, NamespaceMap, NodeName}
import org.orbeon.saxon.s9api.Location
import org.orbeon.saxon.tree.tiny.CompressedWhitespace
import org.orbeon.saxon.tree.util.FastStringBuffer


/**
  * The CommentStripper class is a filter that removes all comments and processing instructions.
  * It also concatenates text nodes that are split by comments and PIs. This follows the rules for
  * processing stylesheets; it is also used for removing comments and PIs from the tree seen
  * by XPath expressions used to process XSD 1.1 assertions
  */
class CommentStripper(next: Receiver) extends ProxyReceiver(next) {

  /*@Nullable*/
  private var savedWhitespace: CompressedWhitespace = null
  private val buffer: FastStringBuffer = new FastStringBuffer(FastStringBuffer.C256)

  override def startElement(elemName: NodeName,
                            `type`: SchemaType,
                            attributes: AttributeMap,
                            namespaces: NamespaceMap,
                            location: Location,
                            properties: Int): Unit = {
    flush()
    nextReceiver.startElement(elemName,
                              `type`,
                              attributes,
                              namespaces,
                              location,
                              properties)
  }

  override def endElement(): Unit = {
    flush()
    nextReceiver.endElement()
  }

  override def characters(chars: CharSequence,
                          locationId: Location,
                          properties: Int): Unit = {
    chars match {
      case whitespace: CompressedWhitespace =>
        if (buffer.isEmptySB && savedWhitespace == null)
          savedWhitespace = whitespace
        else
          whitespace.uncompress(buffer)
      case _                                =>
        if (savedWhitespace != null) {
          savedWhitespace.uncompress(buffer)
          savedWhitespace = null
        }
        buffer.cat(chars)
    }
  }

  override def comment(chars: CharSequence,
                       locationId: Location,
                       properties: Int): Unit = ()

  override def processingInstruction(name: String,
                                     data: CharSequence,
                                     locationId: Location,
                                     properties: Int): Unit = ()

  private def flush(): Unit = {
    if (! buffer.isEmptySB)
      nextReceiver.characters(buffer, Loc.NONE, ReceiverOption.NONE)
    else if (savedWhitespace != null)
      nextReceiver.characters(savedWhitespace, Loc.NONE, ReceiverOption.NONE)
    savedWhitespace = null
    buffer.setLength(0)
  }
}
