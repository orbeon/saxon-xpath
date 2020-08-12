////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
package net.sf.saxon.event

import net.sf.saxon.model.SchemaType

import net.sf.saxon.om.AttributeMap

import net.sf.saxon.om.Item

import net.sf.saxon.om.NamespaceMap

import net.sf.saxon.om.NodeName

import net.sf.saxon.s9api.Location

import net.sf.saxon.trans.XPathException

import Event._




object Event {

  class StartDocument(var properties: Int) extends Event {

    override def replay(out: Receiver): Unit = {
      out.startDocument(properties)
    }

  }

  class EndDocument extends Event {

    override def replay(out: Receiver): Unit = {
      out.endDocument()
    }

  }

  class StartElement(var name: NodeName,
                     var `type`: SchemaType,
                     var attributes: AttributeMap,
                     var namespaces: NamespaceMap,
                     var location: Location,
                     var properties: Int)
      extends Event {

    override def replay(out: Receiver): Unit = {
      out.startElement(name,
                       `type`,
                       attributes,
                       namespaces,
                       location,
                       properties)
    }

    def replay(out: Receiver, newProps: Int): Unit = {
      out.startElement(name,
                       `type`,
                       attributes,
                       namespaces,
                       location,
                       newProps)
    }

    def getProperties(): Int = properties

  }

  class EndElement extends Event {

    override def replay(out: Receiver): Unit = {
      out.endElement()
    }

  }

  class Text(content: CharSequence,
             var location: Location,
             var properties: Int)
      extends Event {

    var content: String = content.toString

    override def replay(out: Receiver): Unit = {
      out.characters(content, location, properties)
    }

  }

  class Comment(content: CharSequence,
                var location: Location,
                var properties: Int)
      extends Event {

    var content: String = content.toString

    override def replay(out: Receiver): Unit = {
      out.comment(content, location, properties)
    }

  }

  class ProcessingInstruction(var target: String,
                              content: CharSequence,
                              var location: Location,
                              var properties: Int)
      extends Event {

    var content: String = content.toString

    override def replay(out: Receiver): Unit = {
      out.processingInstruction(target, content, location, properties)
    }

  }

  class Append(var item: Item, var location: Location, var properties: Int)
      extends Event {

    override def replay(out: Receiver): Unit = {
      out.append(item, location, properties)
    }

  }

}

/**
  * An event is an object representing one of the events that can be passed to a receiver: for example, a startElement,
  * endElement, characters, or comment event. Sufficient information is retained in order to enable a stored event to
  * be "replayed" later.
  */
abstract class Event {

  def replay(out: Receiver): Unit = {}

}

// Copyright (c) 2018-2020 Saxonica Limited
// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is "Incompatible With Secondary Licenses", as defined by the Mozilla Public License, v. 2.0.
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
