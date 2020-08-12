////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
package net.sf.saxon.event

import net.sf.saxon.model.SchemaType

import net.sf.saxon.model.SimpleType

import net.sf.saxon.om.Item

import net.sf.saxon.om.NodeName

import net.sf.saxon.s9api.Location

import net.sf.saxon.trans.XPathException

import OutputterEvent._




object OutputterEvent {

  class StartDocument(var properties: Int) extends OutputterEvent {

    override def replay(out: Outputter): Unit = {
      out.startDocument(properties)
    }

  }

  class EndDocument extends OutputterEvent {

    override def replay(out: Outputter): Unit = {
      out.endDocument()
    }

  }

  class StartElement(var name: NodeName,
                     var `type`: SchemaType,
                     var location: Location,
                     var properties: Int)
      extends OutputterEvent {

    override def replay(out: Outputter): Unit = {
      out.startElement(name, `type`, location, properties)
    }

  }

  class Attribute(var name: NodeName,
                  var `type`: SimpleType,
                  var value: String,
                  var location: Location,
                  var properties: Int)
      extends OutputterEvent {

    override def replay(out: Outputter): Unit = {
      out.attribute(name, `type`, value, location, properties)
    }

  }

  class Namespace(var prefix: String, var uri: String, var properties: Int)
      extends OutputterEvent {

    override def replay(out: Outputter): Unit = {
      out.namespace(prefix, uri, properties)
    }

  }

  class StartContent extends OutputterEvent {

    override def replay(out: Outputter): Unit = {
      out.startContent()
    }

  }

  class EndElement extends OutputterEvent {

    override def replay(out: Outputter): Unit = {
      out.endElement()
    }

  }

  class Text(content: CharSequence,
             var location: Location,
             var properties: Int)
      extends OutputterEvent {

    var content: String = content.toString

    override def replay(out: Outputter): Unit = {
      out.characters(content, location, properties)
    }

  }

  class Comment(content: CharSequence,
                var location: Location,
                var properties: Int)
      extends OutputterEvent {

    var content: String = content.toString

    override def replay(out: Outputter): Unit = {
      out.comment(content, location, properties)
    }

  }

  class ProcessingInstruction(var target: String,
                              content: CharSequence,
                              var location: Location,
                              var properties: Int)
      extends OutputterEvent {

    var content: String = content.toString

    override def replay(out: Outputter): Unit = {
      out.processingInstruction(target, content, location, properties)
    }

  }

  class Append(var item: Item, var location: Location, var properties: Int)
      extends OutputterEvent {

    override def replay(out: Outputter): Unit = {
      out.append(item, location, properties)
    }

  }

}

/**
  * An outputter event is an object representing one of the events that can be passed to an {@link Outputter} :
  * for example, a startElement, attribute, namespace
  * endElement, characters, or comment event. Sufficient information is retained in order to enable a stored event to
  * be "replayed" later.
  */
abstract class OutputterEvent {

  def replay(out: Outputter): Unit = {}

}

// Copyright (c) 2018-2020 Saxonica Limited
// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is "Incompatible With Secondary Licenses", as defined by the Mozilla Public License, v. 2.0.
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
