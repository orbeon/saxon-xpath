////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
package org.orbeon.saxon.tree.util

import org.orbeon.saxon.trans.SaxonErrorCode

import org.orbeon.saxon.trans.XPathException

import org.xml.sax.Attributes

import org.xml.sax.InputSource

import org.xml.sax.SAXException

import org.xml.sax.XMLReader

import org.xml.sax.helpers.XMLFilterImpl

import javax.xml.parsers.ParserConfigurationException

import javax.xml.parsers.SAXParser

import javax.xml.parsers.SAXParserFactory

import java.io.IOException

import java.io.StringReader

import java.util.ArrayList

import java.util.List




object ProcInstParser {

  /*@Nullable*/

  def getPseudoAttribute(content: String, name: String): String = {
    val result: List[String] = new ArrayList[String]()
    val filter: XMLFilterImpl = new XMLFilterImpl() {
      override def startElement(uri: String,
                                localName: String,
                                qName: String,
                                atts: Attributes): Unit = {
        var `val`: String = atts.getValue(name)
        if (`val` != null) {
          result.add(`val`)
        }
      }
    }
    val factory: SAXParserFactory = SAXParserFactory.newInstance()
// allows attribute names containing colons or unbound prefixes
    factory.setNamespaceAware(false)
    val parser: SAXParser = factory.newSAXParser()
    val reader: XMLReader = parser.getXMLReader
    reader.setContentHandler(filter)
    val in: StringReader = new StringReader("<e " + content + "/>")
    reader.parse(new InputSource(in))
    if (result.isEmpty) null else result.get(0)
  }

}

// Copyright (c) 2018-2020 Saxonica Limited
// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is "Incompatible With Secondary Licenses", as defined by the Mozilla Public License, v. 2.0.
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
/**
  * ProcInstParser is used to parse pseudo-attributes within Processing Instructions. This is a utility
  * class that is never instantiated.
  */
