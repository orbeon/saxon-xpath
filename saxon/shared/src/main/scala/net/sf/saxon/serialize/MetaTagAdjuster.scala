////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
package net.sf.saxon.serialize

import net.sf.saxon.event.ProxyReceiver

import net.sf.saxon.event.Receiver

import net.sf.saxon.event.ReceiverOption

import net.sf.saxon.expr.parser.Loc

import net.sf.saxon.lib.NamespaceConstant

import net.sf.saxon.lib.SaxonOutputKeys

import net.sf.saxon.model.BuiltInAtomicType

import net.sf.saxon.model.SchemaType

import net.sf.saxon.model.Untyped

import net.sf.saxon.om._

import net.sf.saxon.s9api.Location

import net.sf.saxon.trans.XPathException

import net.sf.saxon.value.Whitespace

import javax.xml.transform.OutputKeys

import java.util.Properties

import scala.util.control.Breaks._


class MetaTagAdjuster(next: Receiver) extends ProxyReceiver(next) {

  private var seekingHead: Boolean = true

  private var droppingMetaTags: Int = -1

  private var inMetaTag: Boolean = false

  var encoding: String = _

  private var mediaType: String = _

  private var level: Int = 0

  private var isXHTML: Boolean = false

  private var htmlVersion: Int = 4

  def setOutputProperties(details: Properties): Unit = {
    encoding = details.getProperty(OutputKeys.ENCODING)
    if (encoding == null) {
      encoding = "UTF-8"
    }
    mediaType = details.getProperty(OutputKeys.MEDIA_TYPE)
    if (mediaType == null) {
      mediaType = "text/html"
    }
    var htmlVn: String = details.getProperty(SaxonOutputKeys.HTML_VERSION)
    if (htmlVn == null && !isXHTML) {
      htmlVn = details.getProperty(OutputKeys.VERSION)
    }
    if (htmlVn != null && htmlVn.startsWith("5")) {
      htmlVersion = 5
    }
  }

  def setIsXHTML(xhtml: Boolean): Unit = {
    isXHTML = xhtml
  }

  private def comparesEqual(name1: String, name2: String): Boolean =
    if (isXHTML) {
      name1 == name2
    } else {
      name1.equalsIgnoreCase(name2)
    }

  private def matchesName(name: NodeName, local: String): Boolean =
    if (isXHTML) {
      if (name.getLocalPart != local) {
        return false
      }
      if (htmlVersion == 5) {
        name.hasURI("") || name.hasURI(NamespaceConstant.XHTML)
      } else {
        name.hasURI(NamespaceConstant.XHTML)
      }
    } else {
      name.getLocalPart.equalsIgnoreCase(local)
    }

  override def startElement(elemName: NodeName,
                            `type`: SchemaType,
                            attributes: AttributeMap,
                            namespaces: NamespaceMap,
                            location: Location,
                            properties: Int): Unit = {
    if (droppingMetaTags == level) {
      if (matchesName(elemName, "meta")) {
        // if there was an http-equiv="ContentType" attribute, discard the meta element entirely
        var found: Boolean = false
        breakable {
          for (att <- attributes) {
            val name: String = att.getNodeName.getLocalPart
            if (comparesEqual(name, "http-equiv")) {
              val value: String = Whitespace.trim(att.getValue)
              if (value.equalsIgnoreCase("Content-Type")) {
                // case-blind comparison even for XHTML
                found = true
                break()
              }
            }
          }
        }
        inMetaTag = found
        if (found) {
          return
        }
      }
    }
    level += 1
    nextReceiver.startElement(elemName,
      `type`,
      attributes,
      namespaces,
      location,
      properties)
    if (seekingHead && matchesName(elemName, "head")) {
      val headPrefix: String = elemName.getPrefix
      val headURI: String = elemName.getURI
      val metaCode: FingerprintedQName =
        new FingerprintedQName(headPrefix, headURI, "meta")
      var atts: AttributeMap = EmptyAttributeMap.getInstance
      atts = atts.put(
        new AttributeInfo(new NoNamespaceName("http-equiv"),
          BuiltInAtomicType.UNTYPED_ATOMIC,
          "Content-Type",
          Loc.NONE,
          ReceiverOption.NONE))
      atts = atts.put(
        new AttributeInfo(new NoNamespaceName("content"),
          BuiltInAtomicType.UNTYPED_ATOMIC,
          mediaType + "; charset=" + encoding,
          Loc.NONE,
          ReceiverOption.NONE))
      nextReceiver.startElement(metaCode,
        Untyped.getInstance,
        atts,
        namespaces,
        location,
        ReceiverOption.NONE)
      droppingMetaTags = level
      seekingHead = false
      nextReceiver.endElement()
    }
  }

  override def endElement(): Unit = {
    if (inMetaTag) {
      inMetaTag = false
    } else {
      level -= 1
      if (droppingMetaTags == level + 1) {
        droppingMetaTags = -1
      }
      nextReceiver.endElement()
    }
  }

}

// Copyright (c) 2018-2020 Saxonica Limited
// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is "Incompatible With Secondary Licenses", as defined by the Mozilla Public License, v. 2.0.
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
/**
 * The MetaTagAdjuster adds a meta element to the content of the head element, indicating
 * the required content type and encoding; it also removes any existing meta element
 * containing this information
 */
