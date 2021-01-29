////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// Copyright (c) 2018-2020 Saxonica Limited
// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is "Incompatible With Secondary Licenses", as defined by the Mozilla Public License, v. 2.0.
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
package org.orbeon.saxon.serialize

import org.orbeon.saxon.event.{ProxyReceiver, Receiver, ReceiverOption}
import org.orbeon.saxon.expr.parser.Loc
import org.orbeon.saxon.lib.{NamespaceConstant, SaxonOutputKeys}
import org.orbeon.saxon.model.{BuiltInAtomicType, SchemaType, Untyped}
import org.orbeon.saxon.om._
import org.orbeon.saxon.s9api.Location
import org.orbeon.saxon.value.Whitespace

import java.util.Properties
import javax.xml.transform.OutputKeys
import scala.jdk.CollectionConverters._
import scala.util.control.Breaks._


/**
 * The MetaTagAdjuster adds a meta element to the content of the head element, indicating
 * the required content type and encoding; it also removes any existing meta element
 * containing this information
 */
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
    if (encoding == null)
      encoding = "UTF-8"
    mediaType = details.getProperty(OutputKeys.MEDIA_TYPE)
    if (mediaType == null)
      mediaType = "text/html"
    var htmlVn = details.getProperty(SaxonOutputKeys.HTML_VERSION)
    if (htmlVn == null && ! isXHTML)
      htmlVn = details.getProperty(OutputKeys.VERSION)
    if (htmlVn != null && htmlVn.startsWith("5"))
      htmlVersion = 5
  }

  def setIsXHTML(xhtml: Boolean): Unit =
    isXHTML = xhtml

  private def comparesEqual(name1: String, name2: String): Boolean =
    if (isXHTML)
      name1 == name2
    else
      name1.equalsIgnoreCase(name2)

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

  override def startElement(
    elemName   : NodeName,
    `type`     : SchemaType,
    attributes : AttributeMap,
    namespaces : NamespaceMap,
    location   : Location,
    properties : Int
  ): Unit = {
    if (droppingMetaTags == level) {
      if (matchesName(elemName, "meta")) {
        // if there was an http-equiv="ContentType" attribute, discard the meta element entirely
        var found = false
        breakable {
          for (att <- attributes.iterator.asScala) {
            val name = att.getNodeName.getLocalPart
            if (comparesEqual(name, "http-equiv")) {
              val value = Whitespace.trim(att.getValue)
              if (value.equalsIgnoreCase("Content-Type")) {
                // case-blind comparison even for XHTML
                found = true
                break()
              }
            }
          }
        }
        inMetaTag = found
        if (found)
          return
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
      val headPrefix = elemName.getPrefix
      val headURI    = elemName.getURI
      val metaCode   = new FingerprintedQName(headPrefix, headURI, "meta")
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

  override def endElement(): Unit =
    if (inMetaTag) {
      inMetaTag = false
    } else {
      level -= 1
      if (droppingMetaTags == level + 1)
        droppingMetaTags = -1
      nextReceiver.endElement()
    }
}
