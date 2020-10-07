////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
package org.orbeon.saxon.event

import org.orbeon.saxon.utils.Configuration

import org.orbeon.saxon.lib.StandardURIResolver

import org.orbeon.saxon.model.SchemaType

import org.orbeon.saxon.om.AttributeMap

import org.orbeon.saxon.om.NamespaceMap

import org.orbeon.saxon.om.NodeName

import org.orbeon.saxon.s9api.Location

import org.orbeon.saxon.trans.XPathException

import org.orbeon.saxon.tree.util.ProcInstParser

import javax.xml.transform.Source

import javax.xml.transform.TransformerException

import javax.xml.transform.URIResolver

import javax.xml.transform.sax.SAXSource

import java.util.ArrayList

import java.util.Comparator

import java.util.List

import scala.beans.{BeanProperty, BooleanBeanProperty}


class PIGrabber(next: Receiver) extends ProxyReceiver(next) {

  private var config: Configuration = null

  private var reqMedia: String = null

  private var reqTitle: String = null

  private var baseURI: String = null

  private var uriResolver: URIResolver = null

  private var stylesheets: List[String] = new ArrayList()

  @BooleanBeanProperty
  var terminated: Boolean = false

  def setFactory(config: Configuration): Unit = {
    this.config = config
  }

  def setCriteria(media: String, title: String): Unit = {
    this.reqMedia = media
    this.reqTitle = title
  }

  def setBaseURI(uri: String): Unit = {
    baseURI = uri
  }

  def setURIResolver(resolver: URIResolver): Unit = {
    uriResolver = resolver
  }

  override def startElement(elemName: NodeName,
                            `type`: SchemaType,
                            attributes: AttributeMap,
                            namespaces: NamespaceMap,
                            location: Location,
                            properties: Int): Unit = {
    terminated = true
    // abort the parse when the first start element tag is found
    throw new XPathException("#start#")
  }

  override def processingInstruction(target: String,
                                     data: CharSequence,
                                     locationId: Location,
                                     properties: Int): Unit = {
    if (target.==("xml-stylesheet")) {
      val value: String = data.toString
      val piMedia: String = ProcInstParser.getPseudoAttribute(value, "media")
      val piTitle: String = ProcInstParser.getPseudoAttribute(value, "title")
      val piType: String = ProcInstParser.getPseudoAttribute(value, "type")
      val piAlternate: String =
        ProcInstParser.getPseudoAttribute(value, "alternate")
      if (piType == null) {
        return
      }
      if ((piType.==("text/xml") || piType.==("application/xml") ||
        piType.==("text/xsl") ||
        piType.==("applicaton/xsl") ||
        piType.==("application/xml+xslt")) &&
        (reqMedia == null || piMedia == null ||
          // see bug 1729
          getConfiguration.getMediaQueryEvaluator.compare(piMedia, reqMedia) ==
            0) &&
        ((piTitle == null && (piAlternate == null || piAlternate.==("no"))) ||
          (reqTitle == null) ||
          (piTitle != null && piTitle == reqTitle))) {
        val href: String = ProcInstParser.getPseudoAttribute(value, "href")
        if (href == null) {
          throw new XPathException("xml-stylesheet PI has no href attribute")
        }
        // System.err.println("Adding " + href);
        if (piTitle == null && (piAlternate == null || piAlternate.==("no"))) {
          stylesheets.add(0, href)
        } else {
          stylesheets.add(href)
        }
      } else {}
      //System.err.println("No match on required media=" + reqMedia + " title=" + reqTitle );
      //System.err.println("No match on required media=" + reqMedia + " title=" + reqTitle );
    }
    // System.err.println("Found xml-stylesheet media=" + piMedia + " title=" + piTitle);
    // System.err.println("Found xml-stylesheet media=" + piMedia + " title=" + piTitle);
  }

  /*@Nullable*/

  def getAssociatedStylesheets: Array[Source] = {
    if (stylesheets.isEmpty)
      return null
    if (uriResolver == null) {
      uriResolver = new StandardURIResolver(config)
    }
    val result: Array[Source] = Array.ofDim[Source](stylesheets.size)
    for (i <- 0 until stylesheets.size) {
      val href: String = stylesheets.get(i)
      var s: Source = uriResolver.resolve(href, baseURI)
      if (s.isInstanceOf[SAXSource]) {
        s.asInstanceOf[SAXSource].setXMLReader(config.getStyleParser)
      }
      if (s == null) {
        s = config.getSystemURIResolver.resolve(href, baseURI)
      }
      result(i) = s
    }
    result
  }

}

// Copyright (c) 2018-2020 Saxonica Limited
// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is "Incompatible With Secondary Licenses", as defined by the Mozilla Public License, v. 2.0.
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
/**
 * The <tt>PIGrabber</tt> class is a {@link ProxyReceiver} that looks for {@code xml-stylesheet} processing
 * instructions and tests whether they match specified criteria; for those that do, it creates
 * a {@link Source} object referring to the relevant stylesheet
 */
//
