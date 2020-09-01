////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
package net.sf.saxon.lib

import net.sf.saxon.utils.Configuration

import net.sf.saxon.utils.Platform

import net.sf.saxon.utils.Version

import net.sf.saxon.event.FilterFactory

import net.sf.saxon.event.IDFilter

import net.sf.saxon.event.ProxyReceiver

import net.sf.saxon.event.Receiver

import net.sf.saxon.functions.EncodeForUri

import net.sf.saxon.functions.ResolveURI

import net.sf.saxon.functions.URIQueryParameters

import net.sf.saxon.om.SpaceStrippingRule

import net.sf.saxon.resource.BinaryResource

import net.sf.saxon.resource.DataURIScheme

import net.sf.saxon.resource.UnparsedTextResource

import net.sf.saxon.trans.Err

import net.sf.saxon.trans.Maker

import net.sf.saxon.trans.NonDelegatingURIResolver

import net.sf.saxon.trans.XPathException

import org.xml.sax.InputSource

import org.xml.sax.XMLReader

import javax.xml.transform.Source

import javax.xml.transform.sax.SAXSource

import java.io.ByteArrayInputStream

import java.io.InputStream

import java.io.Reader

import java.io.StringReader

import java.net.URI

import java.net.URISyntaxException

import java.util.function.Predicate




class StandardURIResolver(private var config: Configuration)
    extends NonDelegatingURIResolver {

  private var recognizeQueryParameters: Boolean = false

  private var allowedUriTest: Predicate[URI] = null

  def this() = this(null)

  def setRecognizeQueryParameters(recognize: Boolean): Unit = {
    recognizeQueryParameters = recognize
  }

  def queryParametersAreRecognized(): Boolean = recognizeQueryParameters

  def setAllowedUriTest(test: Predicate[URI]): Unit = {
    this.allowedUriTest = test
  }

  def getAllowedUriTest: Predicate[URI] =
    if (allowedUriTest == null) {
      if (config == null) {
        var predicae : Predicate[URI] = new Predicate[URI]() {
          override def test(t: URI): Boolean = true
        }
        predicae
        }else{
        config.getAllowedUriTest
      }
    }
    else allowedUriTest


   def getPlatform: Platform = Version.platform

  def setConfiguration(config: Configuration): Unit = {
    this.config = config
  }

  def getConfiguration: Configuration = config

  def resolve(href: String, base: String): Source = {
    if (config != null && config.isTiming) {
      assert(config != null)
      config.getLogger.info(
        "URIResolver.resolve href=\"" + href + "\" base=\"" +
          base +
          "\"")
    }
    var relativeURI: String = href
    var id: String = null
    val hash: Int = href.indexOf('#')
    if (hash >= 0) {
      relativeURI = href.substring(0, hash)
      id = href.substring(hash + 1)
    }
// System.err.println("StandardURIResolver, href=" + href + ", id=" + id);
// System.err.println("StandardURIResolver, href=" + href + ", id=" + id);
    var params: URIQueryParameters = null
    var uri: URI = null
    var relative: URI = null
    relativeURI = ResolveURI.escapeSpaces(relativeURI)
    relative = new URI(relativeURI)
    val query: String = relative.getQuery
    if (query != null && recognizeQueryParameters) {
      params = new URIQueryParameters(query, config)
      val q: Int = relativeURI.indexOf('?')
      relativeURI = relativeURI.substring(0, q)
    }
    var source: Source = null
    if (recognizeQueryParameters && relativeURI.endsWith(".ptree")) {
      throw new UnsupportedOperationException(
        "PTree files are no longer supported (from Saxon 10.0)")
    }
    try uri = ResolveURI.makeAbsolute(relativeURI, base)
    catch {
      case err: URISyntaxException => {
        val expandedBase: String = ResolveURI.tryToExpand(base)
        if (expandedBase != base) {
          resolve(href, expandedBase)
        }
        throw new XPathException(
          "Invalid URI " + Err.wrap(relativeURI) + " - base " +
            Err.wrap(base),
          err)
      }

    }
    if (!getAllowedUriTest.test(uri)) {
      throw new XPathException(
        "URI '" + uri.toString + "' has been disallowed",
        "FODC0002")
    }
    val uriString: String = uri.toString
    EncodeForUri.checkPercentEncoding(uriString)
// Handle a URI using the data: URI scheme
    if ("data" == uri.getScheme) {
      var resource: Resource = null
      resource = DataURIScheme.decode(uri)
      if (resource.isInstanceOf[BinaryResource]) {
        val contents: Array[Byte] =
          resource.asInstanceOf[BinaryResource].getData
        val is: InputSource = new InputSource(
          new ByteArrayInputStream(contents))
        source = new SAXSource(is)
        source.setSystemId(uriString)
      } else {
        assert(resource.isInstanceOf[UnparsedTextResource])
        val reader: Reader = new StringReader(
          resource.asInstanceOf[UnparsedTextResource].getContent)
        source = new SAXSource(new InputSource(reader))
        source.setSystemId(uriString)
      }
    } else {
      source = new SAXSource()
      setSAXInputSource(source.asInstanceOf[SAXSource], uriString)
    }
    if (params != null) {
      val parser: Maker[XMLReader] = params.getXMLReaderMaker
      if (parser != null) {
        source.asInstanceOf[SAXSource].setXMLReader(parser.make())
      }
    }
    if (source.asInstanceOf[SAXSource].getXMLReader == null) {
      if (config == null) {
        source
          .asInstanceOf[SAXSource]
          .setXMLReader(Version.platform.loadParser())
      } else {}
//((SAXSource)source).setXMLReader(config.getSourceParser());
// Leave the Sender to allocate an XMLReader, so that it can be returned to the pool after use
//((SAXSource)source).setXMLReader(config.getSourceParser());
// Leave the Sender to allocate an XMLReader, so that it can be returned to the pool after use
    }
    if (params != null) {
      val stripSpace: SpaceStrippingRule = params.getSpaceStrippingRule
      source = AugmentedSource.makeAugmentedSource(source)
      source
        .asInstanceOf[AugmentedSource]
        .getParseOptions
        .setSpaceStrippingRule(stripSpace)
    }
    if (id != null) {
      val idFinal: String = id
      val factory: FilterFactory = new FilterFactory() {
        def makeFilter(next: Receiver): ProxyReceiver =
          new IDFilter(next, idFinal)
      }
      source = AugmentedSource.makeAugmentedSource(source)
      source.asInstanceOf[AugmentedSource].addFilter(factory)
    }
    if (params != null) {
      val validation: java.lang.Integer = params.getValidationMode
      if (validation != null) {
        source = AugmentedSource.makeAugmentedSource(source)
        source
          .asInstanceOf[AugmentedSource]
          .setSchemaValidationMode(validation)
      }
    }
    if (params != null) {
      val xinclude: java.lang.Boolean = params.getXInclude
      if (xinclude != null) {
        source = AugmentedSource.makeAugmentedSource(source)
        source
          .asInstanceOf[AugmentedSource]
          .setXIncludeAware(xinclude.booleanValue())
      }
    }
    source
  }
// System.err.println("StandardURIResolver, href=" + href + ", base=" + base);
// Extract any fragment identifier. Note, this code is no longer used to
// resolve fragment identifiers in URI references passed to the document()
// function: the code of the document() function handles these itself.
// Check that any "%" sign in the URI is part of a well-formed percent-encoded UTF-8 character.
// Without this check, dereferencing the resulting URL can fail with arbitrary unchecked exceptions
// System.err.println("StandardURIResolver, href=" + href + ", base=" + base);
// Extract any fragment identifier. Note, this code is no longer used to
// resolve fragment identifiers in URI references passed to the document()
// function: the code of the document() function handles these itself.
// Check that any "%" sign in the URI is part of a well-formed percent-encoded UTF-8 character.
// Without this check, dereferencing the resulting URL can fail with arbitrary unchecked exceptions

   def getPTreeSource(href: String, base: String): Source =
    throw new XPathException(
      "PTree files can only be read using a Saxon-EE configuration")

   def setSAXInputSource(source: SAXSource, uriString: String): Unit = {
    if (uriString.startsWith("classpath:") && uriString.length > 10) {
      val is: InputStream = getConfiguration.getDynamicLoader
        .getResourceAsStream(uriString.substring(10))
      if (is != null) {
        source.setInputSource(new InputSource(is))
        source.setSystemId(uriString)
        return
      }
    }
    source.setInputSource(new InputSource(uriString))
    source.setSystemId(uriString)
  }

}

// Copyright (c) 2018-2020 Saxonica Limited
// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is "Incompatible With Secondary Licenses", as defined by the Mozilla Public License, v. 2.0.
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
/**
  * This class provides the service of converting a URI into an {@link Source}.
  * It is used to get stylesheet modules referenced by xsl:import and xsl:include,
  * and source documents referenced by the document() function. The standard version
  * handles anything that the java URL class will handle, plus the <code>classpath</code>
  * URI scheme defined in the Spring framework, and the <code>data</code> URI scheme defined in
  * RFC 2397.
  * <p>You can write a subclass to handle other kinds of URI, for example references to data in
  * a database, or to handle standard URIs in non-standard ways, for example by supplying
  * authentication credentials.</p>
  */
