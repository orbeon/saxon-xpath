////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// Copyright (c) 2018-2020 Saxonica Limited
// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is "Incompatible With Secondary Licenses", as defined by the Mozilla Public License, v. 2.0.
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
package org.orbeon.saxon.lib

import org.orbeon.saxon.event.{FilterFactory, IDFilter, Receiver}
import org.orbeon.saxon.functions.{EncodeForUri, ResolveURI, URIQueryParameters}
import org.orbeon.saxon.resource.{BinaryResource, DataURIScheme, UnparsedTextResource}
import org.orbeon.saxon.trans.{Err, Maker, NonDelegatingURIResolver, XPathException}
import org.orbeon.saxon.utils.{Configuration, Platform, Version}
import org.xml.sax.{InputSource, XMLReader}

import java.io.{ByteArrayInputStream, InputStream, Reader, StringReader}
import java.net.{URI, URISyntaxException}
import java.util.function.Predicate
import javax.xml.transform.Source
import javax.xml.transform.sax.SAXSource


/**
  * This class provides the service of converting a URI into an `Source`.
  * It is used to get stylesheet modules referenced by xsl:import and xsl:include,
  * and source documents referenced by the document() function. The standard version
  * handles anything that the java URL class will handle, plus the `classpath`
  * URI scheme defined in the Spring framework, and the `data` URI scheme defined in
  * RFC 2397.
 *
  * You can write a subclass to handle other kinds of URI, for example references to data in
  * a database, or to handle standard URIs in non-standard ways, for example by supplying
  * authentication credentials.
  */
class StandardURIResolver(private var config: Configuration)
    extends NonDelegatingURIResolver {

  private var recognizeQueryParameters: Boolean = false
  private var allowedUriTest: Predicate[URI] = null

  def this() = this(null)

  def setRecognizeQueryParameters(recognize: Boolean): Unit =
    recognizeQueryParameters = recognize

  def queryParametersAreRecognized(): Boolean = recognizeQueryParameters

  def setAllowedUriTest(test: Predicate[URI]): Unit =
    this.allowedUriTest = test

  def getAllowedUriTest: Predicate[URI] =
    if (allowedUriTest == null) {
      if (config == null)
        (_: URI) => true
      else
        config.getAllowedUriTest
    } else
      allowedUriTest

  def getPlatform: Platform = Version.platform

  def setConfiguration(config: Configuration): Unit =
    this.config = config

  def getConfiguration: Configuration = config

  def resolve(href: String, base: String): Source = {
    if (config != null && config.isTiming) {
      assert(config != null)
      config.getLogger.info(
        "URIResolver.resolve href=\"" + href + "\" base=\"" +
          base +
          "\"")
    }
    var relativeURI = href
    var id: String = null
    val hash: Int = href.indexOf('#')
    if (hash >= 0) {
      relativeURI = href.substring(0, hash)
      id = href.substring(hash + 1)
    }
// System.err.println("StandardURIResolver, href=" + href + ", id=" + id);
// System.err.println("StandardURIResolver, href=" + href + ", id=" + id);

    var params  : URIQueryParameters = null
    var uri     : URI = null
    var relative: URI = null

    relativeURI = ResolveURI.escapeSpaces(relativeURI)
    relative = new URI(relativeURI)
    val query = relative.getQuery
    if (query != null && recognizeQueryParameters) {
      params = new URIQueryParameters(query, config)
      val q = relativeURI.indexOf('?')
      relativeURI = relativeURI.substring(0, q)
    }
    var source: Source = null
    if (recognizeQueryParameters && relativeURI.endsWith(".ptree")) {
      throw new UnsupportedOperationException(
        "PTree files are no longer supported (from Saxon 10.0)")
    }
    try uri = ResolveURI.makeAbsolute(relativeURI, base)
    catch {
      case err: URISyntaxException =>
        val expandedBase: String = ResolveURI.tryToExpand(base)
        if (expandedBase != base)
          return resolve(href, expandedBase)
        throw new XPathException(
          "Invalid URI " + Err.wrap(relativeURI) + " - base " +
            Err.wrap(base),
          err)
    }
    if (! getAllowedUriTest.test(uri))
      throw new XPathException("URI '" + uri.toString + "' has been disallowed", "FODC0002")
    val uriString = uri.toString
    EncodeForUri.checkPercentEncoding(uriString)
    // Handle a URI using the data: URI scheme
    if ("data" == uri.getScheme) {
      var resource: Resource = null
      resource = DataURIScheme.decode(uri)
      resource match {
        case binaryResource: BinaryResource =>
          val contents = binaryResource.getData
          val is = new InputSource(new ByteArrayInputStream(contents))
          source = new SAXSource(is)
          source.setSystemId(uriString)
        case _ =>
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
      val stripSpace = params.getSpaceStrippingRule
      source = AugmentedSource.makeAugmentedSource(source)
      source
        .asInstanceOf[AugmentedSource]
        .getParseOptions
        .setSpaceStrippingRule(stripSpace)
    }
    if (id != null) {
      val idFinal = id
      val factory: FilterFactory = (next: Receiver) => new IDFilter(next, idFinal)
      source = AugmentedSource.makeAugmentedSource(source)
      source.asInstanceOf[AugmentedSource].addFilter(factory)
    }
    if (params != null) {
      val validation = params.getValidationMode
      if (validation != null) {
        source = AugmentedSource.makeAugmentedSource(source)
        source
          .asInstanceOf[AugmentedSource]
          .setSchemaValidationMode(validation)
      }
    }
    if (params != null) {
      val xinclude = params.getXInclude
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
      val is: InputStream = getConfiguration.getResourceAsStream(uriString.substring(10))
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
