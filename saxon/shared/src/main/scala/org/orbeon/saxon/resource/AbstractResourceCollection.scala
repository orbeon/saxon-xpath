////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
package org.orbeon.saxon.resource

import org.orbeon.saxon.utils.Configuration

import org.orbeon.saxon.utils.Controller

import org.orbeon.saxon.expr.XPathContext

import org.orbeon.saxon.functions.URIQueryParameters

import org.orbeon.saxon.lib._

import org.orbeon.saxon.om.SpaceStrippingRule

import org.orbeon.saxon.trans.Maker

import org.orbeon.saxon.trans.XPathException

import org.orbeon.saxon.trans.XmlProcessingIncident

import org.xml.sax.XMLReader

import java.io._

import java.net.URI

import java.net.URISyntaxException

import java.net.URL

import java.net.URLConnection

import AbstractResourceCollection._


object AbstractResourceCollection {

  def setupErrorHandlingForCollection(
                                       options: ParseOptions,
                                       onError: Int,
                                       oldErrorListener: ErrorReporter): Unit = {
    if (onError == URIQueryParameters.ON_ERROR_IGNORE) {
      options.setErrorReporter((error) => {})
    } else if (onError == URIQueryParameters.ON_ERROR_WARNING) {
      options.setErrorReporter((error) =>
        if (error.isWarning) {
          oldErrorListener.report(error)
        } else {
          oldErrorListener.report(error.asWarning())
          val supp: XmlProcessingIncident = new XmlProcessingIncident(
            "The document will be excluded from the collection").asWarning()
          supp.setLocation(error.getLocation)
          oldErrorListener.report(supp)
        })
    }
  }

  class InputDetails {

    var resourceUri: String = _

    var binaryContent: Array[Byte] = _

    var characterContent: String = _

    var contentType: String = _

    var encoding: String = _

    var parseOptions: ParseOptions = _

    var onError: Int = URIQueryParameters.ON_ERROR_FAIL

    def getInputStream: InputStream = {
      val url: URL = new URL(resourceUri)
      val connection: URLConnection = url.openConnection()
      connection.getInputStream
    }

    def obtainBinaryContent(): Array[Byte] =
      if (binaryContent != null) {
        binaryContent
      } else if (characterContent != null) {
        val e: String = if (encoding != null) encoding else "UTF-8"
        characterContent.getBytes(e)
      } else {
        val stream = getInputStream
        BinaryResource.readBinaryFromStream(stream, resourceUri)
      }


    def obtainCharacterContent(): String =
      if (characterContent != null) {
        characterContent
      } else if (binaryContent != null && encoding != null) {
        new String(binaryContent, encoding)
      } else {
        var builder: StringBuilder = null
        var enc = encoding
        val stream = getInputStream
        if (enc == null) {
          enc = StandardUnparsedTextResolver.inferStreamEncoding(stream, null)
        }
        builder = CatalogCollection.makeStringBuilderFromStream(stream, enc)
        characterContent = builder.toString
        return characterContent
      }

  }

}

/**
 * AbstractCollection is an abstract superclass for the various implementations
 * of ResourceCollection within Saxon. It provides common services such as
 * mapping of file extensions to MIME types, and mapping of MIME types to
 * resource factories.
 */
abstract class AbstractResourceCollection(var config: Configuration)
  extends ResourceCollection {

  var collectionURI: String = _

  var params: URIQueryParameters = null

  def setParams(params: URIQueryParameters) = this.params = params

  def getParams: URIQueryParameters = params

  def getCollectionURI(): String = collectionURI

  def setCollectionURI(collectionURI: String) = this.collectionURI = collectionURI

  def isStable(context: XPathContext): Boolean = {
    if (params == null) {
      return false
    }
    val stable: java.lang.Boolean = params.getStable
    if (stable == null) {
      context.getConfiguration.getBooleanProperty(
        Feature.STABLE_COLLECTION_URI)
    } else {
      stable
    }
  }

  def registerContentType(contentType: String,
                          factory: ResourceFactory): Unit = {
    config.registerMediaType(contentType, factory)
  }

  def optionsFromQueryParameters(
                                  params: URIQueryParameters,
                                  context: XPathContext): ParseOptions = {
    val options: ParseOptions = new ParseOptions(
      context.getConfiguration.getParseOptions)
    if (params != null) {
      val v: java.lang.Integer = params.getValidationMode
      if (v != null) {
        options.setSchemaValidationMode(v)
      }
      val xInclude: java.lang.Boolean = params.getXInclude
      if (xInclude != null) {
        options.setXIncludeAware(xInclude)
      }
      val stripSpace: SpaceStrippingRule = params.getSpaceStrippingRule
      if (stripSpace != null) {
        options.setSpaceStrippingRule(stripSpace)
      }
      val p: Maker[XMLReader] = params.getXMLReaderMaker
      if (p != null) {
        options.setXMLReaderMaker(p)
      }
      var onError: Int = URIQueryParameters.ON_ERROR_FAIL
      if (params.getOnError != null) {
        onError = params.getOnError
      }
      val controller: Controller = context.getController
      //        final PipelineConfiguration newPipe = new PipelineConfiguration(oldPipe);
      val oldErrorListener: ErrorReporter =
        if (controller == null) new StandardErrorReporter()
        else controller.getErrorReporter
      setupErrorHandlingForCollection(options, onError, oldErrorListener)
    }
    // If the URI requested suppression of errors, or that errors should be treated
    // as warnings, we set up a special ErrorListener to achieve this
    //        final PipelineConfiguration oldPipe = context.getConfiguration.makePipelineConfiguration();
    //        oldPipe.setController(context.getController());
    // If the URI requested suppression of errors, or that errors should be treated
    // as warnings, we set up a special ErrorListener to achieve this
    //        final PipelineConfiguration oldPipe = context.getConfiguration.makePipelineConfiguration();
    //        oldPipe.setController(context.getController());
    options
  }

  def getInputDetails(resourceURI: String): InputDetails = {
    val inputDetails: InputDetails = new InputDetails()
    inputDetails.resourceUri = resourceURI
    val uri: URI = new URI(resourceURI)
    if ("file" == uri.getScheme) {
      inputDetails.contentType =
        if (params != null && params.getContentType != null)
          params.getContentType
        else guessContentTypeFromName(resourceURI)
    } else {
      val url: URL = uri.toURL()
      val connection: URLConnection = url.openConnection()
      //inputDetails.inputStream = connection.getInputStream();
      inputDetails.contentType = connection.getContentType
      inputDetails.encoding = connection.getContentEncoding
      for (param <- inputDetails.contentType.replace(" ", "").split(";")) {
        if (param.startsWith("charset=")) {
          inputDetails.encoding = param.split("=", 2)(1)
        } else {
          inputDetails.contentType = param
        }
      }
    }
    if (inputDetails.contentType == null ||
      config.getResourceFactoryForMediaType(inputDetails.contentType) ==
        null) {
      var stream: InputStream = null
      if ("file" == uri.getScheme) {
        val file: File = new File(uri)
        stream = new BufferedInputStream(new FileInputStream(file))
        if (file.length <= 1024) {
          inputDetails.binaryContent =
            BinaryResource.readBinaryFromStream(stream, resourceURI)
          stream.close()
          stream = new ByteArrayInputStream(inputDetails.binaryContent)
        }
      } else {
        val url: URL = uri.toURL()
        val connection: URLConnection = url.openConnection()
        stream = connection.getInputStream
      }
      inputDetails.contentType = guessContentTypeFromContent(stream)
      stream.close()
    }
    if (params != null && params.getOnError != null) {
      inputDetails.onError = params.getOnError
    }
    inputDetails
  }

  def guessContentTypeFromName(resourceURI: String): String = {
    var contentTypeFromName: String =
      URLConnection.guessContentTypeFromName(resourceURI)
    var extension: String = null
    if (contentTypeFromName == null) {
      extension = getFileExtension(resourceURI)
      if (extension != null) {
        contentTypeFromName = config.getMediaTypeForFileExtension(extension)
      }
    }
    contentTypeFromName
  }

  def guessContentTypeFromContent(stream: InputStream): String =
    try {
      var typeStream: InputStream = stream
      if (!typeStream.markSupported()) {
        typeStream = new BufferedInputStream(typeStream)
      }
      URLConnection.guessContentTypeFromStream(typeStream)
    } catch {
      case err: IOException => null

    }

  private def getFileExtension(name: String): String = {
    val i: Int = name.lastIndexOf('.')
    val p: Int = Math.max(name.lastIndexOf('/'), name.lastIndexOf('\\'))
    if (i > p && i + 1 < name.length) {
      return name.substring(i + 1)
    }
    null
  }

  def makeResource(config: Configuration, details: InputDetails): Resource = {
    var factory: ResourceFactory = null
    val contentType: String = details.contentType
    if (contentType != null) {
      factory = config.getResourceFactoryForMediaType(contentType)
    }
    if (factory == null) {
      factory = BinaryResource.FACTORY
    }
    factory.makeResource(config, details)
  }

  def makeTypedResource(config: Configuration,
                        basicResource: Resource): Resource = {
    val mediaType: String = basicResource.getContentType
    val factory: ResourceFactory =
      config.getResourceFactoryForMediaType(mediaType)
    if (factory == null) {
      return basicResource
    }
    if (basicResource.isInstanceOf[BinaryResource]) {
      val details: InputDetails = new InputDetails()
      details.binaryContent =
        basicResource.asInstanceOf[BinaryResource].getData
      details.contentType = mediaType
      details.resourceUri = basicResource.getResourceURI
      factory.makeResource(config, details)
    } else if (basicResource.isInstanceOf[UnparsedTextResource]) {
      val details: InputDetails = new InputDetails()
      details.characterContent =
        basicResource.asInstanceOf[UnparsedTextResource].getContent
      details.contentType = mediaType
      details.resourceUri = basicResource.getResourceURI
      factory.makeResource(config, details)
    } else {
      basicResource
    }
  }

  def makeResource(config: Configuration, resourceURI: String): Resource = {
    val details: InputDetails = getInputDetails(resourceURI)
    makeResource(config, details)
  }

  def stripWhitespace(rules: SpaceStrippingRule): Boolean = false

}

// Copyright (c) 2018-2020 Saxonica Limited
// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is "Incompatible With Secondary Licenses", as defined by the Mozilla Public License, v. 2.0.
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
