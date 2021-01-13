////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// Copyright (c) 2018-2020 Saxonica Limited
// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is "Incompatible With Secondary Licenses", as defined by the Mozilla Public License, v. 2.0.
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
package org.orbeon.saxon.event

import org.orbeon.saxon.expr.number.Numberer_en
import org.orbeon.saxon.expr.parser.Loc
import org.orbeon.saxon.lib._
import org.orbeon.saxon.model.Type
import org.orbeon.saxon.om._
import org.orbeon.saxon.pull.{PullPushCopier, PullSource}
import org.orbeon.saxon.s9api.Location
import org.orbeon.saxon.sapling.SaplingDocument
import org.orbeon.saxon.trans.{SaxonErrorCode, XPathException, XmlProcessingException, XmlProcessingIncident}
import org.orbeon.saxon.utils.Version
import org.xml.sax._

import javax.xml.transform.Source
import javax.xml.transform.sax.SAXSource
import javax.xml.transform.stream.StreamSource
import scala.jdk.CollectionConverters._


/**
  * Sender is a helper class that sends events to a Receiver from any kind of Source object
  */
object Sender {

  def send(source: Source, receiver: Receiver, options: ParseOptions): Unit = {
    var lSource = source
    var parserOptions = options
    val pipe = receiver.getPipelineConfiguration
    parserOptions =
      if (options == null)
        new ParseOptions(pipe.getParseOptions)
      else
        new ParseOptions(options)
    var systemId = lSource.getSystemId
    lSource match {
      case augmentedSource: AugmentedSource =>
        parserOptions.merge(augmentedSource.getParseOptions)
        systemId = lSource.getSystemId
        lSource = augmentedSource.getContainedSource
      case _ =>
    }
    val config = pipe.getConfiguration
    parserOptions.applyDefaults(config)
    receiver.setSystemId(systemId)
    var next = receiver
    val schemaValidation = parserOptions.getSchemaValidationMode
    val filters = parserOptions.getFilters
    if (filters != null) {
      var i = filters.size - 1
      while (i >= 0) {
        val filter = filters.get(i).makeFilter(next)
        filter.setSystemId(lSource.getSystemId)
        next = filter
        i -= 1
      }
    }
    val strippingRule = parserOptions.getSpaceStrippingRule
    if (strippingRule != null && ! strippingRule.isInstanceOf[NoElementsSpaceStrippingRule])
      next = strippingRule.makeStripper(next)
    lSource match {
      case treeInfo: TreeInfo =>
        lSource = treeInfo.getRootNode
      case _ =>
    }
    lSource match {
      case ns: NodeInfo =>
        val baseURI = ns.getBaseURI
        if (schemaValidation != Validation.PRESERVE)
          next = config.getDocumentValidator(next, baseURI, parserOptions, null)
        val kind = ns.getNodeKind
        if (kind != Type.DOCUMENT && kind != Type.ELEMENT)
          throw new IllegalArgumentException("Sender can only handle document or element nodes")
        next.setSystemId(baseURI)
        val loc = new Loc(systemId, -1, -1)
        sendDocumentInfo(ns, next, loc)
        return
      case pullSource: PullSource =>
        sendPullSource(pullSource, next, parserOptions)
        return
      case eventSource: EventSource =>
        eventSource.send(next)
        return
      case saxSource: SAXSource =>
        sendSAXSource(saxSource, next, parserOptions)
        return
      case _ => if (source.isInstanceOf[StreamSource]) {
        val ss            = lSource.asInstanceOf[StreamSource]
        // Following code allows the .NET platform to use a Pull parser
        val dtdValidation = parserOptions.getDTDValidationMode == Validation.STRICT
        val ps            = Version.platform.getParserSource(pipe,
          ss,
          schemaValidation,
          dtdValidation)
        if (ps == ss) {
          val url = lSource.getSystemId
          val is  = new InputSource(url)
          is.setCharacterStream(ss.getReader)
          is.setByteStream(ss.getInputStream)
          var reuseParser = false
          var parser      = parserOptions.obtainXMLReader()
          if (parser == null) {
            parser = config.getSourceParser
            if (parserOptions.getEntityResolver != null && parser.getEntityResolver == null)
              parser.setEntityResolver(parserOptions.getEntityResolver)
            reuseParser = true
          }
          //System.err.println("Using parser: " + parser.getClass().getName);
          val sax = new SAXSource(parser, is)
          sax.setSystemId(lSource.getSystemId)
          sendSAXSource(sax, next, parserOptions)
          if (reuseParser)
            config.reuseSourceParser(parser)
        } else {
          // On .NET with a default URIResolver we can expect an AugnmentedSource wrapping a PullSource
          send(ps, next, parserOptions)
        }
        // the Platform substituted a different kind of source
        // the Platform substituted a different kind of source
        return
      } else lSource match {
        // ORBEON: No StAX support.
        //      case staxSource: StAXSource =>
        //        val reader: XMLStreamReader =
        //          staxSource.getXMLStreamReader
        //        if (reader == null)
        //          throw new XPathException("Saxon can only handle a StAXSource that wraps an XMLStreamReader")
        //        val bridge: StaxBridge = new StaxBridge()
        //        bridge.setXMLStreamReader(reader)
        //        sendPullSource(new PullSource(bridge), next, parserOptions)
        //        return
        case saplingDocument: SaplingDocument =>
          saplingDocument.sendTo(next)
          return
        case _ =>
          next = makeValidator(next, lSource.getSystemId, parserOptions)
          // See if there is a registered SourceResolver than can handle it
          val newSource = config.getSourceResolver.resolveSource(source, config)
          if (newSource.isInstanceOf[StreamSource]    ||
              newSource.isInstanceOf[SAXSource]       ||
              newSource.isInstanceOf[NodeInfo]        ||
              newSource.isInstanceOf[PullSource]      ||
              newSource.isInstanceOf[AugmentedSource] ||
              newSource.isInstanceOf[EventSource]) {
            send(newSource, next, parserOptions)
            return
          }
          val externalObjectModels = config.getExternalObjectModels
          for (externalObjectModel <- externalObjectModels.asScala) {
            val model = externalObjectModel.asInstanceOf[ExternalObjectModel]
            val done  = model.sendSource(lSource, next)
            if (done)
              return
          }
      }
    }
// See if there is a registered external object model that knows about this kind of source
// (Note, this should pick up the platform-specific DOM model)
// See if there is a registered external object model that knows about this kind of source
// (Note, this should pick up the platform-specific DOM model)
    throw new XPathException("A source of type " + source.getClass.getName + " is not supported in this environment")
  }
//        else if (options.getStripSpace() == Whitespace.ALL) {
//            next = new Stripper(AllElementsSpaceStrippingRule.getInstance(), next);
//        } else if (options.getStripSpace() == Whitespace.XSLT) {
//            Controller controller = pipe.getController();
//            if (controller != null) {
//                next = controller.makeStripper(next);
//            }
//        }
//        else if (options.getStripSpace() == Whitespace.ALL) {
//            next = new Stripper(AllElementsSpaceStrippingRule.getInstance(), next);
//        } else if (options.getStripSpace() == Whitespace.XSLT) {
//            Controller controller = pipe.getController();
//            if (controller != null) {
//                next = controller.makeStripper(next);
//            }
//        }

  private def sendDocumentInfo(
    top      : NodeInfo,
    receiver : Receiver,
    location : Location
  ): Unit = {
    val pipe           = receiver.getPipelineConfiguration
    val targetNamePool = pipe.getConfiguration.getNamePool
    var lReceiver      = receiver
    if (top.getConfiguration.getNamePool != targetNamePool) {
      // namecodes as necessary
      lReceiver = new NamePoolConverter(receiver,
                                       top.getConfiguration.getNamePool,
                                       targetNamePool)
    }
// This code allows a document in one Configuration to be copied to another, changing
    val copier = new LocationCopier(
      top.getNodeKind == Type.DOCUMENT)
      pipe.setComponent(classOf[CopyInformee[_<: AnyRef]].getName, copier)
    // start event stream
    receiver.open()
    // copy the contents of the document
    top.getNodeKind match {
      case Type.DOCUMENT =>
        top.copy(receiver,
                 CopyOptions.ALL_NAMESPACES | CopyOptions.TYPE_ANNOTATIONS,
                 location)
      case Type.ELEMENT =>
        receiver.startDocument(ReceiverOption.NONE)
        top.copy(receiver,
                 CopyOptions.ALL_NAMESPACES | CopyOptions.TYPE_ANNOTATIONS,
                 location)
        receiver.endDocument()
      case _ =>
        throw new IllegalArgumentException("Expected document or element node")

    }
    // end event stream
    receiver.close()
  }

  private def sendSAXSource(source: SAXSource,
                            receiver: Receiver,
                            options: ParseOptions): Unit = {
    var lSource = source
    var lReceiver = receiver
    val pipe = lReceiver.getPipelineConfiguration
    var parser               = lSource.getXMLReader
    var reuseParser          = false
    val config               = pipe.getConfiguration
    var listener             = options.getErrorReporter
    if (listener == null)
      listener = pipe.getErrorReporter
    var errorHandler = options.getErrorHandler
    if (errorHandler == null)
      errorHandler = new StandardErrorHandler(listener)
    if (parser == null)
      parser = options.obtainXMLReader()
    if (parser == null) {
      val ss = new SAXSource()
      ss.setInputSource(lSource.getInputSource)
      ss.setSystemId(lSource.getSystemId)
      parser = config.getSourceParser
      parser.setErrorHandler(errorHandler)
      if (options.getEntityResolver != null && parser.getEntityResolver == null)
        parser.setEntityResolver(options.getEntityResolver)
      ss.setXMLReader(parser)
      lSource = ss
      reuseParser = true
    } else {
// user-supplied parser: ensure that it meets the namespace requirements
      configureParser(parser)
      if (parser.getErrorHandler == null)
        parser.setErrorHandler(errorHandler)
    }
    if (!pipe.getParseOptions.isExpandAttributeDefaults) {
      try parser.setFeature("http://xml.org/sax/features/use-attributes2",
                            true)
      catch {
        case _: SAXNotRecognizedException | _: SAXNotSupportedException =>
      }
    }
    val dtdRecover                         = options.getDTDValidationMode == Validation.LAX
    val parserFeatures = options.getParserFeatures
    val parserProperties = options.getParserProperties
    if (parserFeatures != null) {
      for ((key, mapVal) <- parserFeatures.asScala) {
        try {
          val name  = key
          val value = mapVal
          if (name == "http://apache.org/xml/features/xinclude") {
            var tryAgain = false
            try // This feature name is supported in Xerces 2.9.0
              parser.setFeature(name, value)
            catch {
              case _: SAXNotRecognizedException | _: SAXNotSupportedException =>
                tryAgain = true
            }
            if (tryAgain)
              parser.setFeature(name + "-aware", value)
          } else {
            parser.setFeature(key, value)
          }
        } catch {
          case _: SAXNotRecognizedException =>
            if (mapVal)
              config.getLogger.warning(namedParser(parser) + " does not recognize the feature " + key)
          case _: SAXNotSupportedException =>
            if (mapVal)
              config.getLogger.warning(namedParser(parser) + " does not support the feature " + key)
        }
      }
    }
    if (parserProperties != null) {
      for ((key, value) <- parserProperties.asScala) {
        try
          parser.setProperty(key, value)
        catch {
          case _: SAXNotRecognizedException =>
            config.getLogger.warning(namedParser(parser) + " does not recognize the property " + key)
          case _: SAXNotSupportedException  =>
            config.getLogger.warning(namedParser(parser) + " does not support the property " + key)
        }
      }
    }
    val xInclude = options.isXIncludeAware
    if (xInclude) {
      var tryAgain = false
      try // This feature name is supported in the version of Xerces bundled with JDK 1.5
        parser.setFeature("http://apache.org/xml/features/xinclude-aware", true)
      catch {
        case _: SAXNotRecognizedException | _: SAXNotSupportedException =>
          tryAgain = true
      }
      if (tryAgain)
        parser.setFeature("http://apache.org/xml/features/xinclude", true)
    }
    lReceiver = makeValidator(lReceiver, lSource.getSystemId, options)
    var ce: ReceivingContentHandler = null
    val ch = parser.getContentHandler
    ch match {
      case rch: ReceivingContentHandler if config.isCompatible(rch.getConfiguration) =>
        ce = rch
        ce.reset()
      case _ =>
        ce = new ReceivingContentHandler()
        parser.setContentHandler(ce)
        parser.setDTDHandler(ce)
        try
          parser.setProperty("http://xml.org/sax/properties/lexical-handler", ce)
        catch {
          case _: SAXNotSupportedException | _: SAXNotRecognizedException =>
        }
    }
    ce.setReceiver(lReceiver)
    ce.setPipelineConfiguration(pipe)
    try
      parser.parse(lSource.getInputSource)
    catch {
      case err: SAXException =>
        val nested: Exception = err.getException
        nested match {
          case exception: XPathException   =>
            throw exception
          case exception: RuntimeException =>
            throw exception
          case _                           =>
            if ((errorHandler.isInstanceOf[StandardErrorHandler] &&
              errorHandler
                .asInstanceOf[StandardErrorHandler]
                .getFatalErrorCount ==
                0) ||
              (err.isInstanceOf[SAXParseException] &&
                err.asInstanceOf[SAXParseException].getSystemId == null &&
                lSource.getSystemId != null)) {
              val de: XPathException = new XPathException(
                "Error reported by XML parser processing " + lSource.getSystemId +
                  ": " +
                  err.getMessage,
                err
              )
              listener.report(new XmlProcessingException(de))
              de.setHasBeenReported(true)
              throw de
            } else {
              val de: XPathException = new XPathException(err)
              de.setErrorCode(SaxonErrorCode.SXXP0003)
              de.setHasBeenReported(true)
              throw de
            }
        }
      case err: java.io.IOException =>
        throw new XPathException(
          "I/O error reported by XML parser processing " + lSource.getSystemId +
            ": " +
            err.getMessage,
          err)

    }
    errorHandler match {
      case handler: StandardErrorHandler =>
        var errs = handler.getFatalErrorCount
        if (errs > 0)
          throw new XPathException(
            "The XML parser reported " + errs + (if (errs == 1) " error"
            else " errors")
          )
        errs = handler.getErrorCount
        if (errs > 0) {
          var message = "The XML parser reported " + new Numberer_en()
            .toWords(errs)
            .toLowerCase() +
            " validation error" +
            (if (errs == 1) "" else "s")
          if (dtdRecover) {
            message += ". Processing continues, because recovery from validation errors was requested"
            val warning = new XmlProcessingIncident(message).asWarning()
            listener.report(warning)
          } else {
            throw new XPathException(message)
          }
        }
      case _ =>
    }
    if (reuseParser)
      config.reuseSourceParser(parser)
  }
//        if (config.isTiming()) {
//            System.err.println("Using SAX parser " + parser);
//        }
// Reuse the previous ReceivingContentHandler if possible (it contains a useful cache of names)
//        TracingFilter tf = new TracingFilter();
//        tf.setUnderlyingReceiver(receiver);
//        tf.setPipelineConfiguration(pipe);
//        receiver = tf;
//        if (config.isTiming()) {
//            System.err.println("Using SAX parser " + parser);
//        }
// Reuse the previous ReceivingContentHandler if possible (it contains a useful cache of names)
//        TracingFilter tf = new TracingFilter();
//        tf.setUnderlyingReceiver(receiver);
//        tf.setPipelineConfiguration(pipe);
//        receiver = tf;

  private def namedParser(parser: XMLReader): String =
    "Selected XML parser " + parser.getClass.getName

  private def makeValidator(
    receiver : Receiver,
    systemId : String,
    options  : ParseOptions
  ): Receiver = {
    val pipe = receiver.getPipelineConfiguration
    val config = pipe.getConfiguration
    val sv     = options.getSchemaValidationMode
    if (sv != Validation.PRESERVE && sv != Validation.DEFAULT) {
      val controller = pipe.getController
      if (controller != null && ! controller.getExecutable.isSchemaAware && sv != Validation.STRIP)
        throw new XPathException("Cannot use schema-validated input documents when the query/stylesheet is not schema-aware")
    }
    receiver
  }

  private def sendPullSource(
    source   : PullSource,
    receiver : Receiver,
    options  : ParseOptions
  ): Unit = {
    var lReceiver = receiver
    val pipe      = lReceiver.getPipelineConfiguration
    val xInclude  = options.isXIncludeAware
    if (xInclude)
      throw new XPathException("XInclude processing is not supported with a pull parser")
    lReceiver = makeValidator(lReceiver, source.getSystemId, options)
    val provider = source.getPullProvider
    provider.setPipelineConfiguration(pipe)
    lReceiver.setPipelineConfiguration(pipe)
    val copier = new PullPushCopier(provider, lReceiver)
    try
      copier.copy()
    finally
      if (options.isPleaseCloseAfterUse)
        provider.close()
  }

  def configureParser(parser: XMLReader): Unit = {
    parser.setFeature("http://xml.org/sax/features/namespaces", true)
    parser.setFeature("http://xml.org/sax/features/namespace-prefixes", false)
  }
}
