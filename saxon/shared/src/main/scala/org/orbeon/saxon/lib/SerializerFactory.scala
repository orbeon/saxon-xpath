////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// Copyright (c) 2018-2020 Saxonica Limited
// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is "Incompatible With Secondary Licenses", as defined by the Mozilla Public License, v. 2.0.
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
package org.orbeon.saxon.lib

import java.util._
import java.util.regex.Pattern

import javax.xml.transform.sax.SAXResult
import javax.xml.transform.stream.StreamResult
import javax.xml.transform.{OutputKeys, Result, Source, TransformerException}
import org.orbeon.saxon.event._
import org.orbeon.saxon.om.{NameChecker, NamespaceResolver, QNameException}
import org.orbeon.saxon.query.SequenceWrapper
import org.orbeon.saxon.serialize._
import org.orbeon.saxon.trans.{Err, SaxonErrorCode, XPathException}
import org.orbeon.saxon.utils.Configuration
import org.orbeon.saxon.value.BigDecimalValue

//import scala.collection.compat._
import scala.jdk.CollectionConverters._


/**
 * Helper class to construct a serialization pipeline for a given result destination
 * and a given set of output properties. The pipeline is represented by a Receiver object
 * to which result tree events are sent.
 * <p>Since Saxon 8.8 is is possible to write a subclass of SerializerFactory and register it
 * with the Configuration, allowing customisation of the Serializer pipeline.</p>
 * <p>The class includes methods for instantiating each of the components used on the Serialization
 * pipeline. This allows a customized SerializerFactory to replace any or all of these components
 * by subclasses that refine the behaviour.</p>
 */
object SerializerFactory {
  @throws[XPathException]
   def checkYesOrNo(key: String, value: String): String =
    if ("yes" == value || "true" == value || "1" == value)
      "yes"
    else if ("no" == value || "false" == value || "0" == value)
      "no"
    else
      throw new XPathException("Serialization parameter " + Err.wrap(key) + " must have the value yes|no, true|false, or 1|0", "SEPM0016")

  @throws[XPathException]
  private def checkNormalizationForm(value: String): Unit =
    if (!NameChecker.isValidNmtoken(value))
      throw new XPathException("Invalid value for normalization-form: " + "must be NFC, NFD, NFKC, NFKD, fully-normalized, or none", "SEPM0016")

  private def isValidEQName(value: String): Boolean = {
    Objects.requireNonNull(value)
    if (value.isEmpty || !value.startsWith("Q{"))
      return false
    val closer = value.indexOf('}', 2)
    closer >= 2 && closer != value.length - 1 && NameChecker.isValidNCName(value.substring(closer + 1))
  }

  private def isValidClarkName(/*@NotNull*/ value: String) =
    if (value.startsWith("{"))
      isValidEQName("Q" + value)
    else
      isValidEQName("Q{}" + value)

  @throws[XPathException]
   def checkNonNegativeInteger(key: String, value: String): Unit =
    try {
      val n = value.toInt
      if (n < 0)
        throw new XPathException("Value of " + Err.wrap(key) + " must be a non-negative integer", "SEPM0016")
    } catch {
      case _: NumberFormatException =>
        throw new XPathException("Value of " + Err.wrap(key) + " must be a non-negative integer", "SEPM0016")
    }

  @throws[XPathException]
  private def checkDecimal(key: String, value: String): Unit =
    if (!BigDecimalValue.castableAsDecimal(value))
      throw new XPathException("Value of " + Err.wrap(key) + " must be a decimal number", "SEPM0016")

  @throws[XPathException]
   def checkListOfEQNames(key: String, value: String): String = {
    val tok = new StringTokenizer(value, " \t\n\r", false)
    val builder = new StringBuilder
    while (tok.hasMoreTokens) {
      val s = tok.nextToken
      if (isValidEQName(s) || NameChecker.isValidNCName(s))
        builder.append(s)
      else if (isValidClarkName(s)) if (s.startsWith("{"))
        builder.append("Q").append(s)
      else
        builder.append("Q{}").append(s)
      else
        throw new XPathException("Value of " + Err.wrap(key) + " must be a list of QNames in 'Q{uri}local' notation", "SEPM0016")
      builder.append(" ")
    }
    builder.toString
  }

  @throws[XPathException]
   def checkListOfEQNamesAllowingStar(key: String, value: String): String = {
    val builder = new StringBuilder
    val tok = new StringTokenizer(value, " \t\n\r", false)
    while (tok.hasMoreTokens) {
      val s = tok.nextToken
      if ("*" == s || isValidEQName(s) || NameChecker.isValidNCName(s))
        builder.append(s)
      else if (isValidClarkName(s)) if (s.startsWith("{"))
        builder.append("Q").append(s)
      else
        builder.append("Q{}").append(s)
      else
        throw new XPathException("Value of " + Err.wrap(key) + " must be a list of QNames in 'Q{uri}local' notation", "SEPM0016")
      builder.append(" ")
    }
    builder.toString.trim
  }

  private val publicIdPattern = Pattern.compile("^[\\s\\r\\na-zA-Z0-9\\-'()+,./:=?;!*#@$_%]*$")

  @throws[XPathException]
  private def checkPublicIdentifier(value: String): Unit =
    if (!publicIdPattern.matcher(value).matches)
      throw new XPathException("Invalid character in doctype-public parameter", "SEPM0016")

  @throws[XPathException]
  private def checkSystemIdentifier(value: String): Unit =
    if (value.contains("'") && value.contains("\""))
      throw new XPathException("The doctype-system parameter must not contain both an apostrophe and a quotation mark", "SEPM0016")

  /**
   * Process a serialization property whose value is a list of element names, for example cdata-section-elements
   *
   * @param value        The value of the property as written
   * @param nsResolver   The namespace resolver to use; may be null if prevalidated is set or if names are supplied
   *                     in Clark format
   * @param useDefaultNS True if the namespace resolver should be used for unprefixed names; false if
   *                     unprefixed names should be considered to be in no namespace
   * @param prevalidated true if the property has already been validated
   * @param errorCode    The error code to return in the event of problems
   * @return The list of element names with lexical QNames replaced by Clark names, starting with a single space
   * @throws XPathException if any error is found in the list of element names, for example, an undeclared namespace prefix
   */
  @throws[XPathException]
  def parseListOfNodeNames(value: String, nsResolver: NamespaceResolver, useDefaultNS: Boolean, prevalidated: Boolean, errorCode: String): String = {
    val s = new StringBuilder
    val st = new StringTokenizer(value, " \t\n\r", false)
    while (st.hasMoreTokens) {
      val displayname = st.nextToken
      if (prevalidated || (nsResolver == null))
        s.append(' ').append(displayname)
      else if (displayname.startsWith("Q{"))
        s.append(' ').append(displayname.substring(1))
      else
        try {
          val parts = NameChecker.getQNameParts(displayname)
          val muri = nsResolver.getURIForPrefix(parts(0), useDefaultNS)
          if (muri == null)
            throw new XPathException("Namespace prefix '" + parts(0) + "' has not been declared", errorCode)
          s.append(" {").append(muri).append('}').append(parts(1))
        } catch {
          case err: QNameException =>
            throw new XPathException("Invalid element name. " + err.getMessage, errorCode)
        }
    }
    s.toString
  }
}

class SerializerFactory {
  private[lib] var config: Configuration = null
  private[lib] var pipe: PipelineConfiguration = null

  /**
   * Create a SerializerFactory
   *
   * @param config the Saxon Configuration
   */
  def this(config: Configuration) {
    this()
    this.config = config
  }

  def this(pipe: PipelineConfiguration) {
    this()
    this.pipe = pipe
    this.config = pipe.getConfiguration
  }

  def getConfiguration: Configuration = config

  /**
   * Create a serializer with given output properties, and return
   * an XMLStreamWriter that can be used to feed events to the serializer.
   *
   * @param result     the destination of the serialized output (wraps a Writer, an OutputStream, or a File)
   * @param properties the serialization properties to be used
   * @return a serializer in the form of an XMLStreamWriter
   * @throws XPathException
   * if any error occurs
   */
  @throws[XPathException]
  def getXMLStreamWriter(result: StreamResult, properties: Properties): StreamWriterToReceiver = {
    var r = getReceiver(result, new SerializationProperties(properties))
    r = new NamespaceReducer(r)
    new StreamWriterToReceiver(r)
  }

  /**
   * Get a Receiver that wraps a given Result object. Saxon calls this method to construct
   * a serialization pipeline. The method can be overridden in a subclass; alternatively, the
   * subclass can override the various methods used to instantiate components of the serialization
   * pipeline.
   * <p>Note that this method ignores the {@link SaxonOutputKeys#WRAP} output property. If
   * wrapped output is required, the user must create a {@link org.orbeon.saxon.query.SequenceWrapper} directly.</p>
   * <p>The effect of the method changes in Saxon 9.7 so that for serialization methods other than
   * "json" and "adaptive", the returned Receiver performs the function of "sequence normalization" as
   * defined in the Serialization specification. Previously the client code handled this by wrapping the
   * result in a ComplexContentOutputter (usually as a side-effect of called XPathContext.changeOutputDestination()).
   * Wrapping in a ComplexContentOutputter is no longer necessary, though it does no harm because the ComplexContentOutputter
   * is idempotent.</p>
   *
   * <p>Changed in 9.9 so that no character maps are used. Previously the character maps from the Executable
   * associated with the Controller referenced from the PipelineConfiguration were used.</p>
   *
   * @param result The final destination of the serialized output. Usually a StreamResult,
   *               but other kinds of Result are possible.
   * @param pipe   The PipelineConfiguration.
   * @param props  The serialization properties. If this includes the property { @link SaxonOutputKeys#USE_CHARACTER_MAPS}
   *                                                                                   then the PipelineConfiguration must contain a non-null Controller, and the Executable associated with this Controller
   *                                                                                   must have a CharacterMapIndex which is used to resolve the names of the character maps appearing in this property.
   * @return the newly constructed Receiver that performs the required serialization
   * @throws XPathException
   * if any failure occurs
   * @deprecated since Saxon 9.9: use one of the other { @code getReceiver} methods
   */
  @throws[XPathException]
  def getReceiver(result: Result, pipe: PipelineConfiguration, props: Properties): Receiver =
    getReceiver(result, new SerializationProperties(props), pipe)

  /**
   * Get a Receiver that wraps a given Result object. Saxon calls this method to construct
   * a serialization pipeline. The method can be overridden in a subclass; alternatively, the
   * subclass can override the various methods used to instantiate components of the serialization
   * pipeline.
   * <p>This version of the method calls {@link #getReceiver(Result, SerializationProperties, PipelineConfiguration)}
   * supplying default output properties, and a {@code PipelineConfiguration} newly constructed using
   * {@link Configuration#makePipelineConfiguration()}.</p>
   *
   * @param result The final destination of the serialized output. Usually a StreamResult,
   *               but other kinds of Result are possible.
   * @throws XPathException if a serializer cannot be created
   */
  @throws[XPathException]
  def getReceiver(result: Result): Receiver =
    getReceiver(result, new SerializationProperties, config.makePipelineConfiguration)

  /**
   * Get a Receiver that wraps a given Result object. Saxon calls this method to construct
   * a serialization pipeline. The method can be overridden in a subclass; alternatively, the
   * subclass can override the various methods used to instantiate components of the serialization
   * pipeline.
   * <p>This version of the method calls {@link #getReceiver(Result, SerializationProperties, PipelineConfiguration)}
   * supplying a {@code PipelineConfiguration} newly constructed using {@link Configuration#makePipelineConfiguration()}.</p>
   *
   * @param result The final destination of the serialized output. Usually a StreamResult,
   *               but other kinds of Result are possible.
   * @param params The serialization properties, including character maps
   * @return the newly constructed Receiver that performs the required serialization
   * @throws XPathException if a serializer cannot be created
   */
  @throws[XPathException]
  def getReceiver(result: Result, params: SerializationProperties): Receiver =
    getReceiver(result, params, config.makePipelineConfiguration)

  /**
   * Get a Receiver that wraps a given Result object. Saxon calls this method to construct
   * a serialization pipeline. The method can be overridden in a subclass; alternatively, the
   * subclass can override the various methods used to instantiate components of the serialization
   * pipeline.
   * <p>Note that this method ignores the {@link SaxonOutputKeys#WRAP} output property. If
   * wrapped output is required, the user must create a {@link org.orbeon.saxon.query.SequenceWrapper} directly.</p>
   * <p>The effect of the method changes in Saxon 9.7 so that for serialization methods other than
   * "json" and "adaptive", the returned Receiver performs the function of "sequence normalization" as
   * defined in the Serialization specification. Previously the client code handled this by wrapping the
   * result in a ComplexContentOutputter (usually as a side-effect of called XPathContext.changeOutputDestination()).
   * Wrapping in a ComplexContentOutputter is no longer necessary, though it does no harm because the ComplexContentOutputter
   * is idempotent.</p>
   *
   * @param result The final destination of the serialized output. Usually a StreamResult,
   *               but other kinds of Result are possible.
   * @param params The serialization properties, including character maps
   * @param pipe   The PipelineConfiguration.
   * @return the newly constructed Receiver that performs the required serialization
   * @throws XPathException if a serializer cannot be created
   */
  @throws[XPathException]
  def getReceiver(result: Result, params: SerializationProperties, pipe: PipelineConfiguration): Receiver = {

    Objects.requireNonNull(result)
    Objects.requireNonNull(params)
    Objects.requireNonNull(pipe)

    var props = params.getProperties
    var charMapIndex = params.getCharacterMapIndex
    if (charMapIndex == null)
      charMapIndex = new CharacterMapIndex
    val nextInChain = props.getProperty(SaxonOutputKeys.NEXT_IN_CHAIN)
    if (nextInChain != null && nextInChain.nonEmpty) {
      val href = props.getProperty(SaxonOutputKeys.NEXT_IN_CHAIN)
      var base = props.getProperty(SaxonOutputKeys.NEXT_IN_CHAIN_BASE_URI)
      if (base == null)
        base = ""
      val sansNext = new Properties(props)
      sansNext.setProperty(SaxonOutputKeys.NEXT_IN_CHAIN, "")
      return prepareNextStylesheet(pipe, href, base, result)
    }
    val paramDoc = props.getProperty(SaxonOutputKeys.PARAMETER_DOCUMENT)
    if (paramDoc != null) {
      var base = props.getProperty(SaxonOutputKeys.PARAMETER_DOCUMENT_BASE_URI)
      if (base == null) base = result.getSystemId
      val props2 = new Properties(props)
      var source: Source = null
      try source = config.getURIResolver.resolve(paramDoc, base)
      catch {
        case e: TransformerException =>
          throw XPathException.makeXPathException(e)
      }
      val options = new ParseOptions
      options.setSchemaValidationMode(Validation.LAX)
      options.setDTDValidationMode(Validation.SKIP)
      val doc = config.buildDocumentTree(source)
      val ph = new SerializationParamsHandler
      ph.setSerializationParams(doc.getRootNode)
      val paramDocProps = ph.getSerializationProperties.getProperties
      val names = paramDocProps.propertyNames
      while (names.hasMoreElements) {
        val name = names.nextElement.asInstanceOf[String]
        val value = paramDocProps.getProperty(name)
        props2.setProperty(name, value)
      }
      val charMap = ph.getCharacterMap
      if (charMap != null) {
        props2.setProperty(SaxonOutputKeys.USE_CHARACTER_MAPS, charMap.getName.getClarkName)
        charMapIndex.putCharacterMap(charMap.getName, charMap)
      }
      props = props2
    }
    result match {
      case sr: StreamResult =>

        // The "target" is the start of the output pipeline, the Receiver that
        // instructions will actually write to (except that other things like a
        // NamespaceReducer may get added in front of it). The "emitter" is the
        // last thing in the output pipeline, the Receiver that actually generates
        // characters or bytes that are written to the StreamResult.
        var target: SequenceReceiver = null
        val method                   = props.getProperty(OutputKeys.METHOD)
        println(s"xxx saxon StreamResult $method")

        if (method == null)
          return newUncommittedSerializer(result, new Sink(pipe), params)

        println(s"xxx saxon method $method")

        var emitter: Emitter = null
        method match {
          case "html"     =>
            emitter = newHTMLEmitter(props)
            emitter.setPipelineConfiguration(pipe)
            target = createHTMLSerializer(emitter, params, pipe)
          case "xml"      =>
            emitter = newXMLEmitter(props)
            emitter.setPipelineConfiguration(pipe)
            target = createXMLSerializer(emitter.asInstanceOf[XMLEmitter], params)
          case "xhtml"    =>
            emitter = newXHTMLEmitter(props)
            emitter.setPipelineConfiguration(pipe)
            target = createXHTMLSerializer(emitter, params, pipe)
          case "text"     =>
            emitter = newTEXTEmitter
            emitter.setPipelineConfiguration(pipe)
            target = createTextSerializer(emitter, params)
          case "json"     =>
            props.setProperty(OutputKeys.OMIT_XML_DECLARATION, "yes")
            val je        = new JSONEmitter(pipe, sr, props)
            val js        = new JSONSerializer(pipe, je, props)
            val sortOrder = props.getProperty(SaxonOutputKeys.PROPERTY_ORDER)
            if (sortOrder != null)
              js.setPropertySorter(getPropertySorter(sortOrder))
            val characterMapExpander = makeCharacterMapExpander(pipe, props, charMapIndex)
            val normalizer           = makeUnicodeNormalizer(pipe, props)
            return customizeJSONSerializer(js, props, characterMapExpander, normalizer)
          case "adaptive" =>
            ???
          // ORBEON: No `File` support.
          //          val esr = new ExpandedStreamResult(pipe.getConfiguration, result.asInstanceOf[StreamResult], props)
          //          val writer = esr.obtainWriter
          //          val je = new AdaptiveEmitter(pipe, writer)
          //          je.setOutputProperties(props)
          //          val characterMapExpander = makeCharacterMapExpander(pipe, props, charMapIndex)
          //          val normalizer = makeUnicodeNormalizer(pipe, props)
          //          return customizeAdaptiveSerializer(je, props, characterMapExpander, normalizer)
          case _ =>
            if (method.startsWith("{" + NamespaceConstant.SAXON + "}")) {
              val characterMapExpander = makeCharacterMapExpander(pipe, props, charMapIndex)
              val normalizer           = makeUnicodeNormalizer(pipe, props)
              target = createSaxonSerializationMethod(method, params, pipe, characterMapExpander, normalizer, sr)
              target match {
                case e: Emitter => emitter = e
                case _ =>
              }
            }
            else {
              var userReceiver: SequenceReceiver = null
              userReceiver = createUserDefinedOutputMethod(method, props, pipe)
              userReceiver match {
                case e: Emitter =>
                  emitter = e
                  target  = params.makeSequenceNormalizer(emitter)
                case _ => return params.makeSequenceNormalizer(userReceiver)
              }
            }
        }
        if (emitter != null) {
          emitter.setOutputProperties(props)
          emitter.setStreamResult(sr)
        }
        //target = new RegularSequenceChecker(target); // add this back in for diagnostics only
        target.setSystemId(result.getSystemId)
        target
      case _                => // Handle results other than StreamResult: these generally do not involve serialization
        getReceiverForNonSerializedResult(result, props, pipe)
    }
  }

  @throws[XPathException]
  private def makeUnicodeNormalizer(pipe: PipelineConfiguration, props: Properties): ProxyReceiver = {
    val normForm = props.getProperty(SaxonOutputKeys.NORMALIZATION_FORM)
    if (normForm != null && !(normForm == "none"))
      newUnicodeNormalizer(new Sink(pipe), props)
    else
      null
  }

  @throws[XPathException]
  private def makeCharacterMapExpander(pipe: PipelineConfiguration, props: Properties, charMapIndex: CharacterMapIndex): CharacterMapExpander = {
    val useMaps = props.getProperty(SaxonOutputKeys.USE_CHARACTER_MAPS)
    if (useMaps != null)
      charMapIndex.makeCharacterMapExpander(useMaps, new Sink(pipe), this)
    else
      null
  }

  /**
   * Get a Receiver to handle a result other than a StreamResult. This will generally not involve
   * serialization.
   *
   * @param result the destination
   * @param props  the serialization parameters (which in most cases will be ignored)
   * @param pipe   the pipeline configuration
   * @return a suitable receiver to accept the raw query or transformation results
   * @throws XPathException if any failure occurs
   */
  @throws[XPathException]
  private def getReceiverForNonSerializedResult(result: Result, props: Properties, pipe: PipelineConfiguration): Receiver = {
    if (result.isInstanceOf[Emitter]) {
      if (result.asInstanceOf[Emitter].getOutputProperties == null) result.asInstanceOf[Emitter].setOutputProperties(props)
      return result.asInstanceOf[Emitter]
    }
    else if (result.isInstanceOf[JSONSerializer]) {
      if (result.asInstanceOf[JSONSerializer].getOutputProperties == null) result.asInstanceOf[JSONSerializer].setOutputProperties(props)
      return result.asInstanceOf[JSONSerializer]
    }
    else if (result.isInstanceOf[AdaptiveEmitter]) {
      if (result.asInstanceOf[AdaptiveEmitter].getOutputProperties == null) result.asInstanceOf[AdaptiveEmitter].setOutputProperties(props)
      return result.asInstanceOf[AdaptiveEmitter]
    }
    else if (result.isInstanceOf[Receiver]) {
      val receiver = result.asInstanceOf[Receiver]
      receiver.setSystemId(result.getSystemId)
      receiver.setPipelineConfiguration(pipe)
      if (result.asInstanceOf[Receiver].handlesAppend && "no" == props.getProperty(SaxonOutputKeys.BUILD_TREE)) {
        return receiver
        // TODO: handle item-separator
      }
      else return new TreeReceiver(receiver)
    }
    else if (result.isInstanceOf[SAXResult]) {
      val proxy = newContentHandlerProxy
      proxy.setUnderlyingContentHandler(result.asInstanceOf[SAXResult].getHandler)
      proxy.setPipelineConfiguration(pipe)
      proxy.setOutputProperties(props)
      if ("yes" == props.getProperty(SaxonOutputKeys.SUPPLY_SOURCE_LOCATOR)) if (config.isCompileWithTracing && pipe.getController != null) pipe.getController.addTraceListener(proxy.getTraceListener)
      else throw new XPathException("Cannot use saxon:supply-source-locator unless tracing was enabled at compile time", SaxonErrorCode.SXSE0002)
      //proxy.open();
      return makeSequenceNormalizer(proxy, props)
    }
    //ORBEON: No StAX support.
//    else if (result.isInstanceOf[StAXResult]) {
//      val handler = new StAXResultHandlerImpl
//      val r = handler.getReceiver(result, props)
//      r.setPipelineConfiguration(pipe)
//      return makeSequenceNormalizer(r, props)
//    }
    else if (pipe != null) { // try to find an external object model that knows this kind of Result
      val externalObjectModels = pipe.getConfiguration.getExternalObjectModels

      for (externalObjectModel <- externalObjectModels.asScala) {
        val model = externalObjectModel.asInstanceOf[ExternalObjectModel]
        val builder = model.getDocumentBuilder(result)
        if (builder != null) {
          builder.setSystemId(result.getSystemId)
          builder.setPipelineConfiguration(pipe)
          return new TreeReceiver(builder)
        }
      }
    }
    throw new IllegalArgumentException("Unknown type of result: " + result.getClass)
  }

  def makeSequenceNormalizer(receiver: Receiver, properties: Properties): SequenceReceiver = {
    val method = properties.getProperty(OutputKeys.METHOD)
    if ("json" == method || "adaptive" == method) if (receiver.isInstanceOf[SequenceReceiver]) receiver.asInstanceOf[SequenceReceiver]
    else new TreeReceiver(receiver)
    else {
      val pipe = receiver.getPipelineConfiguration
      var result: SequenceReceiver = null
      val separator = properties.getProperty(SaxonOutputKeys.ITEM_SEPARATOR)
      if (separator == null || "#absent" == separator) result = new SequenceNormalizerWithSpaceSeparator(receiver)
      else result = new SequenceNormalizerWithItemSeparator(receiver, separator)
      result.setPipelineConfiguration(pipe)
      result
    }
  }

  /**
   * Create a serialization pipeline to implement the HTML output method. This method is
   * so that it can be customized in a user-written SerializerFactory
   *
   * @param emitter the emitter at the end of the pipeline (created using the method { @link #newHTMLEmitter}
   * @param params the serialization properties
   * @param pipe   the pipeline configuration information
   * @return a Receiver acting as the entry point to the serialization pipeline
   * @throws XPathException if a failure occurs
   */
  @throws[XPathException]
   def createHTMLSerializer(emitter: Emitter, params: SerializationProperties, pipe: PipelineConfiguration): SequenceReceiver = {
    var target:Receiver = null
    target = emitter
    val props = params.getProperties
    if (!("no" == props.getProperty(OutputKeys.INDENT))) target = newHTMLIndenter(target, props)
    target = new NamespaceDifferencer(target, props)
    target = injectUnicodeNormalizer(params, target)
    target = injectCharacterMapExpander(params, target, useNullMarkers = true)
    val cdataElements = props.getProperty(OutputKeys.CDATA_SECTION_ELEMENTS)
    if (cdataElements != null && !cdataElements.isEmpty) target = newCDATAFilter(target, props)
    if (SaxonOutputKeys.isHtmlVersion5(props)) target = addHtml5Component(target, props)
    if (!("no" == props.getProperty(SaxonOutputKeys.ESCAPE_URI_ATTRIBUTES))) target = newHTMLURIEscaper(target, props)
    if (!("no" == props.getProperty(SaxonOutputKeys.INCLUDE_CONTENT_TYPE))) target = newHTMLMetaTagAdjuster(target, props)
    val attributeOrder = props.getProperty(SaxonOutputKeys.ATTRIBUTE_ORDER)
    if (attributeOrder != null && !attributeOrder.isEmpty) target = newAttributeSorter(target, props)
    if (params.getValidationFactory != null) target = params.getValidationFactory.makeFilter(target)
    makeSequenceNormalizer(target, props)
  }

  /**
   * Create a serialization pipeline to implement the text output method. This method is
   * so that it can be customized in a user-written SerializerFactory
   *
   * @param emitter the emitter at the end of the pipeline (created using the method { @link #newTEXTEmitter}
   * @param params the serialization properties
   * @return a Receiver acting as the entry point to the serialization pipeline
   * @throws XPathException if a failure occurs
   */
  @throws[XPathException]
   def createTextSerializer(emitter: Emitter, params: SerializationProperties): SequenceReceiver = {
    val props = params.getProperties
    var target: Receiver = null
    target = injectUnicodeNormalizer(params, emitter)
    target = injectCharacterMapExpander(params, target, useNullMarkers = false)
    target = addTextOutputFilter(target, props)
    if (params.getValidationFactory != null) target = params.getValidationFactory.makeFilter(target)
    makeSequenceNormalizer(target, props)
  }

  /**
   * Create a serialization pipeline to implement the JSON output method. This method is
   * so that it can be customized in a user-written SerializerFactory
   *
   * @param emitter the emitter at the end of the pipeline (created using the method { @link #newTEXTEmitter}
   * @param props                the serialization properties
   * @param characterMapExpander the filter to be used for expanding character maps defined in the stylesheet
   * @param normalizer           the filter used for Unicode normalization
   * @return a Receiver acting as the entry point to the serialization pipeline
   */
  @throws[XPathException]
   def customizeJSONSerializer(emitter: JSONSerializer, props: Properties, characterMapExpander: CharacterMapExpander, normalizer: ProxyReceiver): JSONSerializer = {
    if (normalizer.isInstanceOf[UnicodeNormalizer]) emitter.setNormalizer(normalizer.asInstanceOf[UnicodeNormalizer].getNormalizer)
    if (characterMapExpander != null) emitter.setCharacterMap(characterMapExpander.getCharacterMap)
    emitter
  }

  /**
   * Create a serialization pipeline to implement the Adaptive output method. This method is
   * so that it can be customized in a user-written SerializerFactory
   *
   * @param emitter              the emitter at the end of the pipeline
   * @param props                the serialization properties
   * @param characterMapExpander the filter to be used for expanding character maps defined in the stylesheet
   * @param normalizer           the filter used for Unicode normalization
   * @return a Receiver acting as the entry point to the serialization pipeline
   */
   def customizeAdaptiveSerializer(emitter: AdaptiveEmitter, props: Properties, characterMapExpander: CharacterMapExpander, normalizer: ProxyReceiver): AdaptiveEmitter = {
    if (normalizer.isInstanceOf[UnicodeNormalizer]) emitter.setNormalizer(normalizer.asInstanceOf[UnicodeNormalizer].getNormalizer)
    if (characterMapExpander != null) emitter.setCharacterMap(characterMapExpander.getCharacterMap)
    emitter
  }

  /**
   * Create a serialization pipeline to implement the XHTML output method. This method is
   * so that it can be customized in a user-written SerializerFactory
   *
   * @param emitter the emitter at the end of the pipeline (created using the method { @link #newXHTMLEmitter}
   * @param params the serialization properties
   * @param pipe   the pipeline configuration information
   * @return a Receiver acting as the entry point to the serialization pipeline
   * @throws XPathException if a failure occurs
   */
  @throws[XPathException]
   def createXHTMLSerializer(emitter: Emitter, params: SerializationProperties, pipe: PipelineConfiguration): SequenceReceiver = {
    var target: Receiver = emitter
    val props = params.getProperties
    if (!("no" == props.getProperty(OutputKeys.INDENT))) target = newXHTMLIndenter(target, props)
    target = new NamespaceDifferencer(target, props)
    target = injectUnicodeNormalizer(params, target)
    target = injectCharacterMapExpander(params, target, useNullMarkers = true)
    val cdataElements = props.getProperty(OutputKeys.CDATA_SECTION_ELEMENTS)
    if (cdataElements != null && !cdataElements.isEmpty) target = newCDATAFilter(target, props)
    if (SaxonOutputKeys.isXhtmlHtmlVersion5(props)) target = addHtml5Component(target, props)
    if (!("no" == props.getProperty(SaxonOutputKeys.ESCAPE_URI_ATTRIBUTES))) target = newXHTMLURIEscaper(target, props)
    if (!("no" == props.getProperty(SaxonOutputKeys.INCLUDE_CONTENT_TYPE))) target = newXHTMLMetaTagAdjuster(target, props)
    val attributeOrder = props.getProperty(SaxonOutputKeys.ATTRIBUTE_ORDER)
    if (attributeOrder != null && !attributeOrder.isEmpty) target = newAttributeSorter(target, props)
    if (params.getValidationFactory != null) target = params.getValidationFactory.makeFilter(target)
    makeSequenceNormalizer(target, props)
  }

  /**
   * This method constructs a step in the output pipeline to perform namespace-related
   * tasks for HTML5 serialization. The default implementation adds a NamespaceReducer
   * and an XHTMLPrefixRemover
   *
   * @param target           the Receiver that receives the output of this step
   * @param outputProperties the serialization properties
   * @return a new Receiver to perform HTML5-related namespace manipulation
   */
  def addHtml5Component(target: Receiver, outputProperties: Properties): Receiver = {
    var targetReceiver = target
    targetReceiver = new NamespaceReducer(targetReceiver)
    targetReceiver = new XHTMLPrefixRemover(targetReceiver)
    targetReceiver
  }

  /**
   * Create a serialization pipeline to implement the XML output method. This method is
   * so that it can be customized in a user-written SerializerFactory
   *
   * @param emitter the emitter at the end of the pipeline (created using the method { @link #newXMLEmitter}
   * @param params the serialization properties
   * @return a Receiver acting as the entry point to the serialization pipeline
   * @throws XPathException if a failure occurs
   */
  @throws[XPathException]
  def createXMLSerializer(emitter: XMLEmitter, params: SerializationProperties): SequenceReceiver = {
    var target: Receiver = null
    val props = params.getProperties
    val canonical = "yes" == props.getProperty(SaxonOutputKeys.CANONICAL)
    if ("yes" == props.getProperty(OutputKeys.INDENT) || canonical) target = newXMLIndenter(emitter, props)
    else target = emitter
    target = new NamespaceDifferencer(target, props)
    if ("1.0" == props.getProperty(OutputKeys.VERSION) && config.getXMLVersion == Configuration.XML11) { // Check result meets XML 1.0 constraints if configuration allows XML 1.1 input but
      // this result document must conform to 1.0
      target = newXML10ContentChecker(target, props)
    }
    target = injectUnicodeNormalizer(params, target)
    if (!canonical) target = injectCharacterMapExpander(params, target, useNullMarkers = true)
    val cdataElements = props.getProperty(OutputKeys.CDATA_SECTION_ELEMENTS)
    if (cdataElements != null && !cdataElements.isEmpty && !canonical) target = newCDATAFilter(target, props)
    if (canonical) {
      target = newAttributeSorter(target, props)
      target = newNamespaceSorter(target, props)
    }
    else {
      val attributeOrder = props.getProperty(SaxonOutputKeys.ATTRIBUTE_ORDER)
      if (attributeOrder != null && !attributeOrder.isEmpty) target = newAttributeSorter(target, props)
    }
    if (params.getValidationFactory != null) target = params.getValidationFactory.makeFilter(target)
    makeSequenceNormalizer(target, props)
  }

  @throws[XPathException]
  def createSaxonSerializationMethod(
    method               : String,
    params               : SerializationProperties,
    pipe                 : PipelineConfiguration,
    characterMapExpander : CharacterMapExpander,
    normalizer           : ProxyReceiver,
    result               : StreamResult
  ) = throw new XPathException("Saxon serialization methods require Saxon-PE to be enabled")

  /**
   * Create a serialization pipeline to implement a user-defined output method. This method is
   * so that it can be customized in a user-written SerializerFactory
   *
   * @param method the name of the user-defined output method, as a QName in Clark format
   *               (that is "{uri}local").
   * @param props  the serialization properties
   * @param pipe   the pipeline configuration information
   * @return a Receiver acting as the entry point to the serialization pipeline
   * @throws XPathException if a failure occurs
   */
  @throws[XPathException]
   def createUserDefinedOutputMethod(method: String, props: Properties, pipe: PipelineConfiguration): SequenceReceiver = {
    var userReceiver: Receiver = null // See if this output method is recognized by the Configuration
    userReceiver = pipe.getConfiguration.makeEmitter(method, props)
    userReceiver.setPipelineConfiguration(pipe)
    if (userReceiver.isInstanceOf[ContentHandlerProxy] && "yes" == props.getProperty(SaxonOutputKeys.SUPPLY_SOURCE_LOCATOR)) if (pipe.getConfiguration.isCompileWithTracing && pipe.getController != null) pipe.getController.addTraceListener(userReceiver.asInstanceOf[ContentHandlerProxy].getTraceListener)
    else throw new XPathException("Cannot use saxon:supply-source-locator unless tracing was enabled at compile time", SaxonErrorCode.SXSE0002)
    if (userReceiver.isInstanceOf[SequenceReceiver]) userReceiver.asInstanceOf[SequenceReceiver]
    else new TreeReceiver(userReceiver)
  }

  @throws[XPathException]
  def injectCharacterMapExpander(params: SerializationProperties, out: Receiver, useNullMarkers: Boolean): Receiver = {
    val charMapIndex = params.getCharacterMapIndex
    if (charMapIndex != null) {
      val useMaps = params.getProperties.getProperty(SaxonOutputKeys.USE_CHARACTER_MAPS)
      if (useMaps != null) {
        val expander = charMapIndex.makeCharacterMapExpander(useMaps, out, this)
        expander.setUseNullMarkers(useNullMarkers)
        return expander
      }
    }
    out
  }

  @throws[XPathException]
  def injectUnicodeNormalizer(params: SerializationProperties, out: Receiver): Receiver = {
    val props = params.getProperties
    val normForm = props.getProperty(SaxonOutputKeys.NORMALIZATION_FORM)
    if (normForm != null && !(normForm == "none")) return newUnicodeNormalizer(out, props)
    out
  }

  /**
   * Create a ContentHandlerProxy. This method exists so that it can be overridden in a subclass.
   *
   * @return the newly created ContentHandlerProxy.
   */
  def newContentHandlerProxy = new ContentHandlerProxy

  /**
   * Create an UncommittedSerializer. This method exists so that it can be overridden in a subclass.
   *
   * @param result the result destination
   * @param next   the next receiver in the pipeline
   * @param params the serialization parameters
   * @return the newly created UncommittedSerializer.
   */
  def newUncommittedSerializer(result: Result, next: Receiver, params: SerializationProperties) = new UncommittedSerializer(result, next, params)

  /**
   * Create a new XML Emitter. This method exists so that it can be overridden in a subclass.
   *
   * @param properties the output properties
   * @return the newly created XML emitter.
   */
  def newXMLEmitter(properties: Properties) = new XMLEmitter

  /**
   * Create a new HTML Emitter. This method exists so that it can be overridden in a subclass.
   *
   * @param properties the output properties
   * @return the newly created HTML emitter.
   */
  def newHTMLEmitter(properties: Properties): HTMLEmitter = {
    var emitter: HTMLEmitter = null
    // Note, we recognize html-version even when running XSLT 2.0.
    if (SaxonOutputKeys.isHtmlVersion5(properties))
      emitter = new HTML50Emitter
    else
      emitter = new HTML40Emitter
    emitter
  }

  /**
   * Create a new XHTML Emitter. This method exists so that it can be overridden in a subclass.
   *
   * @param properties the output properties
   * @return the newly created XHTML emitter.
   */
  def newXHTMLEmitter(properties: Properties): XMLEmitter = {
    val is5 = SaxonOutputKeys.isXhtmlHtmlVersion5(properties)
    if (is5)
      new XHTML5Emitter
    else
      new XHTML1Emitter
  }

  /**
   * Add a filter to the text output method pipeline. This does nothing unless overridden
   * in a subclass
   *
   * @param next       the next receiver (typically the TextEmitter)
   * @param properties the output properties
   * @return the receiver to be used in place of the "next" receiver
   * @throws XPathException if the operation fails
   */
  @throws[XPathException]
  def addTextOutputFilter(next: Receiver, properties: Properties): Receiver = next

  /**
   * Create a new Text Emitter. This method exists so that it can be overridden in a subclass.
   *
   * @return the newly created text emitter.
   */
  def newTEXTEmitter = new TEXTEmitter

  /**
   * Create a new XML Indenter. This method exists so that it can be overridden in a subclass.
   *
   * @param next             the next receiver in the pipeline
   * @param outputProperties the serialization parameters
   * @return the newly created XML indenter.
   */
  def newXMLIndenter(next: XMLEmitter, outputProperties: Properties): XMLIndenter = {
    val r = new XMLIndenter(next)
    r.setOutputProperties(outputProperties)
    r
  }

  /**
   * Create a new HTML Indenter. This method exists so that it can be overridden in a subclass.
   *
   * @param next             the next receiver in the pipeline
   * @param outputProperties the serialization parameters
   * @return the newly created HTML indenter.
   */
  def newHTMLIndenter(next: Receiver, outputProperties: Properties): HTMLIndenter = {
    val r = new HTMLIndenter(next, "html")
    r.setOutputProperties(outputProperties)
    r
  }

  /**
   * Create a new XHTML Indenter. This method exists so that it can be overridden in a subclass.
   *
   * @param next             the next receiver in the pipeline
   * @param outputProperties the serialization parameters
   * @return the newly created XHTML indenter.
   */
  def newXHTMLIndenter(next: Receiver, outputProperties: Properties): HTMLIndenter = {
    var method = "xhtml"
    val htmlVersion = outputProperties.getProperty("html-version")
    if (htmlVersion != null && htmlVersion.startsWith("5")) method = "xhtml5"
    val r = new HTMLIndenter(next, method)
    r.setOutputProperties(outputProperties)
    r
  }

  /**
   * Create a new XHTML MetaTagAdjuster, responsible for insertion, removal, or replacement of meta
   * elements. This method exists so that it can be overridden in a subclass.
   *
   * @param next             the next receiver in the pipeline
   * @param outputProperties the serialization parameters
   * @return the newly created XHTML MetaTagAdjuster.
   */
  def newXHTMLMetaTagAdjuster(next: Receiver, outputProperties: Properties): MetaTagAdjuster = {
    val r = new MetaTagAdjuster(next)
    r.setIsXHTML(true)
    r.setOutputProperties(outputProperties)
    r
  }

  /**
   * Create a new XHTML MetaTagAdjuster, responsible for insertion, removal, or replacement of meta
   * elements. This method exists so that it can be overridden in a subclass.
   *
   * @param next             the next receiver in the pipeline
   * @param outputProperties the serialization parameters
   * @return the newly created HTML MetaTagAdjuster.
   */
  def newHTMLMetaTagAdjuster(next: Receiver, outputProperties: Properties): MetaTagAdjuster = {
    val r = new MetaTagAdjuster(next)
    r.setIsXHTML(false)
    r.setOutputProperties(outputProperties)
    r
  }

  /**
   * Create a new HTML URI Escaper, responsible for percent-encoding of URIs in
   * HTML output documents. This method exists so that it can be overridden in a subclass.
   *
   * @param next             the next receiver in the pipeline
   * @param outputProperties the serialization parameters
   * @return the newly created HTML URI escaper.
   */
  def newHTMLURIEscaper(next: Receiver, outputProperties: Properties) = new HTMLURIEscaper(next)

  /**
   * Create a new XHTML URI Escaper, responsible for percent-encoding of URIs in
   * HTML output documents. This method exists so that it can be overridden in a subclass.
   *
   * @param next             the next receiver in the pipeline
   * @param outputProperties the serialization parameters
   * @return the newly created HTML URI escaper.
   */
  def newXHTMLURIEscaper(next: Receiver, outputProperties: Properties) = new XHTMLURIEscaper(next)

  /**
   * Create a new CDATA Filter, responsible for insertion of CDATA sections where required.
   * This method exists so that it can be overridden in a subclass.
   *
   * @param next             the next receiver in the pipeline
   * @param outputProperties the serialization parameters
   * @return the newly created CDATA filter.
   * @throws XPathException
   * if an error occurs
   */
  @throws[XPathException]
  def newCDATAFilter(next: Receiver, outputProperties: Properties): CDATAFilter = {
    val r = new CDATAFilter(next)
    r.setOutputProperties(outputProperties)
    r
  }

  /**
   * Create a new AttributeSorter, responsible for sorting of attributes into a specified order.
   * This method exists so that it can be overridden in a subclass. The Saxon-HE version of
   * this method returns the supplied receiver unchanged (attribute sorting is not supported
   * in Saxon-HE). The AttributeSorter handles both sorting of attributes into a user-specified
   * order (saxon:attribute-order) and sorting into C14N order (saxon:canonical).
   *
   * @param next             the next receiver in the pipeline
   * @param outputProperties the serialization parameters
   * @return the newly created filter.
   */
  @throws[XPathException]
  def newAttributeSorter(next: Receiver, outputProperties: Properties): Receiver = next

  /**
   * Create a new NamespaceSorter, responsible for sorting of namespaces into a specified order.
   *
   * @param next             the next receiver in the pipeline
   * @param outputProperties the serialization parameters
   * @return the newly created filter.
   */
  @throws[XPathException]
  def newNamespaceSorter(next: Receiver, outputProperties: Properties): Receiver = next

  /**
   * Create a new XML 1.0 content checker, responsible for checking that the output conforms to
   * XML 1.0 rules (this is used only if the Configuration supports XML 1.1 but the specific output
   * file requires XML 1.0). This method exists so that it can be overridden in a subclass.
   *
   * @param next             the next receiver in the pipeline
   * @param outputProperties the serialization parameters
   * @return the newly created XML 1.0 content checker.
   */
  def newXML10ContentChecker(next: Receiver, outputProperties: Properties) = new XML10ContentChecker(next)

  /**
   * Create a Unicode Normalizer. This method exists so that it can be overridden in a subclass.
   *
   * @param next             the next receiver in the pipeline
   * @param outputProperties the serialization parameters
   * @return the newly created Unicode normalizer.
   * @throws XPathException
   * if an error occurs
   */
  @throws[XPathException]
  def newUnicodeNormalizer(next: Receiver, outputProperties: Properties): UnicodeNormalizer = {
    val normForm = outputProperties.getProperty(SaxonOutputKeys.NORMALIZATION_FORM)
    new UnicodeNormalizer(normForm, next)
  }

  /**
   * Create a new CharacterMapExpander. This method exists so that it can be overridden in a subclass.
   *
   * @param next the next receiver in the pipeline
   * @return the newly created CharacterMapExpander.
   */
  def newCharacterMapExpander(next: Receiver) = new CharacterMapExpander(next)

  /**
   * Prepare another stylesheet to handle the output of this one.
   * <p>This method is intended for internal use, to support the
   * <code>saxon:next-in-chain</code> extension.</p>
   *
   * @param pipe    the current transformation
   * @param href    URI of the next stylesheet to be applied
   * @param baseURI base URI for resolving href if it's a relative
   *                URI
   * @param result  the output destination of the current stylesheet
   * @return a replacement destination for the current stylesheet
   * @throws XPathException if any dynamic error occurs
   */
  @throws[XPathException]
  def prepareNextStylesheet(pipe: PipelineConfiguration, href: String, baseURI: String, result: Result): Null = {
    pipe.getConfiguration.checkLicensedFeature(Configuration.LicenseFeature.PROFESSIONAL_EDITION, "saxon:next-in-chain", -1)
    null
  }

  /**
   * Get a SequenceWrapper, a class that serializes an XDM sequence with full annotation of item types, node kinds,
   * etc. There are variants for Saxon-HE and Saxon-PE
   *
   * @param destination the place where the wrapped sequence will be sent
   * @return the new SequenceWrapper
   */
  def newSequenceWrapper(destination: Receiver): SequenceWrapper = new SequenceWrapper(destination)

  /**
   * Check that a supplied output property is valid, and normalize the value (specifically in the case of boolean
   * values where yes|true|1 are normalized to "yes", and no|false|0 are normalized to "no"). Clark names in the
   * value (<code>{uri}local</code>) are normalized to EQNames (<code>Q{uri}local</code>)
   *
   * @param key   the name of the property, in Clark format
   * @param value the value of the property. This may be set to null, in which case no validation takes place.
   *              The value must be in JAXP format, that is, with lexical QNames expanded to either EQNames or
   *              Clark names.
   * @return normalized value of the property, or null if the supplied value is null
   * @throws XPathException if the property name or value is invalid
   */
  @throws[XPathException]
  def checkOutputProperty(key: String, value: String): String = {
    var finalValue = value
    if (! key.startsWith("{")) key match {
      case SaxonOutputKeys.ALLOW_DUPLICATE_NAMES |
           SaxonOutputKeys.ESCAPE_URI_ATTRIBUTES |
           SaxonOutputKeys.INCLUDE_CONTENT_TYPE  |
           OutputKeys.INDENT                     |
           OutputKeys.OMIT_XML_DECLARATION       |
           SaxonOutputKeys.UNDECLARE_PREFIXES =>
        if (finalValue != null)
          finalValue = SerializerFactory.checkYesOrNo(key, finalValue)
      case SaxonOutputKeys.BUILD_TREE =>
        if (finalValue != null)
          finalValue = SerializerFactory.checkYesOrNo(key, finalValue)
      case SaxonOutputKeys.BYTE_ORDER_MARK =>
        if (finalValue != null)
          finalValue = SerializerFactory.checkYesOrNo(key, finalValue)
      case OutputKeys.CDATA_SECTION_ELEMENTS    |
           SaxonOutputKeys.SUPPRESS_INDENTATION |
           SaxonOutputKeys.USE_CHARACTER_MAPS =>
        if (finalValue != null)
          finalValue = SerializerFactory.checkListOfEQNames(key, finalValue)
      case OutputKeys.DOCTYPE_PUBLIC =>
        if (finalValue != null)
          SerializerFactory.checkPublicIdentifier(finalValue)
      case OutputKeys.DOCTYPE_SYSTEM =>
        if (finalValue != null)
          SerializerFactory.checkSystemIdentifier(finalValue)
      case OutputKeys.ENCODING =>
      // no constraints
      case SaxonOutputKeys.HTML_VERSION =>
        if (finalValue != null)
          SerializerFactory.checkDecimal(key, finalValue)
      case SaxonOutputKeys.ITEM_SEPARATOR =>
      // no checking needed
      case OutputKeys.METHOD |
           SaxonOutputKeys.JSON_NODE_OUTPUT_METHOD =>
        if (finalValue != null)
          finalValue = checkMethod(key, finalValue)
      case OutputKeys.MEDIA_TYPE =>
      case SaxonOutputKeys.NORMALIZATION_FORM =>
        if (finalValue != null)
          SerializerFactory.checkNormalizationForm(finalValue)
      case SaxonOutputKeys.PARAMETER_DOCUMENT =>
      // no checking
      case OutputKeys.STANDALONE =>
        if (finalValue != null && !(finalValue == "omit"))
          finalValue = SerializerFactory.checkYesOrNo(key, finalValue)
      case OutputKeys.VERSION =>
      case _ =>
        throw new XPathException("Unknown serialization parameter " + Err.wrap(key), "XQST0109")
    } else if (key.startsWith("{http://saxon.sf.net/}")) {
      // Some Saxon serialization parameters are recognized in HE if they are used for internal purposes
      key match {
        case SaxonOutputKeys.STYLESHEET_VERSION =>
        // return
        case SaxonOutputKeys.PARAMETER_DOCUMENT_BASE_URI =>
        case SaxonOutputKeys.SUPPLY_SOURCE_LOCATOR | SaxonOutputKeys.UNFAILING =>
          if (finalValue != null) finalValue = SerializerFactory.checkYesOrNo(key, finalValue)
        case _ =>
          throw new XPathException("Serialization parameter " + Err.wrap(key, Err.EQNAME) + " is not available in Saxon-HE", "XQST0109")
      }
    } else {
      //return;
    }
    finalValue
  }

  @throws[XPathException]
  private def checkMethod(key: String, value: String): String = {
    var finalValue = value
    if (!("xml" == finalValue) && !("html" == finalValue) && !("xhtml" == finalValue) && !("text" == finalValue)) {
      if (!(SaxonOutputKeys.JSON_NODE_OUTPUT_METHOD == key) && ("json" == finalValue || "adaptive" == finalValue))
        return finalValue
      if (value.startsWith("{"))
        finalValue = "Q" + finalValue
      if (SerializerFactory.isValidEQName(finalValue))
        checkExtensions(finalValue)
      else
        throw new XPathException("Invalid value (" + finalValue + ") for serialization method: " + "must be xml|html|xhtml|text|json|adaptive, or a QName in 'Q{uri}local' form", "SEPM0016")
    }
    finalValue
  }

  @throws[XPathException]
   def checkExtensions(key: String /*@Nullable*/) =
    throw new XPathException("Serialization property " + Err.wrap(key, Err.EQNAME) + " is not available in Saxon-HE")

  @throws[XPathException]
   def getPropertySorter(sortSpecification: String) =
    throw new XPathException("Serialization property saxon:property-order is not available in Saxon-HE")
}