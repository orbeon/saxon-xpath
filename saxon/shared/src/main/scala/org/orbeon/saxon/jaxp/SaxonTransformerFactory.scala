////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// Copyright (c) 2018-2020 Saxonica Limited
// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is "Incompatible With Secondary Licenses", as defined by the Mozilla Public License, v. 2.0.
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

package org.orbeon.saxon.jaxp

import javax.xml.transform._
import javax.xml.transform.dom.{DOMResult, DOMSource}
import javax.xml.transform.sax._
import javax.xml.transform.stream.{StreamResult, StreamSource}
import org.orbeon.saxon.event.ReceivingContentHandler
import org.orbeon.saxon.lib.{Feature, FeatureKeys, StandardErrorListener}
import org.orbeon.saxon.utils.Configuration


/**
 * A SaxonTransformerFactory instance can be used to create Transformer and Template
 * objects.
 * <p>Although the class is public, it is not intended to be used directly by applications;
 * applications should normally invoke the subclass `org.orbeon.saxon.TransformerFactoryImpl`.</p>
 * <p>The system property that determines which Factory implementation
 * to create is named "javax.xml.transform.TransformerFactory". This
 * property names a concrete subclass of the TransformerFactory abstract
 * class. If the property is not defined, a platform default is be used.</p>
 * <p>This implementation class implements the abstract methods on both the
 * javax.xml.transform.TransformerFactory and javax.xml.transform.sax.SAXTransformerFactory
 * classes.</p>
 * <p>Since Saxon 9.6, the JAXP transformation interface is re-implemented as a layer
 * on top of the s9api interface. This will affect applications that attempt to
 * down-cast from JAXP interfaces to the underlying implementation classes.</p>
 */
private object SaxonTransformerFactory {
  val FEATURE_SECURE_PROCESSING = javax.xml.XMLConstants.FEATURE_SECURE_PROCESSING
}

// ORBEON: This depends on XSLT stuff, but we onluy want the identity transform support. So we comment
// out what we don't need.
class SaxonTransformerFactory extends ReceivingContentHandler with TransformerHandler {

  // ORBEON
  def setResult(result: Result): Unit = ???
  def setSystemId(systemID: String): Unit = ???
  def getSystemId: String = ???
  def getTransformer: Transformer = ???

  //  private var processor = new Processor(true)
  // ORBEON
  private var config: Configuration = new Configuration
  private var errorListener = new StandardErrorListener

  /**
   * Construct a TransformerFactory using an existing Configuration.
   *
   * @param config the Saxon configuration
   */
  def this(config: Configuration) = {
    this()
    this.config = config
//    processor = new Processor(config)
  }

  /**
   * Set the configuration. This can also be done using the JAXP method
   * setAttribute, with the attribute name {@link org.orbeon.saxon.lib.FeatureKeys# CONFIGURATION}
   *
   * @param config the Saxon configuration
   */
  def setConfiguration(config: Configuration): Unit =
    this.config = config
//    processor.setConfigurationProperty(Feature.CONFIGURATION, config)

  /**
   * Get the configuration. This can also be done using the JAXP method
   * getAttribute, with the attribute name {@link org.orbeon.saxon.lib.FeatureKeys# CONFIGURATION}
   * This is a trapdoor method that provides access to underlying implementation details that
   * may change in subsequent Saxon releases.
   *
   * @return the Saxon configuration
   */
  override def getConfiguration: Configuration = config
//    processor.getUnderlyingConfiguration

//  /**
//   * Process the Source into a Transformer object.
//   * The Transformer object must not be used in multiple threads running concurrently.
//   * Different TransformerFactories can be used concurrently by different
//   * threads.
//   *
//   * @param source An object that holds a URI, input stream, etc. of the stylesheet
//   *               used to perform the transformation.
//   * @return A Transformer object that may be used to perform a transformation
//   *         in a single thread, never null.
//   * @throws javax.xml.transform.TransformerConfigurationException
//   * May throw this during the parse
//   * when it is constructing the Templates object and fails.
//   */
//  def newTransformer(source: Source): Transformer = {
//    val templates = newTemplates(source)
//    templates.newTransformer
//  }

  /**
   * Create a new Transformer object that performs a copy
   * of the source to the result.
   *
   * @return A Transformer object that may be used to perform a transformation
   *         in a single thread, never null.
   */
//  def newTransformer = new IdentityTransformer(processor.getUnderlyingConfiguration)
  def newTransformer = new IdentityTransformer(getConfiguration)

//  /**
//   * Process the Source into a Templates object, which is a
//   * a compiled representation of the source. This Templates object
//   * may then be used concurrently across multiple threads.  Creating
//   * a Templates object allows the TransformerFactory to do detailed
//   * performance optimization of transformation instructions, without
//   * penalizing runtime transformation.
//   *
//   * @param source An object that holds a URL, input stream, etc.
//   * @return A Templates object capable of being used for transformation purposes,
//   *         never null.
//   * @throws javax.xml.transform.TransformerConfigurationException
//   * May throw this during the parse when it
//   * is constructing the Templates object and fails.
//   */
//  def newTemplates(source: Source): Templates = try {
//    val compiler = processor.newXsltCompiler
//    if (errorListener != null)
//      compiler.setErrorReporter(new Nothing(errorListener))
//    val executable = compiler.compile(source)
//    new TemplatesImpl(executable)
//  } catch {
//    case e: SaxonApiExceptionSaxonApiException =>
//      throw new TransformerConfigurationException(e)
//  }

//  /**
//   * Process the Source into a Templates object, which is a
//   * a compiled representation of the source. This Templates object
//   * may then be used concurrently across multiple threads.  Creating
//   * a Templates object allows the TransformerFactory to do detailed
//   * performance optimization of transformation instructions, without
//   * penalizing runtime transformation.
//   *
//   * @param source An object that holds a URL, input stream, etc.
//   * @param info   compile-time options for this stylesheet compilation
//   * @return A Templates object capable of being used for transformation purposes,
//   *         never null.
//   * @throws javax.xml.transform.TransformerConfigurationException
//   * May throw this during the parse when it
//   * is constructing the Templates object and fails.
//   */
//  def newTemplates(source: Source, info: CompilerInfo): Transformer = try {
//    val compiler = processor.newXsltCompiler
//    compiler.getUnderlyingCompilerInfo.copyFrom(info)
//    new TemplatesImpl(compiler.compile(source))
//  } catch {
//    case e: SaxonApiException =>
//      throw new TransformerConfigurationException(e)
//  }

//  /**
//   * Get the stylesheet specification(s) associated
//   * via the xml-stylesheet processing instruction (see
//   * http://www.w3.org/TR/xml-stylesheet/) with the document
//   * document specified in the source parameter, and that match
//   * the given criteria.  Note that it is possible to return several
//   * stylesheets, in which case they are applied as if they were
//   * a list of imports or cascades.
//   *
//   * @param source  The XML source document.
//   * @param media   The media attribute to be matched.  May be null, in which
//   *                case the prefered templates will be used (i.e. alternate = no).
//   *                Note that Saxon does not implement the complex CSS3-based syntax for
//   *                media queries. By default, the media value is simply ignored. An algorithm for
//   *                comparing the requested media with the declared media can be defined using
//   *                the method {@link Configuration# setMediaQueryEvaluator ( Comparator )}.
//   * @param title   The value of the title attribute to match.  May be null.
//   * @param charset The value of the charset attribute to match.  May be null.
//   * @return A Source object suitable for passing to the TransformerFactory.
//   * @throws javax.xml.transform.TransformerConfigurationException
//   * if any problems occur
//   */
//  def getAssociatedStylesheet(source: Source, media: String, title: String, charset: String): Source =
//    try {
//      val compiler = processor.newXsltCompiler
//      if (errorListener != null)
//        compiler.setErrorReporter(new Nothing(errorListener))
//      compiler.getAssociatedStylesheet(source, media, title, charset)
//    } catch {
//      case e: SaxonApiException =>
//        throw new TransformerConfigurationException(e)
//    }

  /**
   * Set an object that is used by default during the transformation
   * to resolve URIs used in xsl:import, or xsl:include.
   *
   * @param resolver An object that implements the URIResolver interface,
   *                 or null.
   */
  def setURIResolver(resolver: URIResolver): Unit =
    getConfiguration.setURIResolver(resolver)

  /**
   * Get the object that is used by default during the transformation
   * to resolve URIs used in document(), xsl:import, or xsl:include.
   *
   * @return The URIResolver that was set with setURIResolver.
   */
  def getURIResolver: URIResolver = getConfiguration.getURIResolver

  /**
   * Look up the value of a feature.
   * <p>The feature name is any absolute URI.</p>
   *
   * @param name The feature name, which is an absolute URI.
   * @return The current state of the feature (true or false).
   */
  def getFeature(name: String): Boolean = name match {
    case SAXSource.FEATURE             |
         SAXResult.FEATURE             |
         DOMSource.FEATURE             |
         DOMResult.FEATURE             |
         StreamSource.FEATURE          |
         StreamResult.FEATURE          |
         SAXTransformerFactory.FEATURE |
         SAXTransformerFactory.FEATURE_XMLFILTER =>
      true
    case SaxonTransformerFactory.FEATURE_SECURE_PROCESSING =>
      ! getConfiguration.getBooleanProperty(Feature.ALLOW_EXTERNAL_FUNCTIONS)
    case _ =>
      try {
        val `val` = getConfiguration.getConfigurationProperty(name)
        `val`.isInstanceOf[Boolean] && `val`.asInstanceOf[Boolean]
      } catch {
        case _: IllegalArgumentException =>
          false
      }
  }

  /**
   * Allows the user to set specific attributes on the underlying
   * implementation.  An attribute in this context is defined to
   * be an option that the implementation provides.
   *
   * <p>Note: setting configuration properties using the method
   * {@link Configuration# setConfigurationProperty ( Feature, Object)}
   * is more efficient, and gives better type safety.</p>
   *
   * @param name  The name of the attribute. This must be one of the constants
   *              defined in class {@link org.orbeon.saxon.lib.FeatureKeys}.
   * @param value The value of the attribute.
   * @throws IllegalArgumentException thrown if Saxon
   *                                  doesn't recognize the attribute.
   * @see org.orbeon.saxon.lib.FeatureKeys
   */
  def setAttribute(name: String, value: Any): Unit = {
    name match {
      case FeatureKeys.CONFIGURATION =>
        setConfiguration(value.asInstanceOf[Configuration])
      // ORBEON
      case _ =>
        throw new IllegalArgumentException
//      case FeatureKeys.CONFIGURATION_FILE =>
//        val reader = new ConfigurationReader
//        try setConfiguration(reader.makeConfiguration(new Nothing(new Nothing(value.asInstanceOf[Nothing]))))
//        catch {
//          case err: Nothing =>
//            throw new Nothing(err)
//        }
//
//      case _ =>
//        processor.getUnderlyingConfiguration.setConfigurationProperty(name, value)
    }
  }

  /**
   * Allows the user to retrieve specific attributes on the underlying
   * implementation.
   *
   * @param name The name of the attribute. This must be one of the constants
   *             defined in class {@link org.orbeon.saxon.lib.FeatureKeys}.
   * @return value The value of the attribute.
   * @throws IllegalArgumentException thrown if the underlying
   *                                  implementation doesn't recognize the attribute.
   */
  /*@Nullable*/
  def getAttribute(name: String): Any = getConfiguration.getConfigurationProperty(name)

  /**
   * Set the error event listener for the TransformerFactory, which
   * is used for the processing of transformation instructions,
   * and not for the transformation itself.
   * <p>This method is defined in JAXP but its use with Saxon is deprecated,
   * because the errorListener will be shared by all stylesheet compilations
   * running under this factory, which may be operating concurrently
   * in different threads.</p>
   *
   * @param listener The new error listener.
   * @throws IllegalArgumentException if listener is null.
   */
  def setErrorListener(listener: Nothing): Unit = {
    if (listener == null)
      throw new IllegalArgumentException
    this.errorListener = listener
  }

  /**
   * Get the error event handler for the TransformerFactory.
   *
   * @return The current error listener, which should never be null.
   */
  def getErrorListener: ErrorListener = errorListener

//  /**
//   * Get a TransformerHandler object that can process SAX
//   * ContentHandler events into a Result, based on the transformation
//   * instructions specified by the argument.
//   *
//   * @param src The Source of the transformation instructions.
//   * @return TransformerHandler ready to transform SAX events.
//   * @throws javax.xml.transform.TransformerConfigurationException
//   * If for some reason the
//   * TransformerHandler can not be created.
//   */
//  def newTransformerHandler(src: Nothing): Nothing = {
//    val tmpl = newTemplates(src)
//    newTransformerHandler(tmpl)
//  }

//  /**
//   * Get a TransformerHandler object that can process SAX
//   * ContentHandler events into a Result, based on the Templates argument.
//   *
//   * @param templates The compiled transformation instructions.
//   * @return TransformerHandler ready to transform SAX events.
//   * @throws javax.xml.transform.TransformerConfigurationException
//   * If for some reason the
//   * TransformerHandler can not be created.
//   */
//  def newTransformerHandler(templates: Nothing): Nothing = {
//    if (!templates.isInstanceOf[Nothing]) throw new Nothing("Templates object was not created by Saxon")
//    val transformer = templates.newTransformer.asInstanceOf[Nothing]
//    transformer.newTransformerHandler
//  }

  /**
   * Get a TransformerHandler object that can process SAX
   * ContentHandler events into a Result. The transformation
   * is defined as an identity (or copy) transformation, for example
   * to copy a series of SAX parse events into a DOM tree.
   *
   * @return A non-null reference to a TransformerHandler, that may
   *         be used as a ContentHandler for SAX parse events.
   */
  def newTransformerHandler: TransformerHandler = {
    val transformer = newTransformer // ORBEON
    new IdentityTransformerHandler(transformer)
  }

  /**
   * Get a TemplatesHandler object that can process SAX
   * ContentHandler events into a Templates object.
   *
   * @return A non-null reference to a TransformerHandler, that may
   *         be used as a ContentHandler for SAX parse events.
   */
//  def newTemplatesHandler = new TemplatesHandlerImpl(processor)

//  /**
//   * Create an XMLFilter that uses the given Source as the
//   * transformation instructions.
//   *
//   * @param src The Source of the transformation instructions.
//   * @return An XMLFilter object, or null if this feature is not supported.
//   * @throws javax.xml.transform.TransformerConfigurationException
//   * If for some reason the
//   * XMLFilter cannot be created.
//   */
//  def newXMLFilter(src: Nothing): Nothing = {
//    val tmpl = newTemplates(src)
//    newXMLFilter(tmpl)
//  }

//  /**
//   * Create an XMLFilter, based on the Templates argument..
//   *
//   * @param templates The compiled transformation instructions.
//   * @return An XMLFilter object.
//   * @throws javax.xml.transform.TransformerConfigurationException
//   * if (for example) the <code>templates</code> object was not created by Saxon.
//   */
//  def newXMLFilter(templates: Nothing): Nothing = {
//    if (!templates.isInstanceOf[Nothing]) throw new Nothing("Supplied Templates object was not created using Saxon")
//    val transformer = templates.newTransformer.asInstanceOf[Nothing]
//    transformer.newXMLFilter
//  }

  /**
   * <p>Set a feature for this <code>TransformerFactory</code> and <code>Transformer</code>s
   * or <code>Template</code>s created by this factory.</p>
   * <p>Feature names are fully qualified {@link java.net.URI}s.
   * Implementations may define their own features.
   * An {@link javax.xml.transform.TransformerConfigurationException} is thrown if this <code>TransformerFactory</code> or the
   * <code>Transformer</code>s or <code>Template</code>s it creates cannot support the feature.
   * It is possible for an <code>TransformerFactory</code> to expose a feature value but be unable to change its state.</p>
   * <p>All implementations are required to support the FEATURE_SECURE_PROCESSING feature.
   * When the feature is:</p>
   * <ul>
   * <li>
   * <code>true</code>: the implementation will limit XML processing to conform to implementation limits
   * and behave in a secure fashion as defined by the implementation.
   * Examples include resolving user defined style sheets and functions.
   * If XML processing is limited for security reasons, it will be reported via a call to the registered
   * {@link javax.xml.transform.ErrorListener# fatalError ( javax.xml.transform.TransformerException exception)}.
   * See {@link #setErrorListener ( javax.xml.transform.ErrorListener listener)}. In the Saxon implementation,
   * this option causes calls on extension functions and extensions instructions to be disabled, and also
   * disables the use of xsl:result-document to write to secondary output destinations.
   * </li>
   * <li>
   * <code>false</code>: the implementation will processing XML according to the XML specifications without
   * regard to possible implementation limits.
   * </li>
   * </ul>
   *
   * @param name  Feature name.
   * @param value Is feature state <code>true</code> or <code>false</code>.
   * @throws javax.xml.transform.TransformerConfigurationException
   *                              if this <code>TransformerFactory</code>
   *                              or the <code>Transformer</code>s or <code>Template</code>s it creates cannot support this feature.
   * @throws NullPointerException If the <code>name</code> parameter is null.
   */
  def setFeature(name: String, value: Boolean): Unit = {
    if (name.equals(SaxonTransformerFactory.FEATURE_SECURE_PROCESSING))
      getConfiguration.setBooleanProperty(Feature.ALLOW_EXTERNAL_FUNCTIONS, !value)
    else
      try
        getConfiguration.setBooleanProperty(name, value)
      catch {
        case _: IllegalArgumentException =>
          throw new TransformerConfigurationException(s"Unsupported TransformerFactory feature: $name")
      }
  }

  /**
   * Get the underlying s9api Processor. (Trapdoor method providing access to underlying
   * implementation details which may change in subsequent releases)
   *
   * @return the processor
   * @since 9.8.0.5
   */
//  def getProcessor: Processor = processor
}

