////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// Copyright (c) 2018-2020 Saxonica Limited
// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is "Incompatible With Secondary Licenses", as defined by the Mozilla Public License, v. 2.0.
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

package org.orbeon.saxon.jaxp

import java.util.{Objects, Properties}

import javax.xml.transform._
import org.orbeon.saxon.event.Sender
import org.orbeon.saxon.lib.ErrorReporterToListener
import org.orbeon.saxon.serialize.SerializationProperties
import org.orbeon.saxon.trans.{XPathException, XmlProcessingException}
import org.orbeon.saxon.utils.Configuration
import org.xml.sax.SAXParseException

import scala.jdk.CollectionConverters._

/**
 * Saxon implementation of the JAXP IdentityTransformer.
 * This is used mainly for serializing various kinds of source under
 * the control of serialization parameters.
 */
class IdentityTransformer protected[jaxp] (var configuration: Configuration) extends Transformer {

  private var localOutputProperties: Properties = null
  private var uriResolver          : URIResolver = configuration.getURIResolver
  private var errorListener        : ErrorListener = null

  override def reset(): Unit = {
    localOutputProperties = null
    uriResolver = getConfiguration.getURIResolver
    errorListener = null
  }

  def setURIResolver(resolver: URIResolver): Unit =
    this.uriResolver = resolver

  def getURIResolver: URIResolver = uriResolver

  def setErrorListener(listener: ErrorListener): Unit =
    this.errorListener = Objects.requireNonNull(listener)

  def getErrorListener: ErrorListener = errorListener

  def setOutputProperties(properties: Properties): Unit =
    if (properties == null)
      localOutputProperties = null
    else
      for (key <- properties.stringPropertyNames.asScala)
        setOutputProperty(key, properties.getProperty(key))

  def getOutputProperties: Properties = {
    // Make a copy, so that modifications to the returned properties object have no effect (even on the
    // local output properties)
    val newProps = new Properties
    val sheetProperties = getStylesheetOutputProperties
    var keys = sheetProperties.propertyNames
    while (keys.hasMoreElements) {
      val key = keys.nextElement.asInstanceOf[String]
      newProps.setProperty(key, sheetProperties.getProperty(key))
    }
    if (localOutputProperties != null) {
      keys = localOutputProperties.propertyNames
      while (keys.hasMoreElements) {
        val key = keys.nextElement.asInstanceOf[String]
        newProps.setProperty(key, localOutputProperties.getProperty(key))
      }
    }
    newProps
  }

  /**
   * Get the output properties defined in the stylesheet. For an identity transformer this is an empty set,
   * but the method is overridden in subclasses.
   *
   * @return the serialization properties defined in the stylesheet, if any.
   */
  protected def getStylesheetOutputProperties = new Properties

  /**
   * Get the local output properties held in this Transformer object, that is the properties
   * explicitly requested using setOutputProperty() or setOutputProperties()
   *
   * @return the local output properties
   */
  protected def getLocalOutputProperties: Properties = {
    if (localOutputProperties == null)
      makeLocalOutputProperties()
    localOutputProperties
  }

  /**
   * Make the localOutputProperties object. This is a Properties object containing
   * the properties set by calls on setOutputProperty or setOutputProperties; it does not include
   * properties set in the stylesheet or in the configuration.
   */
  private def makeLocalOutputProperties(): Unit =
    localOutputProperties = new Properties

  def getOutputProperty(name: String): String = {
    try
      getConfiguration.getSerializerFactory.checkOutputProperty(name, null)
    catch {
      case err: XPathException =>
        throw new IllegalArgumentException(err.getMessage)
    }
    var value: String = null
    if (localOutputProperties != null)
      value = localOutputProperties.getProperty(name)
    if (value == null)
      value = getStylesheetOutputProperties.getProperty(name)
    value
  }

  def setOutputProperty(name: String, value: String): Unit = {
    if (localOutputProperties == null)
      makeLocalOutputProperties()
    val _value =
      try
        getConfiguration.getSerializerFactory.checkOutputProperty(name, value)
      catch {
        case err: XPathException =>
          throw new IllegalArgumentException(err.getMessage)
      }
    localOutputProperties.setProperty(name, _value)
  }

  def setParameter(name: String, value: Any): Unit = () // No action. Since parameters have no effect on the transformation, we ignore them
  def getParameter(name: String): AnyRef = null
  def clearParameters(): Unit = () // No action

  def transform(source: Source, result: Result): Unit =
    try {
      val sf = getConfiguration.getSerializerFactory
      val receiver = sf.getReceiver(result, new SerializationProperties(getOutputProperties))
      val options = receiver.getPipelineConfiguration.getParseOptions
      if (errorListener != null)
        options.setErrorReporter(new ErrorReporterToListener(errorListener))
      options.setContinueAfterValidationErrors(true)
      Sender.send(source, receiver, options)
    } catch {
      case err: XPathException =>
        err.getException match {
          case spe: SAXParseException =>
            // This generally means the error was already reported.
            if (spe.getException.isInstanceOf[RuntimeException])
              reportFatalError(err)
          case _ => reportFatalError(err)
        }
        throw err
    }

  def getConfiguration: Configuration = configuration

  protected def reportFatalError(err: XPathException): Unit =
    try
      if (errorListener != null)
        errorListener.error(err)
      else
        getConfiguration.makeErrorReporter.report(new XmlProcessingException(err))
    catch {
      case _: TransformerException =>
    }
}
