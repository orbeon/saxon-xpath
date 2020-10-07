////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
package org.orbeon.saxon.event

import org.orbeon.saxon.utils.Configuration

import org.orbeon.saxon.utils.Controller

import org.orbeon.saxon.expr.XPathContext

import org.orbeon.saxon.lib.ErrorReporter

import org.orbeon.saxon.lib.ParseOptions

import org.orbeon.saxon.lib.SchemaURIResolver

import org.orbeon.saxon.s9api.HostLanguage

import javax.xml.transform.URIResolver

import java.util.HashMap

import java.util.Map

import scala.beans.{BeanProperty, BooleanBeanProperty}


class PipelineConfiguration {

  private var config: Configuration = _

  private var uriResolver: URIResolver = _

  @BeanProperty
  var schemaURIResolver: SchemaURIResolver = _

  @BeanProperty
  var controller: Controller = _

  @BeanProperty
  var parseOptions: ParseOptions = _

  @BeanProperty
  var hostLanguage: HostLanguage.HostLanguage = HostLanguage.XSLT

  private var components: Map[String, Any] = _

  private var context: XPathContext = _

  def this(configuration: Configuration) = {
    this
    this.config = configuration
    parseOptions = new ParseOptions()
  }

  def this(p: PipelineConfiguration) = {
    this()
    config = p.config
    uriResolver = p.uriResolver
    schemaURIResolver = p.schemaURIResolver
    controller = p.controller
    parseOptions = new ParseOptions(p.parseOptions)
    hostLanguage = p.hostLanguage
    if (p.components != null) {
      components = new HashMap(p.components)
    }
    context = p.context
  }

  /*@NotNull*/

  def getConfiguration: Configuration = config

  def setConfiguration(config: Configuration): Unit = {
    this.config = config
  }

  def getErrorReporter: ErrorReporter = {
    var reporter: ErrorReporter = parseOptions.getErrorReporter
    if (reporter == null) {
      reporter = config.makeErrorReporter
    }
    reporter
  }

  def setErrorReporter(errorReporter: ErrorReporter): Unit = {
    parseOptions.setErrorReporter(errorReporter)
  }

  def getURIResolver: URIResolver = uriResolver

  def setURIResolver(uriResolver: URIResolver): Unit = {
    this.uriResolver = uriResolver
  }

  def setUseXsiSchemaLocation(recognize: Boolean): Unit = {
    parseOptions.setUseXsiSchemaLocation(recognize)
  }

  def setRecoverFromValidationErrors(recover: Boolean): Unit = {
    parseOptions.setContinueAfterValidationErrors(recover)
  }

  def isRecoverFromValidationErrors: Boolean =
    parseOptions.isContinueAfterValidationErrors

  def isXSLT: Boolean = hostLanguage == HostLanguage.XSLT

  def setExpandAttributeDefaults(expand: Boolean): Unit = {
    parseOptions.setExpandAttributeDefaults(expand)
  }

  def setComponent(name: String, value: AnyRef): Unit = {
    if (components == null) {
      components = new HashMap()
    }
    components.put(name, value)
  }

  def getComponent(name: String) = if (components == null) null else components.get(name)

  def setXPathContext(context: XPathContext): Unit = {
    this.context = context
  }

  def getXPathContext: XPathContext = context

}
