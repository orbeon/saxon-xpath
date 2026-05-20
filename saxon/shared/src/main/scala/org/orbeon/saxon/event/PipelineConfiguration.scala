package org.orbeon.saxon.event

import java.util.{HashMap, Map}

import javax.xml.transform.URIResolver
import org.orbeon.saxon.expr.XPathContext
import org.orbeon.saxon.lib.{ErrorReporter, ParseOptions, SchemaURIResolver}
import org.orbeon.saxon.s9api.HostLanguage
import org.orbeon.saxon.utils.{Configuration, Controller}

class PipelineConfiguration {

  private var config: Configuration = _
  private var uriResolver_ : URIResolver = _

  var schemaURIResolver: SchemaURIResolver = _

  var controller: Controller = _

  var parseOptions: ParseOptions = _

  var hostLanguage: HostLanguage.HostLanguage = HostLanguage.XSLT


  private var components: Map[String, Any] = _

  private var context: XPathContext = _

  def this(configuration: Configuration) = {
    this()
    this.config = configuration
    parseOptions = new ParseOptions
  }

  def this(p: PipelineConfiguration) = {
    this()
    config = p.config
    uriResolver = p.uriResolver
    schemaURIResolver = p.schemaURIResolver
    controller = p.controller
    parseOptions = new ParseOptions(p.parseOptions)
    hostLanguage = p.hostLanguage
    if (p.components != null)
      components = new HashMap(p.components)
    context = p.context
  }

  /*@NotNull*/
  def configuration: Configuration = config

  def getConfiguration: Configuration = configuration

  def configuration_=(config: Configuration): Unit =
    this.config = config

  def errorReporter: ErrorReporter = {
    var reporter = parseOptions.errorReporter
    if (reporter == null)
      reporter = config.makeErrorReporter
    reporter
  }

  def errorReporter_=(errorReporter: ErrorReporter): Unit =
    parseOptions.errorReporter = errorReporter

  def uriResolver: URIResolver = this.uriResolver_

  def uriResolver_=(uriResolver: URIResolver): Unit =
    this.uriResolver_ = uriResolver

  def useXsiSchemaLocation_=(recognize: Boolean): Unit =
    parseOptions.useXsiSchemaLocation = recognize

  def recoverFromValidationErrors_=(recover: Boolean): Unit =
    parseOptions.continueAfterValidationErrors = recover

  def recoverFromValidationErrors: Boolean =
    parseOptions.continueAfterValidationErrors

  def isXSLT: Boolean = hostLanguage == HostLanguage.XSLT

  def getHostLanguage: HostLanguage.HostLanguage = hostLanguage

  def setHostLanguage(hostLanguage: HostLanguage.HostLanguage): Unit =
    this.hostLanguage = hostLanguage

  def expandAttributeDefaults_=(expand: Boolean): Unit =
    parseOptions.expandAttributeDefaults = expand

  def setComponent(name: String, value: AnyRef): Unit = {
    if (components == null)
      components = new HashMap
    components.put(name, value)
  }

  def getComponent(name: String): Any =
    if (components == null) null else components.get(name)

  def xPathContext_=(context: XPathContext): Unit =
    this.context = context

  def xPathContext: XPathContext = context

  def getXPathContext: XPathContext = context

  def setXPathContext(context: XPathContext): Unit = this.context = context

  def xpathContext_=(context: XPathContext): Unit = xPathContext = context

  def xpathContext: XPathContext = xPathContext
}
