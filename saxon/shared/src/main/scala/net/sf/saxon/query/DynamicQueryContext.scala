////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
package net.sf.saxon.query

import net.sf.saxon.utils.Configuration

import net.sf.saxon.utils.Controller

import net.sf.saxon.expr.instruct.GlobalParameterSet

import net.sf.saxon.functions.AccessorFn

import net.sf.saxon.lib._

import net.sf.saxon.om.GroundedValue

import net.sf.saxon.om.Item

import net.sf.saxon.om.NodeInfo

import net.sf.saxon.om.StructuredQName

import net.sf.saxon.trans.XPathException

import net.sf.saxon.value.DateTimeValue

import javax.xml.transform.ErrorListener

import javax.xml.transform.URIResolver

import scala.beans.{BeanProperty, BooleanBeanProperty}


class DynamicQueryContext(private var config: Configuration) {

  var contextItem: Item = _

  private var parameters: GlobalParameterSet = new GlobalParameterSet()

  private var uriResolver: URIResolver = config.getURIResolver

  @BeanProperty
  var errorReporter: ErrorReporter = new StandardErrorReporter()

  /*@Nullable*/

  @BeanProperty
  var traceListener: TraceListener = _

  @BeanProperty
  var unparsedTextURIResolver: UnparsedTextURIResolver = _

  @BeanProperty
  var currentDateAndTime: DateTimeValue = _

  @BeanProperty
  var traceFunctionDestination: Logger = config.getLogger

  private var validationMode: Int = Validation.DEFAULT

  private var applyConversionRules: Boolean = true

  def getSchemaValidationMode(): Int = validationMode

  def setSchemaValidationMode(validationMode: Int): Unit = {
    this.validationMode = validationMode
  }

  def setApplyFunctionConversionRulesToExternalVariables(
                                                          convert: Boolean): Unit = {
    applyConversionRules = convert
  }

  def isApplyFunctionConversionRulesToExternalVariables(): Boolean =
    applyConversionRules

  def setContextItem(item: Item): Unit = {
    if (item == null) {
      throw new NullPointerException("Context item cannot be null")
    }
    if (item.isInstanceOf[NodeInfo]) {
      if (!item.asInstanceOf[NodeInfo].getConfiguration.isCompatible(config)) {
        throw new IllegalArgumentException(
          "Supplied node must be built using the same or a compatible Configuration")
      }
    }
    contextItem = item
  }

  //parameters.put(StandardNames.SAXON_CONTEXT_ITEM, item);
  //parameters.put(StandardNames.SAXON_CONTEXT_ITEM, item);

  def getContextItem: Item = contextItem

  def setParameter(expandedName: StructuredQName, value: GroundedValue): Unit = {
    if (parameters == null) {
      parameters = new GlobalParameterSet()
    }
    parameters.put(expandedName, value)
  }

  def clearParameters(): Unit = {
    parameters = new GlobalParameterSet()
  }

  /*@Nullable*/

  def getParameter(expandedName: StructuredQName): GroundedValue = {
    if (parameters == null) {
      null
    }
    parameters.get(expandedName)
  }

  def getParameters(): GlobalParameterSet =
    if (parameters == null) {
      new GlobalParameterSet()
    } else {
      parameters
    }

  def setURIResolver(resolver: URIResolver): Unit = {
    // System.err.println("Setting uriresolver to " + resolver + " on " + this);
    uriResolver = resolver
  }

  def getURIResolver(): URIResolver = uriResolver

  def setErrorListener(listener: ErrorListener): Unit = {
    errorReporter = new ErrorReporterToListener(listener)
  }

  def getErrorListener(): ErrorListener = {
    val uel: ErrorReporter = getErrorReporter
    if (uel.isInstanceOf[ErrorReporterToListener]) {
      uel.asInstanceOf[ErrorReporterToListener].getErrorListener
    } else {
      null
    }
  }

  def setCurrentDateTime(dateTime: DateTimeValue): Unit = {
    currentDateAndTime = dateTime
    if (dateTime.getComponent(AccessorFn.Component.TIMEZONE) ==
      null) {
      throw new XPathException("Supplied date/time must include a timezone")
    }
  }

  def getConfiguration(): Configuration = config

  def initializeController(controller: Controller): Unit = {
    controller.setURIResolver(getURIResolver)
    controller.setErrorReporter(getErrorReporter)
    controller.addTraceListener(getTraceListener)
    if (unparsedTextURIResolver != null) {
      controller.setUnparsedTextURIResolver(unparsedTextURIResolver)
    }
    controller.setTraceFunctionDestination(getTraceFunctionDestination)
    controller.setSchemaValidationMode(getSchemaValidationMode)
    val currentDateTime: DateTimeValue = getCurrentDateAndTime
    if (currentDateTime != null) {
      controller.setCurrentDateTime(currentDateTime)
    }
    controller.setGlobalContextItem(contextItem)
    controller.initializeController(parameters)
    controller.setApplyFunctionConversionRulesToExternalVariables(
      applyConversionRules)
  }

}
