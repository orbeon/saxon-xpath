package org.orbeon.saxon.xpath

import org.orbeon.saxon.utils.Configuration

import org.orbeon.saxon.expr.Expression

import org.orbeon.saxon.expr.PackageData

import org.orbeon.saxon.expr.instruct.SlotManager

import org.orbeon.saxon.lib.NamespaceConstant

import org.orbeon.saxon.model.SchemaException

import org.orbeon.saxon.om.NamespaceResolver

import org.orbeon.saxon.om.StructuredQName

import org.orbeon.saxon.sxpath.AbstractStaticContext

import org.orbeon.saxon.trans.XPathException

import javax.xml.XMLConstants

import javax.xml.namespace.NamespaceContext

import javax.xml.transform.Source

import javax.xml.xpath.XPathFunctionResolver

import javax.xml.xpath.XPathVariableResolver

import java.util.Arrays

import java.util.Iterator

import scala.collection.Set

import JAXPXPathStaticContext._

import scala.beans.{BeanProperty, BooleanBeanProperty}

object JAXPXPathStaticContext {

  private class MinimalNamespaceContext
    extends NamespaceContext
      with NamespaceResolver {

    def getNamespaceURI(prefix: String): String =
      if (prefix == null) {
        throw new IllegalArgumentException("prefix")
      } else if (prefix == XMLConstants.DEFAULT_NS_PREFIX) {
        ""
      } else if (prefix.==("xml")) {
        NamespaceConstant.XML
      } else if (prefix.==("xs")) {
        NamespaceConstant.SCHEMA
      } else if (prefix.==("xsi")) {
        NamespaceConstant.SCHEMA_INSTANCE
      } else if (prefix.==("saxon")) {
        NamespaceConstant.SAXON
      } else {
        null
      }

    def getPrefix(namespaceURI: String): String =
      throw new UnsupportedOperationException()

    def getPrefixes(namespaceURI: String): Iterator[String] =
      throw new UnsupportedOperationException()

    def iteratePrefixes: Iterator[String] = {
      val prefixes: Array[String] = Array("", "xml", "xs", "xsi", "saxon")
      Arrays.asList(prefixes: _*).iterator
    }

    def getURIForPrefix(prefix: String, useDefault: Boolean): String =
      getNamespaceURI(prefix)

  }

}

class JAXPXPathStaticContext(config: Configuration)
  extends AbstractStaticContext
    with NamespaceResolver {

  @BeanProperty
  var stackFrameMap: SlotManager = config.makeSlotManager

  private var xpathFunctionLibrary: XPathFunctionLibrary =
    new XPathFunctionLibrary()

  @BeanProperty
  var namespaceContext: NamespaceContext = new MinimalNamespaceContext()

  private var variableResolver: XPathVariableResolver = _

  this.setConfiguration(config)

  this.setDefaultFunctionLibrary(31)

  addFunctionLibrary(xpathFunctionLibrary)

  this.packageData = new PackageData(getConfiguration)

  def setXPathVariableResolver(resolver: XPathVariableResolver): Unit = {
    this.variableResolver = resolver
  }

  def getXPathVariableResolver: XPathVariableResolver = variableResolver

  def setXPathFunctionResolver(
                                xPathFunctionResolver: XPathFunctionResolver): Unit = {
    if (xpathFunctionLibrary != null) {
      xpathFunctionLibrary.setXPathFunctionResolver(xPathFunctionResolver)
    }
  }

  def getXPathFunctionResolver: XPathFunctionResolver =
    if (xpathFunctionLibrary != null) {
      xpathFunctionLibrary.getXPathFunctionResolver
    } else {
      null
    }

  def getNamespaceResolver(): NamespaceResolver = this

  def getURIForPrefix(prefix: String, useDefault: Boolean): String =
    if (prefix.isEmpty) {
      if (useDefault) {
        getDefaultElementNamespace
      } else {
        NamespaceConstant.NULL
      }
    } else {
      namespaceContext.getNamespaceURI(prefix)
    }

  def iteratePrefixes: Iterator[String] =
    if (namespaceContext.isInstanceOf[NamespaceResolver]) {
      namespaceContext.asInstanceOf[NamespaceResolver].iteratePrefixes
    } else {
      throw new UnsupportedOperationException()
    }

  def bindVariable(qName: StructuredQName): Expression =
    if (variableResolver != null) {
      new JAXPVariableReference(qName, variableResolver)
    } else {
      throw new XPathException(
        "Variable is used in XPath expression, but no JAXP VariableResolver is available")
    }

  def importSchema(source: Source): Unit = {
    getConfiguration.addSchemaSource(source,
      getConfiguration.makeErrorReporter)
    this.setSchemaAware(true)
  }

  def isImportedSchema(namespace: String): Boolean =
    getConfiguration.isSchemaAvailable(namespace)

  override def getImportedSchemaNamespaces(): Set[String] =
    getConfiguration.getImportedNamespaces.asInstanceOf[Set[String]]

}
