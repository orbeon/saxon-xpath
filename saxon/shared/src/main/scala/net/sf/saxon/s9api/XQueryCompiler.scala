package net.sf.saxon.s9api

import net.sf.saxon.expr.parser.OptimizerOptions
import net.sf.saxon.lib.ErrorReporter
import net.sf.saxon.lib.ModuleURIResolver
import net.sf.saxon.pattern.NodeKindTest
import net.sf.saxon.query.StaticQueryContext
import javax.xml.transform.ErrorListener
import java.io._
import java.net.URI
import java.util.List

import net.sf.saxon.utils.Configuration

import scala.beans.{BeanProperty}
import scala.util.{Try, Using}

class XQueryCompiler(@BeanProperty var processor: Processor) {

  private var staticQueryContext: StaticQueryContext =
    processor.getUnderlyingConfiguration.newStaticQueryContext

  @BeanProperty
  var required_ContextItemType: ItemType = _

  @BeanProperty
  var encoding: String = _

  def setBaseURI(baseURI: URI): Unit = {
    if (baseURI == null) {
      staticQueryContext.setBaseURI(null)
    } else {
      if (!baseURI.isAbsolute) {
        throw new IllegalArgumentException(
          "Base URI must be an absolute URI: " + baseURI)
      }
      staticQueryContext.setBaseURI(baseURI.toString)
    }
  }

  def getBaseURI: URI = new URI(staticQueryContext.getBaseURI)

  def setErrorListener(listener: ErrorListener): Unit = {
    staticQueryContext.setErrorListener(listener)
  }

  def getErrorListener: ErrorListener = staticQueryContext.getErrorListener

  def setErrorReporter(reporter: ErrorReporter): Unit = {
    staticQueryContext.setErrorReporter(reporter)
  }

  def getErrorReporter: ErrorReporter = staticQueryContext.getErrorReporter

  def setCompileWithTracing(option: Boolean): Unit = {
    staticQueryContext.setCompileWithTracing(option)
  }

  def isCompileWithTracing: Boolean = staticQueryContext.isCompileWithTracing

  def setModuleURIResolver(resolver: ModuleURIResolver): Unit = {
    staticQueryContext.setModuleURIResolver(resolver)
  }

  def getModuleURIResolver: ModuleURIResolver =
    staticQueryContext.getModuleURIResolver

  def setUpdatingEnabled(updating: Boolean): Unit = {
    if (updating &&
      !staticQueryContext.getConfiguration.isLicensedFeature(
        Configuration.LicenseFeature.ENTERPRISE_XQUERY)) {
      throw new UnsupportedOperationException(
        "XQuery Update is not supported in this Saxon Configuration")
    }
    staticQueryContext.setUpdatingEnabled(updating)
  }

  def isUpdatingEnabled: Boolean = staticQueryContext.isUpdatingEnabled

  def setSchemaAware(schemaAware: Boolean): Unit = {
    if (schemaAware &&
      !processor.getUnderlyingConfiguration.isLicensedFeature(
        Configuration.LicenseFeature.ENTERPRISE_XQUERY)) {
      throw new UnsupportedOperationException(
        "Schema-awareness requires a Saxon-EE license")
    }
    staticQueryContext.setSchemaAware(schemaAware)
  }

  def isSchemaAware: Boolean = staticQueryContext.isSchemaAware

  def setStreaming(option: Boolean): Unit = {
    staticQueryContext.setStreaming(option)
    if (option &&
      !processor.getUnderlyingConfiguration.isLicensedFeature(
        Configuration.LicenseFeature.ENTERPRISE_XQUERY)) {
      throw new UnsupportedOperationException(
        "Streaming requires a Saxon-EE license")
    }
    if (option) {
      this.required_ContextItemType =
        new ConstructedItemType(NodeKindTest.DOCUMENT, getProcessor)
    }
  }

  def isStreaming: Boolean = staticQueryContext.isStreaming

  def getLanguageVersion: String = "3.1"

  def declareNamespace(prefix: String, uri: String): Unit = {
    staticQueryContext.declareNamespace(prefix, uri)
  }

  def declareDefaultCollation(uri: String): Unit = {
    staticQueryContext.declareDefaultCollation(uri)
  }

  def setRequiredContextItemType(`type`: ItemType): Unit = {
    required_ContextItemType = `type`
    staticQueryContext.setRequiredContextItemType(`type`.getUnderlyingItemType)
  }

  def setFastCompilation(fast: Boolean): Unit = {
    if (fast) {
      staticQueryContext.setOptimizerOptions(new OptimizerOptions(0))
    } else {
      staticQueryContext.setOptimizerOptions(
        getProcessor.getUnderlyingConfiguration.getOptimizerOptions)
    }
  }

  def isFastCompilation: Boolean =
    staticQueryContext.getOptimizerOptions.getOptions == 0

  def compileLibrary(query: String): Unit = {
    staticQueryContext.compileLibrary(query)
  }

  def compileLibrary(query: File): Unit = {

    Using(new FileInputStream(query)) { stream =>
      var savedBaseUri: String = staticQueryContext.getBaseURI
      staticQueryContext.setBaseURI(query.toURI().toString)
      staticQueryContext.compileLibrary(stream, encoding)
      staticQueryContext.setBaseURI(savedBaseUri)
    }
  }

  def compileLibrary(query: Reader): Unit = {
    staticQueryContext.compileLibrary(query)
  }

  def compileLibrary(query: InputStream): Unit = {
    staticQueryContext.compileLibrary(query, encoding)
  }

  def compile(query: String): XQueryExecutable =
    new XQueryExecutable(processor, staticQueryContext.compileQuery(query))

  def compile(query: File): XQueryExecutable = {
    Using(new FileInputStream(query)) { stream =>
      val savedBaseUri: String = staticQueryContext.getBaseURI
      staticQueryContext.setBaseURI(query.toURI().toString)
      val exec: XQueryExecutable = new XQueryExecutable(
        processor,
        staticQueryContext.compileQuery(stream, encoding))
      staticQueryContext.setBaseURI(savedBaseUri)
      exec
    }.get
  }

  def compile(query: InputStream): XQueryExecutable =
    new XQueryExecutable(processor,
      staticQueryContext.compileQuery(query, encoding))

  def compile(query: Reader): XQueryExecutable =
    new XQueryExecutable(processor, staticQueryContext.compileQuery(query))

  def getUnderlyingStaticContext: StaticQueryContext = staticQueryContext

  def setErrorList(errorList: List[_ >: StaticError]): Unit = {
    this.setErrorReporter((err: StaticError) => errorList.add(err))
  }

}
