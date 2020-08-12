package net.sf.saxon.trans

import net.sf.saxon.expr.instruct.GlobalParameterSet
import net.sf.saxon.expr.parser.CodeInjector
import net.sf.saxon.expr.parser.OptimizerOptions
import net.sf.saxon.lib._
import net.sf.saxon.om.GroundedValue
import net.sf.saxon.om.StructuredQName
import net.sf.saxon.query.QueryLibrary
import net.sf.saxon.s9api.UnprefixedElementMatchingPolicy
import net.sf.saxon.trans.packages.PackageLibrary
import javax.xml.transform.ErrorListener
import javax.xml.transform.URIResolver
import java.util.Collection

import net.sf.saxon.s9api.UnprefixedElementMatchingPolicy.UnprefixedElementMatchingPolicy
import net.sf.saxon.utils.Configuration

import scala.beans.{BeanProperty, BooleanBeanProperty}

//remove if not needed
//import scala.collection.JavaConversions._

class CompilerInfo(private var config: Configuration) {

  private var uriResolver: URIResolver = _

  @BeanProperty
  var outputURIResolver: OutputURIResolver = StandardOutputResolver.getInstance

  @BeanProperty
  var errorReporter: ErrorReporter = new StandardErrorReporter()

  @BeanProperty
  var codeInjector: CodeInjector = _

  private var recoveryPolicy: Int = Mode.RECOVER_WITH_WARNINGS

  @BooleanBeanProperty
  var schemaAware: Boolean = _

  @BeanProperty
  var messageReceiverClassName: String =
    "net.sf.saxon.serialize.MessageEmitter"

  @BeanProperty
  var defaultInitialMode: StructuredQName = _

  @BeanProperty
  var defaultInitialTemplate: StructuredQName = _

  private var suppliedParameters: GlobalParameterSet = new GlobalParameterSet()

  @BeanProperty
  var defaultCollation: String = _

  @BeanProperty
  var packageLibrary: PackageLibrary = new PackageLibrary(this)

  @BooleanBeanProperty
  var assertionsEnabled: Boolean = false

  @BeanProperty
  var targetEdition: String = "HE"

  @BooleanBeanProperty
  var relocatable: Boolean = false

  @BeanProperty
  var queryLibraries: Collection[QueryLibrary] = _

  @BeanProperty
  var optimizerOptions: OptimizerOptions = config.getOptimizerOptions

  private var defaultNamespaceForElementsAndTypes: String = ""

  @BeanProperty
  var unprefixedElementMatchingPolicy: UnprefixedElementMatchingPolicy =
    UnprefixedElementMatchingPolicy.DEFAULT_NAMESPACE

  def this(info: CompilerInfo, config: Configuration) = {
    this(config)
    copyFrom(info)
  }

  def copyFrom(info: CompilerInfo): Unit = {
    config = info.config
    uriResolver = info.uriResolver
    outputURIResolver = info.outputURIResolver
    errorReporter = info.errorReporter
    codeInjector = info.codeInjector
    recoveryPolicy = info.recoveryPolicy
    schemaAware = info.schemaAware
    messageReceiverClassName = info.messageReceiverClassName
    defaultInitialMode = info.defaultInitialMode
    defaultInitialTemplate = info.defaultInitialTemplate
    suppliedParameters = new GlobalParameterSet(info.suppliedParameters)
    defaultCollation = info.defaultCollation
    assertionsEnabled = info.assertionsEnabled
    targetEdition = info.targetEdition
    packageLibrary = new PackageLibrary(info.packageLibrary)
    relocatable = info.relocatable
    optimizerOptions = info.optimizerOptions
    queryLibraries = info.queryLibraries
    defaultNamespaceForElementsAndTypes =
      info.defaultNamespaceForElementsAndTypes
    unprefixedElementMatchingPolicy = info.unprefixedElementMatchingPolicy
  }

  def getConfiguration(): Configuration = config

  def setJustInTimeCompilation(jit: Boolean): Unit = {
    optimizerOptions =
      if (jit)
        optimizerOptions.union(new OptimizerOptions(OptimizerOptions.JIT))
      else optimizerOptions.except(new OptimizerOptions(OptimizerOptions.JIT))
  }

  def isJustInTimeCompilation(): Boolean =
    optimizerOptions.isSet(OptimizerOptions.JIT)

  def setURIResolver(resolver: URIResolver): Unit = {
    uriResolver = resolver
  }

  def setParameter(name: StructuredQName, seq: GroundedValue): Unit = {
    suppliedParameters.put(name, seq)
  }

  def getParameters(): GlobalParameterSet = suppliedParameters

  def clearParameters(): Unit = {
    suppliedParameters.clear()
  }

  def setGenerateByteCode(option: Boolean): Unit = {
    optimizerOptions =
      if (option)
        optimizerOptions.union(
          new OptimizerOptions(OptimizerOptions.BYTE_CODE))
      else
        optimizerOptions.except(
          new OptimizerOptions(OptimizerOptions.BYTE_CODE))
  }

  def isGenerateByteCode(): Boolean =
    optimizerOptions.isSet(OptimizerOptions.BYTE_CODE)

  def getURIResolver(): URIResolver = uriResolver

  def setErrorListener(listener: ErrorListener): Unit = {
    this.errorReporter = new ErrorReporterToListener(listener)
  }

  def getErrorListener(): ErrorListener =
    if (errorReporter.isInstanceOf[ErrorReporterToListener]) {
      errorReporter.asInstanceOf[ErrorReporterToListener].getErrorListener
    } else {
      null
    }

  def isCompileWithTracing(): Boolean = codeInjector != null

  def setXsltVersion(version: Int): Unit = {}

  def getXsltVersion(): Int = 30

  def getDefaultElementNamespace(): String =
    defaultNamespaceForElementsAndTypes

  def setDefaultElementNamespace(
                                  defaultNamespaceForElementsAndTypes: String): Unit = {
    this.defaultNamespaceForElementsAndTypes =
      defaultNamespaceForElementsAndTypes
  }

  def setXQueryLibraries(libraries: Collection[QueryLibrary]): Unit = {
    this.queryLibraries = libraries
  }

}