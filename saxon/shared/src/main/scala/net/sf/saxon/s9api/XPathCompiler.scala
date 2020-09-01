package net.sf.saxon.s9api

import java.net.URI
import java.util.{Iterator, Map, Objects}
import java.util.concurrent.ConcurrentHashMap

import net.sf.saxon.expr.StaticContext
import net.sf.saxon.expr.parser.OptimizerOptions
import net.sf.saxon.lib.{ErrorReporter, StringCollator}
import net.sf.saxon.s9api.OccurrenceIndicator._
import net.sf.saxon.sxpath.{IndependentContext, XPathEvaluator, XPathExpression, XPathVariable}
import net.sf.saxon.trans._
import net.sf.saxon.utils.Configuration
import net.sf.saxon.value.SequenceType

import scala.beans.BeanProperty

class XPathCompiler(@BeanProperty var processor: Processor) {

  private val evaluator: XPathEvaluator = new XPathEvaluator(processor.getUnderlyingConfiguration)

  private val env: IndependentContext =
    this.evaluator.getStaticContext.asInstanceOf[IndependentContext]

  @BeanProperty
  var required_ContextItemType: ItemType = _

  private var cache: Map[String, XPathExecutable] = null

  def setBackwardsCompatible(option: Boolean): Unit = {
    if (cache != null) {
      cache.clear()
    }
    env.setBackwardsCompatibilityMode(option)
  }

  def isBackwardsCompatible: Boolean = env.isInBackwardsCompatibleMode

  def setSchemaAware(schemaAware: Boolean): Unit = {
    if (schemaAware &&
      !processor.getUnderlyingConfiguration.isLicensedFeature(
        Configuration.LicenseFeature.SCHEMA_VALIDATION)) {
      throw new UnsupportedOperationException(
        "Schema processing requires a licensed Saxon-EE configuration")
    }
    env.setSchemaAware(schemaAware)
  }

  def isSchemaAware: Boolean = env.getPackageData.isSchemaAware

  def setLanguageVersion(value: String): Unit = {
    if (cache != null) {
      cache.clear()
    }
    var version: Int = 0
    if ("1.0" == value) {
      version = 20
      env.setBackwardsCompatibilityMode(true)
    } else if ("2.0" == value) {
      version = 20
    } else if ("3.0" == value || "3.05" == value) {
      version = 30
    } else if ("3.1" == value) {
      version = 31
    } else {
      throw new IllegalArgumentException("XPath version")
    }
    env.setXPathLanguageLevel(version)
    env.setDefaultFunctionLibrary(version)
  }

  def getLanguageVersion: String =
    if (env.getXPathVersion == 20) {
      "2.0"
    } else if (env.getXPathVersion == 30) {
      "3.0"
    } else if (env.getXPathVersion == 31) {
      "3.1"
    } else {
      throw new IllegalStateException(
        "Unknown XPath version " + env.getXPathVersion)
    }

  def setBaseURI(uri: URI): Unit = {
    if (cache != null) {
      cache.clear()
    }
    if (uri == null) {
      env.setBaseURI(null)
    } else {
      if (!uri.isAbsolute) {
        throw new IllegalArgumentException(
          "Supplied base URI must be absolute")
      }
      env.setBaseURI(uri.toString)
    }
  }

  def getBaseURI: URI = new URI(env.getStaticBaseURI)

  def setWarningHandler(reporter: ErrorReporter): Unit = {
    env.setWarningHandler(
      (message, location) =>
        reporter.report(
          new XmlProcessingIncident(message, SaxonErrorCode.SXWN9000, location)
            .asWarning()))
  }

  def declareNamespace(prefix: String, uri: String): Unit = {
    if (cache != null) {
      cache.clear()
    }
    env.declareNamespace(prefix, uri)
  }

  def importSchemaNamespace(uri: String): Unit = {
    if (cache != null) {
      cache.clear()
    }
    env.getImportedSchemaNamespaces()(uri)
    env.setSchemaAware(true)
  }

  def setAllowUndeclaredVariables(allow: Boolean): Unit = {
    if (cache != null) {
      cache.clear()
    }
    env.setAllowUndeclaredVariables(allow)
  }

  def isAllowUndeclaredVariables: Boolean = env.isAllowUndeclaredVariables

  def declareVariable(qname: QName): Unit = {
    if (cache != null) {
      cache.clear()
    }
    env.declareVariable(qname.getNamespaceURI, qname.getLocalName)
  }

  def declareVariable(qname: QName,
                      itemType: ItemType,
                      occurrences: OccurrenceIndicator): Unit = {
    if (cache != null) {
      cache.clear()
    }
    val `var`: XPathVariable =
      env.declareVariable(qname.getNamespaceURI, qname.getLocalName)
    `var`.setRequiredType(
      SequenceType.makeSequenceType(itemType.getUnderlyingItemType,
        occurrences.getCardinality))
  }

  /* Commenting this method as XsltPackage Scala class does not exist
  def addXsltFunctionLibrary(libraryPackage: XsltPackage): Unit = { // XsltPackage not exist
    env.getFunctionLibrary
      .asInstanceOf[FunctionLibraryList]
      .addFunctionLibrary(
        libraryPackage.getUnderlyingPreparedPackage.getPublicFunctions)
  }
*/
  def setRequiredContextItemType(`type`: ItemType): Unit = {
    required_ContextItemType = `type`
    env.setRequiredContextItemType(`type`.getUnderlyingItemType)
  }

  def declareDefaultCollation(uri: String): Unit = {
    var c: StringCollator = null
    try c = getProcessor.getUnderlyingConfiguration.getCollation(uri)
    catch {
      case e: XPathException => c = null

    }
    if (c == null) {
      throw new IllegalStateException("Unknown collation " + uri)
    }
    env.setDefaultCollationName(uri)
  }

  def setCaching(caching: Boolean): Unit = {
    if (caching) {
      if (cache == null) {
        cache = new ConcurrentHashMap()
      }
    } else {
      cache = null
    }
  }

  def isCaching: Boolean = cache != null

  def setFastCompilation(fast: Boolean): Unit = {
    if (fast) {
      env.setOptimizerOptions(new OptimizerOptions(0))
    } else {
      env.setOptimizerOptions(
        getProcessor.getUnderlyingConfiguration.getOptimizerOptions)
    }
  }

  def isFastCompilation: Boolean = env.getOptimizerOptions.getOptions == 0

  def compile(source: String): XPathExecutable = {
    Objects.requireNonNull(source)
    if (cache != null) {
      this.synchronized {
        var expr = cache.get(source)
        if (expr == null) {
          expr = internalCompile(source)
          cache.put(source, expr)
        }
        expr
      }
    } else {
      internalCompile(source)
    }
  }

  private def internalCompile(source: String): XPathExecutable = {
    env.getDecimalFormatManager.checkConsistency()
    var eval = evaluator
    var ic = env
    if (ic.isAllowUndeclaredVariables) {
      eval = new XPathEvaluator(processor.getUnderlyingConfiguration)
      ic = new IndependentContext(env)
      eval.setStaticContext(ic)
      val iter = env.iterateExternalVariables()
      while (iter.hasNext) {
        val `var` = iter.next()
        val var2 = ic.declareVariable(`var`.getVariableQName)
        var2.setRequiredType(`var`.getRequiredType)
      }
    }
    val cexp = eval.createExpression(source)
    new XPathExecutable(cexp, processor, ic)
  }

  def evaluate(expression: String, contextItem: XdmItem): XdmValue = {
    Objects.requireNonNull(expression)
    val selector: XPathSelector = compile(expression).load()
    if (contextItem != null) {
      selector.setContextItem(contextItem)
    }
    selector.evaluate()
  }

  def evaluateSingle(expression: String, contextItem: XdmItem): XdmItem = {
    Objects.requireNonNull(expression)
    val selector: XPathSelector = compile(expression).load()
    if (contextItem != null) {
      selector.setContextItem(contextItem)
    }
    selector.evaluateSingle()
  }

  def compilePattern(source: String): XPathExecutable = {
    Objects.requireNonNull(source)
    env.getDecimalFormatManager.checkConsistency()
    val cexp: XPathExpression = evaluator.createPattern(source)
    new XPathExecutable(cexp, processor, env)
  }

  def setDecimalFormatProperty(format: QName,
                               property: String,
                               value: String): Unit = {
    var dfm: DecimalFormatManager = env.getDecimalFormatManager
    if (dfm == null) {
      dfm = new DecimalFormatManager(HostLanguage.XPATH, env.getXPathVersion)
      env.setDecimalFormatManager(dfm)
    }
    val symbols: DecimalSymbols =
      dfm.obtainNamedDecimalFormat(format.getStructuredQName)
    property match {
      case "decimal-separator" => symbols.setDecimalSeparator(value)
      case "grouping-separator" => symbols.setGroupingSeparator(value)
      case "exponent-separator" => symbols.setExponentSeparator(value)
      case "infinity" => symbols.setInfinity(value)
      case "NaN" => symbols.setNaN(value)
      case "minus-sign" => symbols.setMinusSign(value)
      case "percent" => symbols.setPercent(value)
      case "per-mille" => symbols.setPerMille(value)
      case "zero-digit" => symbols.setZeroDigit(value)
      case "digit" => symbols.setDigit(value)
      case "pattern-separator" => symbols.setPatternSeparator(value)
      case _ =>
        throw new IllegalArgumentException(
          "Unknown decimal format attribute " + property)

    }
  }

  def getUnderlyingStaticContext: StaticContext = env

}
