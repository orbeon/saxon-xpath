package org.orbeon.saxon.sxpath

import java.util.function.BiConsumer
import java.{util => ju}

import org.orbeon.saxon.expr.parser.{Loc, RetainedStaticContext}
import org.orbeon.saxon.expr.{EarlyEvaluationContext, PackageData, StaticContext, XPathContext}
import org.orbeon.saxon.functions.registry.{ConstructorFunctionLibrary, XPath20FunctionSet}
import org.orbeon.saxon.functions.{FunctionLibrary, FunctionLibraryList}
import org.orbeon.saxon.lib.NamespaceConstant
import org.orbeon.saxon.model.{AnyItemType, ItemType}
import org.orbeon.saxon.om.StructuredQName
import org.orbeon.saxon.s9api.{HostLanguage, Location, UnprefixedElementMatchingPolicy}
import org.orbeon.saxon.trans.{DecimalFormatManager, SaxonErrorCode, XmlProcessingIncident}
import org.orbeon.saxon.utils.Configuration

import scala.beans.BeanProperty


abstract class AbstractStaticContext extends StaticContext {

  private var baseURI: String = null
  private var config: Configuration = _
  @BeanProperty
  var packageData: PackageData = _
  @BeanProperty
  var containingLocation: Location = Loc.NONE
  @BeanProperty
  var defaultCollationName: String = _
  private var libraryList: FunctionLibraryList = new FunctionLibraryList
  @BeanProperty
  var defaultFunctionNamespace: String = NamespaceConstant.FN
  var defaultElementNamespace: String = NamespaceConstant.NULL
  private var backwardsCompatible: Boolean = false
  private var xpathLanguageLevel: Int = 31
  var usingDefaultFunctionLibrary: Boolean = _
  private val typeAliases: ju.Map[StructuredQName, ItemType] = new ju.HashMap
  private var unprefixedElementPolicy: UnprefixedElementMatchingPolicy.UnprefixedElementMatchingPolicy =
    UnprefixedElementMatchingPolicy.DEFAULT_NAMESPACE

  def setDefaultElementNamespace(uri: String): Unit =
    defaultElementNamespace = uri

  def getDefaultElementNamespace: String = defaultElementNamespace

  @BeanProperty
  var warningHandler: BiConsumer[String, Location] = (message, locator) => {
    val incident: XmlProcessingIncident =
      new XmlProcessingIncident(message, SaxonErrorCode.SXWN9000, locator)
        .asWarning()
    config.makeErrorReporter.report(incident)
  }

  def setConfiguration(config: Configuration): Unit = {
    this.config = config
    this.defaultCollationName = config.getDefaultCollationName
  }

  def getConfiguration: Configuration = config

  def setSchemaAware(aware: Boolean): Unit =
    getPackageData.setSchemaAware(aware)

  def makeRetainedStaticContext(): RetainedStaticContext =
    new RetainedStaticContext(this)

  def setDefaultFunctionLibrary(): Unit = {
    val lib = new FunctionLibraryList
    lib.addFunctionLibrary(config.getXPath31FunctionSet)
    lib.addFunctionLibrary(getConfiguration.getBuiltInExtensionLibraryList)
    lib.addFunctionLibrary(new ConstructorFunctionLibrary(getConfiguration))
    lib.addFunctionLibrary(config.getIntegratedFunctionLibrary)
    config.addExtensionBinders(lib)
    this.setFunctionLibrary(lib)
  }

  def addFunctionLibrary(library: FunctionLibrary): Unit =
    libraryList.addFunctionLibrary(library)

  def makeEarlyEvaluationContext(): XPathContext =
    new EarlyEvaluationContext(getConfiguration)

  def setBaseURI(baseURI: String): Unit =
    this.baseURI = baseURI

  def getStaticBaseURI: String = if (baseURI == null) "" else baseURI

  def getFunctionLibrary: FunctionLibrary = libraryList

  def setFunctionLibrary(lib: FunctionLibraryList): Unit = {
    libraryList = lib
    usingDefaultFunctionLibrary = false
  }

  def issueWarning(s: String, locator: Location): Unit =
    getWarningHandler.accept(s, locator)

  def getSystemId: String = ""

  def setXPathLanguageLevel(level: Int): Unit =
    xpathLanguageLevel = level

  def getXPathVersion: Int = xpathLanguageLevel

  def setBackwardsCompatibilityMode(option: Boolean): Unit =
    backwardsCompatible = option

  def isInBackwardsCompatibleMode: Boolean = backwardsCompatible

  def setDecimalFormatManager(manager: DecimalFormatManager): Unit =
    getPackageData.setDecimalFormatManager(manager)

  def getRequiredContextItemType: ItemType = AnyItemType

  def getDecimalFormatManager: DecimalFormatManager = {
    var manager = getPackageData.getDecimalFormatManager
    if (manager == null) {
      manager = new DecimalFormatManager(HostLanguage.XPATH, xpathLanguageLevel)
      getPackageData.setDecimalFormatManager(manager)
    }
    manager
  }

  def setDefaultFunctionLibrary(version: Int): Unit = {
    val lib = new FunctionLibraryList
    version match {
      case 20 | 30 =>
      case 305     => lib.addFunctionLibrary(config.getXPath30FunctionSet)
      case 31      => lib.addFunctionLibrary(config.getXPath31FunctionSet)
      case _       => lib.addFunctionLibrary(XPath20FunctionSet.getInstance)
    }
    lib.addFunctionLibrary(getConfiguration.getBuiltInExtensionLibraryList)
    lib.addFunctionLibrary(new ConstructorFunctionLibrary(getConfiguration))
    lib.addFunctionLibrary(config.getIntegratedFunctionLibrary)
    config.addExtensionBinders(lib)
    setFunctionLibrary(lib)
  }

  def setTypeAlias(name: StructuredQName, `type`: ItemType): Unit =
    typeAliases.put(name, `type`)

  override def resolveTypeAlias(typeName: StructuredQName): ItemType =
    typeAliases.get(typeName)

  def setUnprefixedElementMatchingPolicy(policy: UnprefixedElementMatchingPolicy.UnprefixedElementMatchingPolicy): Unit =
    this.unprefixedElementPolicy = policy

  override def getUnprefixedElementMatchingPolicy: UnprefixedElementMatchingPolicy.UnprefixedElementMatchingPolicy =
    unprefixedElementPolicy
}
