package org.orbeon.saxon.expr.instruct

import org.orbeon.saxon.expr.{PackageData, XPathContext}
import org.orbeon.saxon.expr.parser.RoleDiagnostic
import org.orbeon.saxon.functions.FunctionLibraryList
import org.orbeon.saxon.om.{Item, StructuredQName}
import org.orbeon.saxon.query.QueryModule
import org.orbeon.saxon.s9api.HostLanguage
import org.orbeon.saxon.s9api.HostLanguage.HostLanguage
import org.orbeon.saxon.serialize.{CharacterMapIndex, SerializationProperties}
import org.orbeon.saxon.trace.ExpressionPresenter
import org.orbeon.saxon.trans.XPathException
import org.orbeon.saxon.utils.Configuration

import java.util._
import scala.beans.BeanProperty
import scala.jdk.CollectionConverters._


class Executable(var config: Configuration) {

  @BeanProperty
  var topLevelPackage: PackageData = _

  @BeanProperty
  var packages: List[PackageData] = new ArrayList()

  private var defaultOutputProperties: Properties = _

  private var characterMapIndex: CharacterMapIndex = _

  private var queryLibraryModules: HashMap[String, List[QueryModule]] = _

  private var queryLocationHintsProcessed: HashSet[String] = _

  @BeanProperty
  var functionLibrary: FunctionLibraryList = _

  @BeanProperty
  var hostLanguage: HostLanguage = HostLanguage.XSLT

  private val globalParams: Map[StructuredQName, GlobalParam] = new HashMap

  private var outputDeclarations: HashMap[StructuredQName, Properties] = null

  var createsSecondaryResult: Boolean = false

   var schemaAware: Boolean = false

  @BeanProperty
  var globalContextRequirement: GlobalContextRequirement = null


  def setConfiguration(config: Configuration): Unit = {
    this.config = config
  }

  def getConfiguration: Configuration = config

  def addPackage(data: PackageData): Unit = {
    packages.add(data)
  }

  def setCharacterMapIndex(cmi: CharacterMapIndex): Unit = {
    characterMapIndex = cmi
  }

  def setDefaultOutputProperties(properties: Properties): Unit = {
    defaultOutputProperties = properties
  }

  def getPrimarySerializationProperties: SerializationProperties = {
    if (defaultOutputProperties == null) {
      defaultOutputProperties = new Properties()
    }
    val props: Properties = defaultOutputProperties
    new SerializationProperties(props, getCharacterMapIndex)
  }

  def setOutputProperties(qName: StructuredQName,
                          properties: Properties): Unit = {
    if (outputDeclarations == null) {
      outputDeclarations = new HashMap(5)
    }
    outputDeclarations.put(qName, properties)
  }

  def getOutputProperties: Properties =
    new Properties(defaultOutputProperties)

  def getOutputProperties(qName: StructuredQName): Properties =
    if (outputDeclarations == null) {
      null
    } else {
      outputDeclarations.get(qName)
    }

  def addQueryLibraryModule(module: QueryModule): Unit = {
    if (queryLibraryModules == null) {
      queryLibraryModules = new HashMap(5)
    }
    val uri: String = module.getModuleNamespace
    var existing: List[QueryModule] = queryLibraryModules.get(uri)
    if (existing == null) {
      existing = new ArrayList(5)
      existing.add(module)
      queryLibraryModules.put(uri, existing)
    } else if (!existing.contains(module)) {
      existing.add(module)
    }
  }

  def getQueryLibraryModules(namespace: String): List[QueryModule] = {
    if (queryLibraryModules == null) {
      return null
    }
    queryLibraryModules.get(namespace)
  }

  def getQueryModuleWithSystemId(systemId: String,
                                 topModule: QueryModule): QueryModule = {
    if (systemId == topModule.getSystemId) {
      return topModule
    }
    val miter: Iterator[QueryModule] = getQueryLibraryModules
    while (miter.hasNext) {
      val sqc: QueryModule = miter.next()
      val uri: String = sqc.getSystemId
      if (uri != null && uri == systemId) {
        return sqc
      }
    }
    null
  }

  def getQueryLibraryModules: Iterator[QueryModule] =
    if (queryLibraryModules == null) {
      Collections.emptyIterator[QueryModule]()
    } else {
      val modules: List[QueryModule] = new ArrayList[QueryModule]()
      for (queryModules <- queryLibraryModules.values.asScala) {
        modules.addAll(queryModules)
      }
      modules.iterator
    }

  def addQueryLocationHintProcessed(uri: String): Unit = {
    if (queryLocationHintsProcessed == null) {
      queryLocationHintsProcessed = new HashSet()
    }
    queryLocationHintsProcessed.add(uri)
  }

  def isQueryLocationHintProcessed(uri: String): Boolean =
    queryLocationHintsProcessed != null && queryLocationHintsProcessed
      .contains(uri)

  def fixupQueryModules(main: QueryModule): Unit = {
    main.bindUnboundVariables()
    if (queryLibraryModules != null) {
      for (queryModules <- queryLibraryModules.values.asScala; env <- queryModules.asScala) {
        env.bindUnboundVariables()
      }
    }
    val varDefinitions: List[GlobalVariable] =
      main.fixupGlobalVariables(main.getGlobalStackFrameMap)
    main.bindUnboundFunctionCalls()
    if (queryLibraryModules != null) {
      for (queryModules <- queryLibraryModules.values.asScala; env <- queryModules.asScala) {
        env.bindUnboundFunctionCalls()
      }
    }
    main.checkForCircularities(varDefinitions, main.getGlobalFunctionLibrary)
    main.fixupGlobalFunctions()
    main.typeCheckGlobalVariables(varDefinitions)
    main.optimizeGlobalFunctions()
  }

  def explainGlobalVariables(presenter: ExpressionPresenter): Unit = {
    presenter.startElement("globalVariables")
    for (pack <- getPackages.asScala; variables <- pack.getGlobalVariableList.asScala) {
      variables.export(presenter)
    }
    presenter.endElement()
  }

  def registerGlobalParameter(param: GlobalParam): Unit = {
    globalParams.put(param.getVariableQName, param)
  }

  def getGlobalParameters: Map[StructuredQName, GlobalParam] = globalParams

  def getGlobalParameter(name: StructuredQName): GlobalParam =
    globalParams.get(name)

  def checkSuppliedParameters(params: GlobalParameterSet): Unit = ()

  def setCreatesSecondaryResult(flag: Boolean): Unit = {
    createsSecondaryResult = flag
  }

  def checkInitialContextItem(contextItem: Item, context: XPathContext): Item = {
    var contextItemVar:Item = contextItem;
    if (globalContextRequirement == null) {
      return contextItemVar
    }
    if (contextItemVar != null && globalContextRequirement.isAbsentFocus) {
      throw new XPathException(
        "The global context item is required to be absent",
        "XPDY0002")
    }
    val th = config.getTypeHierarchy
    if (contextItemVar == null) {
      if (!globalContextRequirement.isMayBeOmitted) {
        throw new XPathException(
          "A global context item is required, but none has been supplied",
          "XTDE3086")
      }
      if (globalContextRequirement.getDefaultValue != null) {
        try{
          contextItemVar =globalContextRequirement.getDefaultValue.evaluateItem(context)
        }catch {
          case e: XPathException => {
            if ("XPDY0002" == e.getErrorCodeLocalPart) {
              if (e.getMessage.contains("last()") || e.getMessage.contains(
                "position")) {} else {
                e.setErrorCode("XQDY0054")
              }
            }
            throw e
          }

        }
        if (contextItemVar == null) {
          throw new XPathException(
            "The context item cannot be initialized to an empty sequence",
            "XPTY0004")
        }
        for (itemType <- globalContextRequirement.getRequiredItemTypes.asScala
             if !itemType.matches(contextItemVar, th)) {
          val role: RoleDiagnostic = new RoleDiagnostic(
            RoleDiagnostic.MISC,
            "defaulted global context item",
            0)
          val s: String = role.composeErrorMessage(itemType, contextItemVar, th)
          throw new XPathException(s, "XPTY0004")
        }
      }
    } else {
      for (itemType <- globalContextRequirement.getRequiredItemTypes.asScala
           if !itemType.matches(contextItemVar, config.getTypeHierarchy)) {
        val role: RoleDiagnostic = new RoleDiagnostic(
          RoleDiagnostic.MISC,
          "supplied global context item",
          0)
        val s: String = role.composeErrorMessage(itemType, contextItemVar, th)
        throw new XPathException(
          s,
          if (getHostLanguage == HostLanguage.XSLT) "XTTE0590" else "XPTY0004")
      }
    }
    contextItemVar
  }

  def setSchemaAware(aware: Boolean): Unit =
    schemaAware = aware

  def isSchemaAware: Boolean = schemaAware

  def getCharacterMapIndex = {
    if (characterMapIndex == null)
      characterMapIndex = new CharacterMapIndex
    characterMapIndex
  }
}