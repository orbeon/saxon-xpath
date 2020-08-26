package net.sf.saxon.query

import net.sf.saxon.utils.Configuration
import net.sf.saxon.expr._
import net.sf.saxon.expr.instruct._
import net.sf.saxon.expr.parser._
import net.sf.saxon.functions.FunctionLibrary
import net.sf.saxon.functions.FunctionLibraryList
import net.sf.saxon.functions.ResolveURI
import net.sf.saxon.functions.registry.BuiltInFunctionSet
import net.sf.saxon.functions.registry.ConstructorFunctionLibrary
import net.sf.saxon.lib.NamespaceConstant
import net.sf.saxon.lib.Validation
import net.sf.saxon.model.AnyItemType
import net.sf.saxon.model.ItemType
import net.sf.saxon.model.SchemaType
import net.sf.saxon.om._
import net.sf.saxon.s9api.HostLanguage
import net.sf.saxon.s9api.Location
import net.sf.saxon.s9api.XmlProcessingError
import net.sf.saxon.trace.ExpressionPresenter
import net.sf.saxon.trace.TraceCodeInjector
import net.sf.saxon.trans._
import java.net.URI
import java.net.URISyntaxException
import java.util._
import scala.beans.{BeanProperty, BooleanBeanProperty}
import scala.jdk.CollectionConverters._

object QueryModule {

  def makeQueryModule(baseURI: String,
                      executable: Executable,
                      importer: QueryModule,
                      query: String,
                      namespaceURI: String): QueryModule = {
    val config: Configuration = executable.getConfiguration
    val module: QueryModule = new QueryModule(config, importer)
    module.setLocationURI(new URI(baseURI))
    module.setBaseURI(baseURI)
    module.setExecutable(executable)
    module.setModuleNamespace(namespaceURI)
    executable.addQueryLibraryModule(module)
    val qp: XQueryParser = config
      .newExpressionParser("XQ", importer.isUpdating, 31)
      .asInstanceOf[XQueryParser]
    if (importer.getCodeInjector != null) {
      qp.setCodeInjector(importer.getCodeInjector)
    } else if (config.isCompileWithTracing) {
      qp.setCodeInjector(new TraceCodeInjector())
    }
    val qnp: QNameParser = new QNameParser(module.getLiveNamespaceResolver)
      .withAcceptEQName(importer.getXPathVersion >= 30)
      .withUnescaper(
        new XQueryParser.Unescaper(config.getValidCharacterChecker))
    qp.setQNameParser(qnp)
    qp.parseLibraryModule(query, module)
    val namespace: String = module.getModuleNamespace
    if (namespace == null) {
      val err: XPathException = new XPathException(
        "Imported module must be a library module")
      err.setErrorCode("XQST0059")
      err.setIsStaticError(true)
      throw err
    }
    if (namespace != namespaceURI) {
      val err: XPathException = new XPathException(
        "Imported module's namespace does not match requested namespace")
      err.setErrorCode("XQST0059")
      err.setIsStaticError(true)
      throw err
    }
    module
  }

  private class ActiveNamespace {

    var prefix: String = _

    var uri: String = _

  }

}

class QueryModule extends StaticContext {

  var isMainModule: Boolean = true

  var sqc: StaticQueryContext = _
  private var config: Configuration = sqc.getConfiguration

  @BeanProperty
  var userQueryContext: StaticQueryContext = _

  private var topModule: QueryModule = this

  @BeanProperty
  var location_URI: URI = if (baseURI == null) null else new URI(baseURI)

  private var baseURI: String = sqc.getBaseURI

  @BeanProperty
  var moduleNamespace: String = _

  private var explicitPrologNamespaces: HashMap[String, String] = _

  private var activeNamespaces: Stack[NamespaceBinding] = new Stack()

  private var variables: HashMap[StructuredQName, GlobalVariable] = _

  private var libraryVariables: HashMap[StructuredQName, GlobalVariable] = _

  private var undeclaredVariables: HashMap[StructuredQName, UndeclaredVariable] =
    _

  private var importedSchemata: HashSet[String] = _

  private var loadedSchemata: HashMap[String, HashSet[String]] = _

  @BeanProperty
  var executable: Executable = sqc.makeExecutable()

  private var importers: List[QueryModule] = null

  private var functionLibraryList: FunctionLibraryList = _

  @BeanProperty
  var globalFunctionLibrary: XQueryFunctionLibrary = _

  private var localFunctionLibraryNr: Int = _

  private var importedFunctionLibraryNr: Int = _

  private var unboundFunctionLibraryNr: Int = _

  private var importedModuleNamespaces: Set[String] = _

  @BooleanBeanProperty
  var inheritNamespaces: Boolean = true

  @BooleanBeanProperty
  var preserveNamespaces: Boolean = true

  @BeanProperty
  var constructionMode: Int = Validation.PRESERVE

  @BeanProperty
  var defaultFunctionNamespace: String = _

  private var defaultElementNamespace: String = _

  private var preserveSpace: Boolean = false

  private var defaultEmptyLeast: Boolean = true

  private var defaultCollationName: String = sqc.getDefaultCollationName

  var revalidationMode: Int = Validation.SKIP

  var isUpdating: Boolean = false

  @BeanProperty
  var requiredContextItemType: ItemType = AnyItemType.getInstance

  @BeanProperty
  lazy val decimalFormatManager: DecimalFormatManager =
    new DecimalFormatManager(HostLanguage.XQUERY, getXPathVersion)

  @BeanProperty
  var codeInjector: CodeInjector = _

  @BeanProperty
  var packageData: PackageData = _

  private var moduleStaticContext: RetainedStaticContext = null

  private var moduleLocation: Location =
    if (sqc.getModuleLocation == null) new Loc(sqc.getSystemId, 1, -1)
    else sqc.getModuleLocation

  @BeanProperty
  var optimizerOpts: OptimizerOptions = sqc.getOptimizerOptions

  init(sqc)

  val pd: PackageData = new PackageData(config)

  pd.setHostLanguage(HostLanguage.XQUERY)

  pd.setSchemaAware(isSchemaAware)

  var vars: Iterator[GlobalVariable] = sqc.iterateDeclaredGlobalVariables()
  while (vars.hasNext) {
    val `var`: GlobalVariable = vars.next()
    declareVariable(`var`)
    pd.addGlobalVariable(`var`)
    `var`.setPackageData(pd)
  }

  executable.setTopLevelPackage(pd)

  executable.addPackage(pd)

  def this(sqc: StaticQueryContext) {
    this
    this.sqc = sqc
  }

  def this(config: Configuration, importer: QueryModule) = {
    this()
    this.config = config
    importers = null
    if (importer == null) {
      topModule = this
    } else {
      topModule = importer.topModule
      userQueryContext = importer.userQueryContext
      importers = new ArrayList(2)
      importers.add(importer)
    }
    init(userQueryContext)
    packageData = importer.getPackageData
    activeNamespaces = new Stack()
    executable = null
    optimizerOpts = importer.optimizerOpts
  }

  private def init(sqc: StaticQueryContext): Unit = {
    userQueryContext = sqc
    variables = new HashMap(10)
    undeclaredVariables = new HashMap(5)
    if (isTopLevelModule) {
      libraryVariables = new HashMap(10)
    }
    importedSchemata = new HashSet(5)
    importedModuleNamespaces = new HashSet(5)
    moduleNamespace = null
    activeNamespaces = new Stack()
    explicitPrologNamespaces = new HashMap(10)
    if (sqc != null) {
      inheritNamespaces = sqc.isInheritNamespaces
      preserveNamespaces = sqc.isPreserveNamespaces
      preserveSpace = sqc.isPreserveBoundarySpace
      defaultEmptyLeast = sqc.isEmptyLeast
      defaultFunctionNamespace = sqc.getDefaultFunctionNamespace
      defaultElementNamespace = sqc.getDefault_ElementNamespace
      defaultCollationName = sqc.getDefaultCollationName
      constructionMode = sqc.getConstructionMode
      if (constructionMode == Validation.PRESERVE && !sqc.isSchemaAware) {
        constructionMode = Validation.STRIP
      }
      requiredContextItemType = sqc.getRequiredContextItemType
      isUpdating = sqc.isUpdatingEnabled
      codeInjector = sqc.getCodeInjector
      optimizerOpts = sqc.getOptimizerOptions
    }
    initializeFunctionLibraries(sqc)
  }

  private def initializeFunctionLibraries(sqc: StaticQueryContext): Unit = {
    val config: Configuration = getConfiguration
    if (isTopLevelModule) {
      globalFunctionLibrary = new XQueryFunctionLibrary(config)
    }
    functionLibraryList = new FunctionLibraryList()
    functionLibraryList.addFunctionLibrary(getBuiltInFunctionSet)
    functionLibraryList.addFunctionLibrary(
      config.getBuiltInExtensionLibraryList)
    functionLibraryList.addFunctionLibrary(
      new ConstructorFunctionLibrary(config))
    localFunctionLibraryNr =
      functionLibraryList.addFunctionLibrary(new XQueryFunctionLibrary(config))
    importedFunctionLibraryNr = functionLibraryList.addFunctionLibrary(
      new ImportedFunctionLibrary(this,
        getTopLevelModule.getGlobalFunctionLibrary))
    if (sqc != null && sqc.getExtensionFunctionLibrary != null) {
      functionLibraryList.addFunctionLibrary(sqc.getExtensionFunctionLibrary)
    }
    functionLibraryList.addFunctionLibrary(config.getIntegratedFunctionLibrary)
    config.addExtensionBinders(functionLibraryList)
    unboundFunctionLibraryNr =
      functionLibraryList.addFunctionLibrary(new UnboundFunctionLibrary())
  }

  def getBuiltInFunctionSet(): BuiltInFunctionSet =
    if (isUpdating) {
      config.getXQueryUpdateFunctionSet
    } else {
      config.getXPath31FunctionSet
    }

  def getConfiguration(): Configuration = config

  def isTopLevelModule(): Boolean = this == topModule

  def setIsMainModule(main: Boolean): Unit = {
    isMainModule = main
  }

  def mayImportModule(namespace: String): Boolean = {
    if (namespace == moduleNamespace) {
      return false
    }
    if (importers == null) {
      return true
    }
    for (importer <- importers.asScala if !importer.mayImportModule(namespace)) {
      false
    }
    true
  }

  def isSchemaAware(): Boolean = executable.isSchemaAware

  def makeRetainedStaticContext(): RetainedStaticContext =
    if (activeNamespaces.empty()) {
      if (moduleStaticContext == null) {
        moduleStaticContext = new RetainedStaticContext(this)
      }
      moduleStaticContext
    } else {
      new RetainedStaticContext(this)
    }

  def setPreserveBoundarySpace(preserve: Boolean): Unit = {
    preserveSpace = preserve
  }

  def isPreserveBoundarySpace(): Boolean = preserveSpace

  def setEmptyLeast(least: Boolean): Unit = {
    defaultEmptyLeast = least
  }

  def isEmptyLeast(): Boolean = defaultEmptyLeast

  def getImportedFunctionLibrary(): ImportedFunctionLibrary =
    functionLibraryList
      .get(importedFunctionLibraryNr)
      .asInstanceOf[ImportedFunctionLibrary]

  def addImportedNamespace(uri: String): Unit = {
    if (importedModuleNamespaces == null) {
      importedModuleNamespaces = new HashSet(5)
    }
    importedModuleNamespaces.add(uri)
    getImportedFunctionLibrary.addImportedNamespace(uri)
  }

  def importsNamespace(uri: String): Boolean =
    importedModuleNamespaces != null && importedModuleNamespaces.contains(uri)

  def getTopLevelModule(): QueryModule = topModule

  def getContainingLocation(): Location = moduleLocation

  def setLocationURI(uri: URI): Unit = {
    location_URI = uri
    moduleLocation = new Loc(location_URI.toString, 1, -1)
  }

  def getSystemId(): String =
    if (location_URI == null) null else location_URI.toString

  def setBaseURI(uri: String): Unit = {
    baseURI = uri
  }

  def getStaticBaseURI(): String = baseURI

  def getGlobalStackFrameMap(): SlotManager =
    getPackageData.getGlobalSlotManager

  def declareVariable(`var`: GlobalVariable): Unit = {
    val key: StructuredQName = `var`.getVariableQName
    if (variables.get(key) != null) {
      val old: GlobalVariable = variables.get(key)
      if (old == `var` ||
        old.getUltimateOriginalVariable == `var`.getUltimateOriginalVariable) {} else {
        var oldloc: String = " (see line " + old.getLineNumber
        val oldSysId: String = old.getSystemId
        if (oldSysId != null && oldSysId != `var`.getSystemId) {
          oldloc += " in module " + old.getSystemId
        }
        oldloc += ")"
        val err: XPathException = new XPathException(
          "Duplicate definition of global variable " + `var`.getVariableQName.getDisplayName +
            oldloc)
        err.setErrorCode("XQST0049")
        err.setIsStaticError(true)
        err.setLocation(`var`)
        throw err
      }
    }
    variables.put(key, `var`)
    getPackageData.addGlobalVariable(`var`)
    val libVars: HashMap[StructuredQName, GlobalVariable] =
      getTopLevelModule.libraryVariables
    val old: GlobalVariable = libVars.get(key)
    if (old == null || old == `var`) {} else {
      val err: XPathException = new XPathException(
        "Duplicate definition of global variable " + `var`.getVariableQName.getDisplayName +
          " (see line " +
          old.getLineNumber +
          " in module " +
          old.getSystemId +
          ')')
      err.setErrorCode("XQST0049")
      err.setIsStaticError(true)
      err.setLocation(`var`)
      throw err
    }
    if (!isMainModule) {
      libVars.put(key, `var`)
    }
  }

  def getGlobalVariables(): java.lang.Iterable[GlobalVariable] =
    libraryVariables.values

  def fixupGlobalVariables(
                            globalVariableMap: SlotManager): List[GlobalVariable] = {
    val varDefinitions: List[GlobalVariable] =
      new ArrayList[GlobalVariable](20)
    val iters: List[Iterator[GlobalVariable]] =
      new ArrayList[Iterator[GlobalVariable]]()
    iters.add(variables.values.iterator())
    iters.add(libraryVariables.values.iterator())
    for (iter <- iters.asScala) {
      while (iter.hasNext) {
        val `var`: GlobalVariable = iter.next()
        if (!varDefinitions.contains(`var`)) {
          val slot: Int =
            globalVariableMap.allocateSlotNumber(`var`.getVariableQName)
          `var`.compile(getExecutable, slot)
          varDefinitions.add(`var`)
        }
      }
    }
    varDefinitions
  }

  def lookForModuleCycles(referees: Stack[QueryModule],
                          lineNumber: Int): Unit = {
    if (referees.contains(this)) {
      val s: Int = referees.indexOf(this)
      referees.push(this)
      val message: StringBuilder = new StringBuilder(
        "Circular dependency between modules. ")
      for (i <- s until referees.size - 1) {
        val next: QueryModule = referees.get(i + 1)
        if (i == s) {
          message
            .append("Module ")
            .append(getSystemId)
            .append(" references module ")
            .append(next.getSystemId)
        } else {
          message.append(", which references module ").append(next.getSystemId)
        }
      }
      message.append('.')
      val err: XPathException = new XPathException(message.toString)
      err.setErrorCode("XQST0093")
      err.setIsStaticError(true)
      val loc: Loc = new Loc(getSystemId, lineNumber, -1)
      err.setLocator(loc)
      throw err
    } else {
      referees.push(this)
      val viter: Iterator[GlobalVariable] = getModuleVariables
      while (viter.hasNext) {
        val gv: GlobalVariable = viter.next()
        val select: Expression = gv.getBody
        if (select != null) {
          val list: List[Binding] = new ArrayList[Binding](10)
          ExpressionTool.gatherReferencedVariables(select, list)
          for (b <- list.asScala if b.isInstanceOf[GlobalVariable]) {
            val uri: String = b.asInstanceOf[GlobalVariable].getSystemId
            val qName: StructuredQName = b.getVariableQName
            val synthetic: Boolean =
              qName.hasURI(NamespaceConstant.SAXON_GENERATED_VARIABLE)
            if (!synthetic && uri != null && uri != getSystemId) {
              val sqc: QueryModule =
                executable.getQueryModuleWithSystemId(uri, topModule)
              if (sqc != null) {
                sqc.lookForModuleCycles(
                  referees,
                  b.asInstanceOf[GlobalVariable].getLineNumber)
              }
            }
          }
          val fList: List[UserFunction] = new ArrayList[UserFunction](5)
          ExpressionTool.gatherCalledFunctions(select, fList)
          for (f <- fList.asScala) {
            val uri: String = f.getSystemId
            if (uri != null && uri != getSystemId) {
              val sqc: QueryModule =
                executable.getQueryModuleWithSystemId(uri, topModule)
              if (sqc != null) {
                sqc.lookForModuleCycles(referees, f.getLineNumber)
              }
            }
          }
        }
      }
      val fiter: Iterator[XQueryFunction] =
        getLocalFunctionLibrary.getFunctionDefinitions
      while (fiter.hasNext) {
        val gf: XQueryFunction = fiter.next()
        val body: Expression = gf.getUserFunction.getBody
        if (body != null) {
          val vList: List[Binding] = new ArrayList[Binding](10)
          ExpressionTool.gatherReferencedVariables(body, vList)
          for (b <- vList.asScala if b.isInstanceOf[GlobalVariable]) {
            val uri: String = b.asInstanceOf[GlobalVariable].getSystemId
            val qName: StructuredQName = b.getVariableQName
            val synthetic: Boolean = qName.hasURI(NamespaceConstant.SAXON) && "gg" == qName.getPrefix
            if (!synthetic && uri != null && uri != getSystemId) {
              val sqc: QueryModule =
                executable.getQueryModuleWithSystemId(uri, topModule)
              if (sqc != null) {
                sqc.lookForModuleCycles(
                  referees,
                  b.asInstanceOf[GlobalVariable].getLineNumber)
              }
            }
          }
          val fList: List[UserFunction] = new ArrayList[UserFunction](10)
          ExpressionTool.gatherCalledFunctions(body, fList)
          for (f <- fList.asScala) {
            val uri: String = f.getSystemId
            if (uri != null && uri != getSystemId) {
              val sqc: QueryModule =
                executable.getQueryModuleWithSystemId(uri, topModule)
              if (sqc != null) {
                sqc.lookForModuleCycles(referees, f.getLineNumber)
              }
            }
          }
        }
      }
      referees.pop()
    }
  }

  def getModuleVariables(): Iterator[GlobalVariable] =
    variables.values.iterator()

  def checkForCircularities(
                             compiledVars: List[GlobalVariable],
                             globalFunctionLibrary: XQueryFunctionLibrary): Unit = {
    val iter: Iterator[GlobalVariable] = compiledVars.iterator()
    var stack: Stack[Any] = null
    while (iter.hasNext) {
      if (stack == null) {
        stack = new Stack()
      }
      val gv: GlobalVariable = iter.next()
      if (gv != null) {
        gv.lookForCycles(stack, globalFunctionLibrary)
      }
    }
  }

  def typeCheckGlobalVariables(compiledVars: List[GlobalVariable]): Unit = {
    val visitor: ExpressionVisitor = ExpressionVisitor.make(this)
    for (compiledVar <- compiledVars.asScala) {
      compiledVar.typeCheck(visitor)
    }
    if (isMainModule) {
      val gcr: GlobalContextRequirement =
        executable.getGlobalContextRequirement
      if (gcr != null && gcr.getDefaultValue != null) {
        val info: ContextItemStaticInfo = getConfiguration
          .makeContextItemStaticInfo(AnyItemType.getInstance, true)
        gcr.setDefaultValue(gcr.getDefaultValue.typeCheck(visitor, info))
      }
    }
  }

  def bindVariable(qName: StructuredQName): Expression = {
    var `var`: GlobalVariable = variables.get(qName)
    if (`var` == null) {
      val uri: String = qName.getURI
      if ((uri.==("") && isMainModule) || uri == moduleNamespace ||
        importsNamespace(uri)) {
        val main: QueryModule = getTopLevelModule
        `var` = main.libraryVariables.get(qName)
        if (`var` == null) {
          var uvar: UndeclaredVariable = undeclaredVariables.get(qName)
          if (uvar != null) {
            val ref: GlobalVariableReference = new GlobalVariableReference(qName)
            uvar.registerReference(ref)
            return ref
          } else {
            uvar = new UndeclaredVariable()
            uvar.setPackageData(main.getPackageData)
            uvar.setVariableQName(qName)
            val ref: GlobalVariableReference = new GlobalVariableReference(qName)
            uvar.registerReference(ref)
            undeclaredVariables.put(qName, uvar)
            return ref
          }
        } else {
          if (`var`.isPrivate) {
            val err: XPathException = new XPathException(
              "Variable $" + qName.getDisplayName + " is private")
            err.setErrorCode("XPST0008")
            err.setIsStaticError(true)
            throw err
          }
        }
      } else {
        val err: XPathException = new XPathException(
          "Variable $" + qName.getDisplayName + " has not been declared")
        err.setErrorCode("XPST0008")
        err.setIsStaticError(true)
        throw err
      }
    } else {
      if (`var`.isPrivate &&
        (`var`.getSystemId == null || `var`.getSystemId != getSystemId)) {
        var message: String = "Variable $" + qName.getDisplayName + " is private"
        if (`var`.getSystemId == null) {
          message += " (no base URI known)"
        }
        val err: XPathException = new XPathException(message, "XPST0008")
        err.setIsStaticError(true)
        throw err
      }
    }
    val vref: GlobalVariableReference = new GlobalVariableReference(qName)
    `var`.registerReference(vref)
    vref
  }

  def getFunctionLibrary(): FunctionLibrary = functionLibraryList

  def getLocalFunctionLibrary(): XQueryFunctionLibrary =
    functionLibraryList
      .get(localFunctionLibraryNr)
      .asInstanceOf[XQueryFunctionLibrary]

  def declareFunction(function: XQueryFunction): Unit = {
    val config: Configuration = getConfiguration
    if (function.getNumberOfArguments == 1) {
      val name: StructuredQName = function.getFunctionName
      val t: SchemaType = config.getSchemaType(name)
      if (t != null && t.isAtomicType) {
        val err: XPathException = new XPathException(
          "Function name " + function.getDisplayName +
            " clashes with the name of the constructor function for an atomic type")
        err.setErrorCode("XQST0034")
        err.setIsStaticError(true)
        throw err
      }
    }
    val local: XQueryFunctionLibrary = getLocalFunctionLibrary
    local.declareFunction(function)
    val main: QueryModule = getTopLevelModule
    main.globalFunctionLibrary.declareFunction(function)
  }

  def bindUnboundFunctionCalls(): Unit = {
    val lib: UnboundFunctionLibrary = functionLibraryList
      .get(unboundFunctionLibraryNr)
      .asInstanceOf[UnboundFunctionLibrary]
    lib.bindUnboundFunctionReferences(functionLibraryList.asInstanceOf[XQueryFunctionBinder], getConfiguration)
  }

  def fixupGlobalFunctions(): Unit = {
    globalFunctionLibrary.fixupGlobalFunctions(this)
  }

  def optimizeGlobalFunctions(): Unit = {
    globalFunctionLibrary.optimizeGlobalFunctions(this)
  }

  def explainGlobalFunctions(out: ExpressionPresenter): Unit = {
    globalFunctionLibrary.asInstanceOf[QueryModule].explainGlobalFunctions(out)
  }

  def getUserDefinedFunction(uri: String,
                             localName: String,
                             arity: Int): UserFunction =
    globalFunctionLibrary.asInstanceOf[QueryModule].getUserDefinedFunction(uri, localName, arity)

  def bindUnboundVariables(): Unit = {
    for (uv <- undeclaredVariables.values.asScala) {
      val qName: StructuredQName = uv.getVariableQName
      var `var`: GlobalVariable = variables.get(qName)
      if (`var` == null) {
        val uri: String = qName.getURI
        if (importsNamespace(uri)) {
          val main: QueryModule = getTopLevelModule
          `var` = main.libraryVariables.get(qName)
        }
      }
      if (`var` == null) {
        val err: XPathException = new XPathException(
          "Unresolved reference to variable $" + uv.getVariableQName.getDisplayName)
        err.setErrorCode("XPST0008")
        err.setIsStaticError(true)
        throw err
      } else if (`var`.isPrivate && `var`.getSystemId != getSystemId) {
        val err: XPathException = new XPathException(
          "Cannot reference a private variable in a different module")
        err.setErrorCode("XPST0008")
        err.setIsStaticError(true)
        throw err
      } else {
        uv.transferReferences(`var`)
      }
    }
  }

  def addImportedSchema(targetNamespace: String,
                        baseURI: String,
                        locationURIs: List[String]): Unit = {
    if (importedSchemata == null) {
      importedSchemata = new HashSet(5)
    }
    importedSchemata.add(targetNamespace)
    var loadedSchemata: HashMap[String, HashSet[String]] =
      getTopLevelModule.loadedSchemata
    if (loadedSchemata == null) {
      loadedSchemata = new HashMap(5)
      getTopLevelModule.loadedSchemata = loadedSchemata
    }
    var entries: HashSet[String] = loadedSchemata.get(targetNamespace)
    if (entries == null) {
      entries = new HashSet(locationURIs.size)
      loadedSchemata.put(targetNamespace, entries)
    }
    for (relative <- locationURIs.asScala) {
      try {
        val abs: URI = ResolveURI.makeAbsolute(relative, baseURI)
        entries.add(abs.toString)
      } catch {
        case e: URISyntaxException => {}

      }
    }
  }

  def isImportedSchema(namespace: String): Boolean =
    importedSchemata != null && importedSchemata.contains(namespace)

  def getImportedSchemaNamespaces(): collection.Set[String] =
    if (importedSchemata == null) {
      Collections.emptySet().asInstanceOf[collection.Set[String]]
    } else {
      importedSchemata.asScala
    }

  def reportStaticError(err: XPathException): Unit = {
    if (!err.hasBeenReported) {
      reportStaticError(new XmlProcessingException(err))
      err.setHasBeenReported(true)
    }
  }

  def reportStaticError(err: XmlProcessingError): Unit = {
    userQueryContext.getErrorReporter.report(err)
    if (err.getFatalErrorMessage != null) {
      throw new XmlProcessingAbort(err.getFatalErrorMessage)
    }
  }

  def makeEarlyEvaluationContext(): XPathContext =
    new EarlyEvaluationContext(getConfiguration)

  def setDefaultCollationName(collation: String): Unit = {
    defaultCollationName = collation
  }

  override def getDefaultCollationName = {
    if (defaultCollationName == null) defaultCollationName = NamespaceConstant.CODEPOINT_COLLATION_URI
    defaultCollationName
  }

  def declarePrologNamespace(prefix: String, uri: String): Unit = {
    if (prefix == null) {
      throw new NullPointerException(
        "Null prefix supplied to declarePrologNamespace()")
    }
    if (uri == null) {
      throw new NullPointerException(
        "Null namespace URI supplied to declarePrologNamespace()")
    }
    if (prefix.==("xml") != uri == NamespaceConstant.XML) {
      val err: XPathException = new XPathException(
        "Invalid declaration of the XML namespace")
      err.setErrorCode("XQST0070")
      err.setIsStaticError(true)
      throw err
    }
    if (explicitPrologNamespaces.get(prefix) != null) {
      val err: XPathException = new XPathException(
        "Duplicate declaration of namespace prefix \"" + prefix +
          '"')
      err.setErrorCode("XQST0033")
      err.setIsStaticError(true)
      throw err
    } else {
      explicitPrologNamespaces.put(prefix, uri)
    }
  }

  def declareActiveNamespace(prefix: String, uri: String): Unit = {
    if (prefix == null) {
      throw new NullPointerException(
        "Null prefix supplied to declareActiveNamespace()")
    }
    if (uri == null) {
      throw new NullPointerException(
        "Null namespace URI supplied to declareActiveNamespace()")
    }
    val entry: NamespaceBinding = new NamespaceBinding(prefix, uri)
    activeNamespaces.push(entry)
  }

  def undeclareNamespace(): Unit = {
    activeNamespaces.pop()
  }

  def getLiveNamespaceResolver(): NamespaceResolver = new NamespaceResolver() {
    def getURIForPrefix(prefix: String, useDefault: Boolean): String =
      checkURIForPrefix(prefix)

    def iteratePrefixes(): Iterator[String] =
      getNamespaceResolver.iteratePrefixes()
  }

  def checkURIForPrefix(prefix: String): String = {
    if (activeNamespaces != null) {
      var i: Int = activeNamespaces.size - 1
      while (i >= 0) {
        if (activeNamespaces.get(i).getPrefix == prefix) {
          val uri: String = activeNamespaces.get(i).getURI
          if (uri.==("") && prefix.!=("")) {
            return null
          }
          return uri
        }
        i -= 1
      }
    }
    if (prefix.isEmpty) {
      return defaultElementNamespace
    }
    var uri: String = explicitPrologNamespaces.get(prefix)
    if (uri != null) {
      return if (uri.isEmpty)  null else uri
    }
    if (userQueryContext != null) {
      uri = userQueryContext.getNamespaceForPrefix(prefix)
      if (uri != null) {
        return uri
      }
    }
    null
  }

  def getDefaultElementNamespace(): String = checkURIForPrefix("")

  def setDefaultElementNamespace(uri: String): Unit = {
    defaultElementNamespace = uri
  }

  def setRevalidationMode(mode: Int): Unit = {
    if (mode == Validation.STRICT || mode == Validation.LAX ||
      mode == Validation.SKIP) {
      revalidationMode = mode
    } else {
      throw new IllegalArgumentException("Invalid mode " + mode)
    }
  }

  def getRevalidationMode: Int = revalidationMode

  def getActiveNamespaceBindings(): NamespaceMap = {
    if (activeNamespaces == null) {
      return NamespaceMap.emptyMap
    }
    var result: NamespaceMap = NamespaceMap.emptyMap
    val prefixes: HashSet[String] = new HashSet[String](10)
    var n: Int = activeNamespaces.size - 1
    while (n >= 0) {
      val an: NamespaceBinding = activeNamespaces.get(n)
      if (!prefixes.contains(an.getPrefix)) {
        prefixes.add(an.getPrefix)
        if (!an.getURI.isEmpty) {
          result = result.put(an.getPrefix, an.getURI)
        }
      }
      {
        n -= 1;
        n + 1
      }
    }
    result
  }

  def getNamespaceResolver(): NamespaceResolver = {
    var result: NamespaceMap = NamespaceMap.emptyMap
    val userDeclaredNamespaces: HashMap[String, String] =
      userQueryContext.getUserDeclaredNamespaces
    for ((key, value) <- userDeclaredNamespaces.asScala) {
      result = result.put(key, value)
    }
    for ((key, value) <- explicitPrologNamespaces.asScala) {
      result = result.put(key, value)
    }
    if (!defaultElementNamespace.isEmpty) {
      result = result.put("", defaultElementNamespace)
    }
    if (activeNamespaces == null) {
      return result
    }
    val prefixes: HashSet[String] = new HashSet[String](10)
    var n: Int = activeNamespaces.size - 1
    while (n >= 0) {
      val an: NamespaceBinding = activeNamespaces.get(n)
      if (!prefixes.contains(an.getPrefix)) {
        prefixes.add(an.getPrefix)
        result =
          if (an.getURI.isEmpty) result.remove(an.getPrefix)
          else result.put(an.getPrefix, an.getURI)
      }
      n -= 1;
      n + 1

    }
    result
  }

  def issueWarning(s: String, locator: Location): Unit = {
    val err: XmlProcessingIncident = new XmlProcessingIncident(s).asWarning()
    err.setLocation(locator)
    err.setHostLanguage(HostLanguage.XQUERY)
    userQueryContext.getErrorReporter.report(err)
  }

  def isInBackwardsCompatibleMode(): Boolean = false

  def getXPathVersion(): Int = 31

  // def getKeyManager(): KeyManager = packageData.getKeyManager // getKeyManager class not exist

  override def resolveTypeAlias(typeName: StructuredQName): ItemType =
    getPackageData.obtainTypeAliasManager.getItemType(typeName)

}
