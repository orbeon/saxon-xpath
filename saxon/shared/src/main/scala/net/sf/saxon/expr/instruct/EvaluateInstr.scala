package net.sf.saxon.expr.instruct

import java.util

import net.sf.saxon.utils.Configuration
import net.sf.saxon.utils.Controller
import net.sf.saxon.expr._
import net.sf.saxon.expr.parser._
import net.sf.saxon.expr.sort.LRUCache
import net.sf.saxon.functions.ExecutableFunctionLibrary
import java.util._

import net.sf.saxon.functions.FunctionLibraryList
import net.sf.saxon.functions.registry.BuiltInFunctionSet
import net.sf.saxon.functions.registry.XPath31FunctionSet
import net.sf.saxon.lib.Feature
import net.sf.saxon.lib.NamespaceConstant
import net.sf.saxon.ma.map.HashTrieMap
import net.sf.saxon.ma.map.MapItem
import net.sf.saxon.model.BuiltInAtomicType
import net.sf.saxon.model.ItemType
import net.sf.saxon.model.Type
import net.sf.saxon.om._
import net.sf.saxon.s9api.Location
import net.sf.saxon.style.StylesheetPackage
import net.sf.saxon.sxpath.IndependentContext
import net.sf.saxon.sxpath.XPathVariable
import net.sf.saxon.trace.ExpressionPresenter
import net.sf.saxon.trans.XPathException
import net.sf.saxon.tree.iter.AtomicIterator
import net.sf.saxon.tree.iter.ManualIterator
import net.sf.saxon.tree.util.FastStringBuffer
import net.sf.saxon.value._
import java.util.function.Predicate

import scala.beans.{BeanProperty, BooleanBeanProperty}
import scala.jdk.CollectionConverters._

class EvaluateInstr(xpath: Expression,
                    private var requiredType: SequenceType,
                    contextItemExpr: Expression,
                    baseUriExpr: Expression,
                    namespaceContextExpr: Expression,
                    schemaAwareExpr: Expression)
  extends Expression {

  private var xpathOp: Operand = _

  private var contextItemOp: Operand = _

  private var baseUriOp: Operand = _

  private var namespaceContextOp: Operand = _

  private var schemaAwareOp: Operand = _

  private var optionsOp: Operand = _

  private var importedSchemaNamespaces: Set[String] = _

  @BeanProperty
  var actualParams: Array[WithParam] = _

  private var dynamicParamsOp: Operand = _

  private var defaultXPathNamespace: String = null

  if (xpath != null) {
    xpathOp = new Operand(this, xpath, OperandRole.SINGLE_ATOMIC)
  }

  if (contextItemExpr != null) {
    contextItemOp = new Operand(this, contextItemExpr, OperandRole.NAVIGATE)
  }

  if (baseUriExpr != null) {
    baseUriOp = new Operand(this, baseUriExpr, OperandRole.SINGLE_ATOMIC)
  }

  if (namespaceContextExpr != null) {
    namespaceContextOp =
      new Operand(this, namespaceContextExpr, OperandRole.INSPECT)
  }

  if (schemaAwareExpr != null) {
    schemaAwareOp =
      new Operand(this, schemaAwareExpr, OperandRole.SINGLE_ATOMIC)
  }

  def setOptionsExpression(options: Expression): Unit = {
    optionsOp = new Operand(this, options, OperandRole.ABSORB)
  }

  def setActualParameters(params: Array[WithParam]): Unit = {
    this.actualParams = params
  }

  def setDefaultXPathNamespace(defaultXPathNamespace: String): Unit = {
    this.defaultXPathNamespace = defaultXPathNamespace
  }

  override def isInstruction(): Boolean = true

  def importSchemaNamespace(ns: String): Unit = {
    if (importedSchemaNamespaces == null) {
      importedSchemaNamespaces = new HashSet()
    }
    importedSchemaNamespaces.add(ns)
  }

  override def typeCheck(visitor: ExpressionVisitor,
                         contextInfo: ContextItemStaticInfo): Expression = {
    importedSchemaNamespaces = visitor.getStaticContext.getImportedSchemaNamespaces.asInstanceOf[util.Set[String]]
    typeCheckChildren(visitor, contextInfo)
    WithParam.typeCheck(getActualParams, visitor, contextInfo)
    this
  }

  override def optimize(visitor: ExpressionVisitor,
                        contextItemType: ContextItemStaticInfo): Expression = {
    optimizeChildren(visitor, contextItemType)
    this
  }

  def getItemType(): ItemType = requiredType.getPrimaryType

   def computeCardinality(): Int = requiredType.getCardinality

  override def addToPathMap(
                             pathMap: PathMap,
                             pathMapNodeSet: PathMap.PathMapNodeSet): PathMap.PathMapNodeSet =
    throw new UnsupportedOperationException(
      "Cannot do document projection when xsl:evaluate is used")

  override def getIntrinsicDependencies(): Int =
    StaticProperty.DEPENDS_ON_FOCUS | StaticProperty.DEPENDS_ON_XSLT_CONTEXT

  override def operands(): java.lang.Iterable[Operand] = {
    val sub: List[Operand] = new ArrayList[Operand](8)
    if (xpathOp != null) {
      sub.add(xpathOp)
    }
    if (contextItemOp != null) {
      sub.add(contextItemOp)
    }
    if (baseUriOp != null) {
      sub.add(baseUriOp)
    }
    if (namespaceContextOp != null) {
      sub.add(namespaceContextOp)
    }
    if (schemaAwareOp != null) {
      sub.add(schemaAwareOp)
    }
    if (dynamicParamsOp != null) {
      sub.add(dynamicParamsOp)
    }
    if (optionsOp != null) {
      sub.add(optionsOp)
    }
    WithParam.gatherOperands(this, getActualParams, sub)
    sub
  }

  def getImplementationMethod(): Int = Expression.ITERATE_METHOD

  def copy(rebindings: RebindingMap): Expression = {
    val e2: EvaluateInstr = new EvaluateInstr(
      getXpath.copy(rebindings),
      requiredType,
      getContextItemExpr.copy(rebindings),
      if (getBaseUriExpr == null) null else getBaseUriExpr.copy(rebindings),
      if (getNamespaceContextExpr == null) null
      else getNamespaceContextExpr.copy(rebindings),
      if (getSchemaAwareExpr == null) null
      else getSchemaAwareExpr.copy(rebindings)
    )
    ExpressionTool.copyLocationInfo(this, e2)
    e2.setRetainedStaticContext(getRetainedStaticContext)
    e2.importedSchemaNamespaces = importedSchemaNamespaces
    e2.setActualParams(WithParam.copy(e2, getActualParams, rebindings))
    if (optionsOp != null) {
      e2.setOptionsExpression(optionsOp.getChildExpression.copy(rebindings))
    }
    if (dynamicParamsOp != null) {
      e2.setDynamicParams(dynamicParamsOp.getChildExpression.copy(rebindings))
    }
    e2
  }

  override def iterate(context: XPathContext): SequenceIterator = {
    val config: Configuration = context.getConfiguration
    if (config.getBooleanProperty(Feature.DISABLE_XSL_EVALUATE)) {
      throw new XPathException("xsl:evaluate has been disabled", "XTDE3175")
    }
    val exprText: String = getXpath.evaluateAsString(context).toString
    val baseUri: String =
      if (getBaseUriExpr == null) getStaticBaseURIString
      else Whitespace.trim(getBaseUriExpr.evaluateAsString(context))
    val focus: Item = getContextItemExpr.evaluateItem(context)
    var namespaceContextBase: NodeInfo = null
    if (getNamespaceContextExpr != null) {
      namespaceContextBase =
        getNamespaceContextExpr.evaluateItem(context).asInstanceOf[NodeInfo]
    }
    val schemaAwareAttr: String =
      Whitespace.trim(getSchemaAwareExpr.evaluateAsString(context))
    var isSchemaAware: Boolean = false
    if ("yes" == schemaAwareAttr || "true" == schemaAwareAttr ||
      "1" == schemaAwareAttr) {
      isSchemaAware = true
    } else if ("no" == schemaAwareAttr || "false" == schemaAwareAttr ||
      "0" == schemaAwareAttr) {
      isSchemaAware = false
    } else {
      val err: XPathException = new XPathException(
        "The schema-aware attribute of xsl:evaluate must be yes|no|true|false|0|1")
      err.setErrorCode("XTDE0030")
      err.setLocation(getLocation)
      err.setXPathContext(context)
      throw err
    }
    var expr: Expression = null
    var slotMap: SlotManager = null
    val fsb: FastStringBuffer = new FastStringBuffer(
      exprText.length + (if (baseUri == null) 4 else baseUri.length) +
        40)
    fsb.append(baseUri)
    fsb.append("##")
    fsb.append(schemaAwareAttr)
    fsb.append("##")
    fsb.append(exprText)
    if (namespaceContextBase != null) {
      fsb.append("##")
      namespaceContextBase.generateId(fsb)
    }
    val cacheKey: String = fsb.toString
    var declaredVars: Collection[XPathVariable] = null
    val controller: Controller = context.getController
    var cache: LRUCache[String, Array[Any]] = null
    controller.synchronized {
      cache = controller
        .getUserData(this.getLocation, "xsl:evaluate")
        .asInstanceOf[LRUCache[String, Array[Any]]]
      if (cache == null) {
        cache = new LRUCache(100)
        controller.setUserData(this.getLocation, "xsl:evaluate", cache)
      } else {
        val o: Array[Any] = cache.get(cacheKey)
        if (o != null) {
          expr = o(0).asInstanceOf[Expression]
          slotMap = o(1).asInstanceOf[SlotManager]
          declaredVars = o(2).asInstanceOf[Collection[XPathVariable]]
        }
      }
    }
    var dynamicParams: MapItem = null
    if (dynamicParamsOp != null) {
      dynamicParams = dynamicParamsOp.getChildExpression
        .evaluateItem(context)
        .asInstanceOf[MapItem]
    }
    if (expr == null) {
      val options: MapItem =
        (if (optionsOp == null) new HashTrieMap()
        else
          optionsOp.getChildExpression
            .evaluateItem(context)
            .asInstanceOf[MapItem])
      val env: IndependentContext = new IndependentContext(config) {
        override def issueWarning(s: String, locator: Location): Unit = {
          var message: String = "In dynamic expression {" + exprText + "}: " + s
          context.getController.warning(message, null, getLocation)
        }
      }
      env.setBaseURI(baseUri)
      env.setExecutable(context.getController.getExecutable)
      env.setXPathLanguageLevel(31)
      env.setDefaultCollationName(
        getRetainedStaticContext.getDefaultCollationName)
      if (getNamespaceContextExpr != null) {
        env.setNamespaces(namespaceContextBase)
      } else {
        env.setNamespaceResolver(getRetainedStaticContext)
        env.setDefaultElementNamespace(
          getRetainedStaticContext.getDefaultElementNamespace.asInstanceOf[String])
      }
      val libraryList0: FunctionLibraryList =
        getRetainedStaticContext.getPackageData
          .asInstanceOf[StylesheetPackage]
          .getFunctionLibrary
      val libraryList1: FunctionLibraryList = new FunctionLibraryList()
      for (lib <- libraryList0.getLibraryList.asScala) {
        if (lib.isInstanceOf[BuiltInFunctionSet] &&
          lib
            .asInstanceOf[BuiltInFunctionSet]
            .getNamespace == NamespaceConstant.FN) {
          libraryList1.addFunctionLibrary(XPath31FunctionSet.getInstance)
        } /*else if (/*lib.isInstanceOf[StylesheetFunctionLibrary] ||*/ lib // StylesheetFunctionLibrary scala class does not exist
          .isInstanceOf[ExecutableFunctionLibrary]) {
          libraryList1.addFunctionLibrary(
            new PublicStylesheetFunctionLibrary(lib)) // PublicStylesheetFunctionLibrary scala class does not exist
        }*/ else {
          libraryList1.addFunctionLibrary(lib)
        }
      }
      env.setFunctionLibrary(libraryList1)
      env.setDecimalFormatManager(
        getRetainedStaticContext.getDecimalFormatManager)
      env.setXPathLanguageLevel(
        config.getConfigurationProperty(Feature.XPATH_VERSION_FOR_XSLT))
      if (isSchemaAware) {
        val allowAny: GroundedValue =
          options.get(new StringValue("allow-any-namespace"))
        if (allowAny != null && allowAny.effectiveBooleanValue()) {
          env.setImportedSchemaNamespaces(config.getImportedNamespaces.asInstanceOf[util.Set[String]])
        } else {
          env.setImportedSchemaNamespaces(importedSchemaNamespaces)
        }
      }
      val defaultCollation: GroundedValue =
        options.get(new StringValue("default-collation"))
      if (defaultCollation != null) {
        env.setDefaultCollationName(defaultCollation.head().getStringValue)
      }
      val locals: Map[StructuredQName, Integer] =
        new HashMap[StructuredQName, Integer]()
      if (dynamicParams != null) {
        dynamicParams.keys.forEachOrFail((paramName) => {
          if (!(paramName.isInstanceOf[QNameValue])) {
            val err: XPathException = new XPathException(
              "Parameter names supplied to xsl:evaluate must have type xs:QName, not " +
                paramName
                  .asInstanceOf[AtomicValue]
                  .getItemType
                  .getPrimitiveItemType
                  .getDisplayName,
              "XTTE3165"
            )
            err.setIsTypeError(true)
            throw err
          }
          val `var`: XPathVariable =
            env.declareVariable(paramName.asInstanceOf[QNameValue])
          locals.put(paramName.asInstanceOf[QNameValue].getStructuredQName,
            `var`.getLocalSlotNumber)
        })
      }
      if (getActualParams != null) {
        for (actualParam <- getActualParams) {
          val name: StructuredQName = actualParam.getVariableQName
          if (locals.get(name) == null) {
            val `var`: XPathVariable = env.declareVariable(name)
            locals.put(name, `var`.getLocalSlotNumber)
          }
        }
      }
      try expr = ExpressionTool.make(exprText, env, 0, Token.EOF, null)
      catch {
        case e: XPathException => {
          val err: XPathException = new XPathException(
            "Static error in XPath expression supplied to xsl:evaluate: " +
              e.getMessage +
              ". Expression: {" +
              exprText +
              "}")
          err.setErrorCode("XTDE3160")
          err.setLocation(getLocation)
          throw err
        }

      }
      expr.setRetainedStaticContext(env.makeRetainedStaticContext())
      val role: RoleDiagnostic =
        new RoleDiagnostic(RoleDiagnostic.EVALUATE_RESULT, exprText, 0)
      val visitor: ExpressionVisitor = ExpressionVisitor.make(env)
      val tc: TypeChecker = config.getTypeChecker(false)
      expr = tc.staticTypeCheck(expr, requiredType, role, visitor)
      val contextItemType: ItemType = Type.ITEM_TYPE
      expr = ExpressionTool.resolveCallsToCurrentFunction(expr)
      val cit: ContextItemStaticInfo = config.makeContextItemStaticInfo(
        contextItemType,
        context.getContextItem == null)
      expr = expr.typeCheck(visitor, cit).optimize(visitor, cit)
      slotMap = env.getStackFrameMap
      ExpressionTool.allocateSlots(expr, slotMap.getNumberOfVariables, slotMap)
      if (cacheKey != null) {
        declaredVars = env.getDeclaredVariables
        cache.put(cacheKey, Array(expr, slotMap, declaredVars))
      }
    }
    val c2: XPathContextMajor = context.newContext()
    if (focus == null) {
      c2.setCurrentIterator(null)
    } else {
      val mono: ManualIterator = new ManualIterator(focus)
      c2.setCurrentIterator(mono)
    }
    c2.openStackFrame(slotMap)
    if (getActualParams != null) {
      for (i <- 0 until getActualParams.length) {
        val slot: Int =
          slotMap.getVariableMap.indexOf(getActualParams()(i).getVariableQName)
        c2.setLocalVariable(slot, getActualParams()(i).getSelectValue(context))
      }
    }
    if (dynamicParams != null) {
      val iter = dynamicParams.keys
      var paramName: QNameValue = null
      while (({
        paramName = iter.next().asInstanceOf[QNameValue]
        paramName
      }) != null) {
        val slot: Int =
          slotMap.getVariableMap.indexOf(paramName.getStructuredQName)
        if (slot >= 0) {
          c2.setLocalVariable(slot, dynamicParams.get(paramName))
        }
      }
    }
    for (decVal <- declaredVars.asScala) {
      val name: StructuredQName = decVal.getVariableQName
      val nameMatch: Predicate[Expression] = (e) =>
        e.isInstanceOf[LocalVariableReference] &&
          e.asInstanceOf[LocalVariableReference].getVariableName == name &&
          e.asInstanceOf[LocalVariableReference]
            .getBinding
            .isInstanceOf[XPathVariable]
      if (dynamicParams != null &&
        dynamicParams.get(new QNameValue(name, BuiltInAtomicType.QNAME)) ==
          null &&
        !isActualParam(name) &&
        ExpressionTool.contains(expr, sameFocusOnly = false, nameMatch)) {
        throw new XPathException(
          "No value has been supplied for variable " + name.getDisplayName,
          "XPST0008")
      }
    }
    try expr.iterate(c2)
    catch {
      case err: XPathException => {
        val e2: XPathException = new XPathException(
          "Dynamic error in expression {" + exprText + "} called using xsl:evaluate",
          err)
        e2.setLocation(getLocation)
        e2.setErrorCodeQName(err.getErrorCodeQName)
        throw e2
      }

    }
  }

  def export(out: ExpressionPresenter): Unit = {
    out.startElement("evaluate", this)
    if (SequenceType.ANY_SEQUENCE != requiredType) {
      out.emitAttribute("as", requiredType.toAlphaCode)
    }
    if (importedSchemaNamespaces != null && !importedSchemaNamespaces.isEmpty) {
      val buff: FastStringBuffer = new FastStringBuffer(256)
      for (s <- importedSchemaNamespaces.asScala) {
        var str = s
        if (str.isEmpty) {
          str = "##"
        }
        buff.append(str)
        buff.cat(' ')
      }
      buff.setLength(buff.length - 1)
      out.emitAttribute("schNS", buff.toString)
    }
    if (defaultXPathNamespace != null) {
      out.emitAttribute("dxns", defaultXPathNamespace)
    }
    out.setChildRole("xpath")
    getXpath.export(out)
    if (getContextItemExpr != null) {
      out.setChildRole("cxt")
      getContextItemExpr.export(out)
    }
    if (getBaseUriExpr != null) {
      out.setChildRole("baseUri")
      getBaseUriExpr.export(out)
    }
    if (getNamespaceContextExpr != null) {
      out.setChildRole("nsCxt")
      getNamespaceContextExpr.export(out)
    }
    if (getSchemaAwareExpr != null) {
      out.setChildRole("sa")
      getSchemaAwareExpr.export(out)
    }
    if (optionsOp != null) {
      out.setChildRole("options")
      optionsOp.getChildExpression.export(out)
    }
    WithParam.exportParameters(actualParams, out, tunnel = false)
    if (dynamicParamsOp != null) {
      out.setChildRole("wp")
      getDynamicParams.export(out)
    }
    out.endElement()
  }

  def getXpath(): Expression = xpathOp.getChildExpression

  def setXpath(xpath: Expression): Unit = {
    if (xpathOp == null) {
      xpathOp = new Operand(this, xpath, OperandRole.SINGLE_ATOMIC)
    } else {
      xpathOp.setChildExpression(xpath)
    }
  }

  def getContextItemExpr(): Expression =
    if (contextItemOp == null) null else contextItemOp.getChildExpression

  def setContextItemExpr(contextItemExpr: Expression): Unit = {
    if (contextItemOp == null) {
      contextItemOp = new Operand(this, contextItemExpr, OperandRole.NAVIGATE)
    } else {
      contextItemOp.setChildExpression(contextItemExpr)
    }
  }

  def getBaseUriExpr(): Expression =
    if (baseUriOp == null) null else baseUriOp.getChildExpression

  def setBaseUriExpr(baseUriExpr: Expression): Unit = {
    if (baseUriOp == null) {
      baseUriOp = new Operand(this, baseUriExpr, OperandRole.SINGLE_ATOMIC)
    } else {
      baseUriOp.setChildExpression(baseUriExpr)
    }
  }

  def getNamespaceContextExpr(): Expression =
    if (namespaceContextOp == null) null
    else namespaceContextOp.getChildExpression

  def setNamespaceContextExpr(namespaceContextExpr: Expression): Unit = {
    if (namespaceContextOp == null) {
      namespaceContextOp =
        new Operand(this, namespaceContextExpr, OperandRole.INSPECT)
    } else {
      namespaceContextOp.setChildExpression(namespaceContextExpr)
    }
  }

  def getSchemaAwareExpr(): Expression =
    if (schemaAwareOp == null) null else schemaAwareOp.getChildExpression

  def setSchemaAwareExpr(schemaAwareExpr: Expression): Unit = {
    if (schemaAwareOp == null) {
      schemaAwareOp =
        new Operand(this, schemaAwareExpr, OperandRole.SINGLE_ATOMIC)
    } else {
      schemaAwareOp.setChildExpression(schemaAwareExpr)
    }
  }

  def isActualParam(name: StructuredQName): Boolean =
    actualParams
      .find(_.getVariableQName == name)
      .map(_ => true)
      .getOrElse(false)

  def setDynamicParams(params: Expression): Unit = {
    if (dynamicParamsOp == null) {
      dynamicParamsOp = new Operand(this, params, OperandRole.SINGLE_ATOMIC)
    } else {
      dynamicParamsOp.setChildExpression(params)
    }
  }

  def getDynamicParams(): Expression = dynamicParamsOp.getChildExpression

}
