package org.orbeon.saxon.sxpath

import java.{util => ju}

import org.orbeon.saxon.expr.instruct.{Executable, SlotManager}
import org.orbeon.saxon.expr.parser.{OptimizerOptions, RetainedStaticContext}
import org.orbeon.saxon.expr.{Expression, LocalVariableReference, PackageData}
import org.orbeon.saxon.functions.FunctionLibraryList
import org.orbeon.saxon.lib.NamespaceConstant
import org.orbeon.saxon.model.{AnyItemType, ItemType, Type}
import org.orbeon.saxon.om.{AxisInfo, NamespaceResolver, NodeInfo, StructuredQName}
import org.orbeon.saxon.s9api.HostLanguage
import org.orbeon.saxon.trans.XPathException
import org.orbeon.saxon.value.QNameValue

import org.orbeon.saxon.utils.Configuration

import scala.jdk.CollectionConverters._


class IndependentContext(config: Configuration)
  extends AbstractStaticContext
    with XPathStaticContext
    with NamespaceResolver {

  var namespaces               : ju.HashMap[String, String] = new ju.HashMap(10)
  var variables                : ju.HashMap[StructuredQName, XPathVariable] = new ju.HashMap(20)
  var externalResolver         : NamespaceResolver = null
  var requiredContextItemType  : ItemType = AnyItemType
  var importedSchemaNamespaces : ju.Set[String] = new ju.HashSet
  var autoDeclare              : Boolean = false
  var executable               : Executable = _
  var retainedStaticContext    : RetainedStaticContext = _
  var optimizerOptions         : OptimizerOptions = config.getOptimizerOptions
  var parentlessContextItem    : Boolean = _

  // Primary constructor
  locally {
    this.setConfiguration(config)
    clearNamespaces()
    this.setDefaultFunctionLibrary(31)

    this.usingDefaultFunctionLibrary = true
    this.defaultCollationName = config.getDefaultCollationName // already set by `setConfiguration()` above

    this.packageData = {
      val pd = new PackageData(config)
      pd.setHostLanguage(HostLanguage.XPATH)
      pd.setSchemaAware(false)
      pd
    }
  }

  def this() = this(new Configuration)

  def this(ic: IndependentContext) = {
    this(ic.getConfiguration)

    this.packageData = ic.getPackageData
    this.setBaseURI(ic.getStaticBaseURI)
    this.containingLocation = ic.getContainingLocation
    this.defaultElementNamespace = ic.getDefaultElementNamespace
    this.defaultFunctionNamespace = ic.getDefaultFunctionNamespace
    this.setBackwardsCompatibilityMode(ic.isInBackwardsCompatibleMode)

    namespaces = new ju.HashMap(ic.namespaces)
    variables = new ju.HashMap(10)

    val libList = ic.getFunctionLibrary.asInstanceOf[FunctionLibraryList]
    if (libList != null)
      this.setFunctionLibrary(libList.copy().asInstanceOf[FunctionLibraryList])

    this.importedSchemaNamespaces = ic.importedSchemaNamespaces
    externalResolver = ic.externalResolver
    autoDeclare = ic.autoDeclare
    this.setXPathLanguageLevel(ic.getXPathVersion)
    requiredContextItemType = ic.requiredContextItemType
    this.executable = ic.getExecutable
  }

  override def makeRetainedStaticContext(): RetainedStaticContext = {
    if (retainedStaticContext == null)
      retainedStaticContext = new RetainedStaticContext(this)
    retainedStaticContext
  }

  def declareNamespace(prefix: String, uri: String): Unit = {

    if (prefix == null)
      throw new NullPointerException("Null prefix supplied to declareNamespace()")

    if (uri == null)
      throw new NullPointerException("Null namespace URI supplied to declareNamespace()")

    if ("" == prefix)
      this.defaultElementNamespace = uri
    else
      namespaces.put(prefix, uri)
  }

  override def setDefaultElementNamespace(uri: String): Unit = {
    var uriStr = uri
    if (uriStr == null)
      uriStr = ""
    super.setDefaultElementNamespace(uriStr)
    namespaces.put("", uriStr)
  }

  def clearNamespaces(): Unit = {
    namespaces.clear()
    declareNamespace("xml", NamespaceConstant.XML)
    declareNamespace("xsl", NamespaceConstant.XSLT)
    declareNamespace("saxon", NamespaceConstant.SAXON)
    declareNamespace("xs", NamespaceConstant.SCHEMA)
    declareNamespace("", "")
  }

  def clearAllNamespaces(): Unit = {
    namespaces.clear()
    declareNamespace("xml", NamespaceConstant.XML)
    declareNamespace("", "")
  }

  def setNamespaces(node: NodeInfo): Unit = {
    var nodeInf = node
    namespaces.clear()
    val kind = nodeInf.getNodeKind
    if (kind == Type.ATTRIBUTE || kind == Type.TEXT || kind == Type.COMMENT ||
      kind == Type.PROCESSING_INSTRUCTION ||
      kind == Type.NAMESPACE) {
      nodeInf = nodeInf.getParent
    }
    if (nodeInf == null)
      return
    val iter = nodeInf.iterateAxis(AxisInfo.NAMESPACE)
    while (true) {
      val ns = iter.next()
      if (ns == null)
        return
      val prefix = ns.getLocalPart
      if ("" == prefix)
        this.defaultElementNamespace = ns.getStringValue
      else
        declareNamespace(ns.getLocalPart, ns.getStringValue)
    }
  }

  def setNamespaceResolver(resolver: NamespaceResolver): Unit =
    externalResolver = resolver

  def setAllowUndeclaredVariables(allow: Boolean): Unit =
    autoDeclare = allow

  def isAllowUndeclaredVariables: Boolean = autoDeclare

  def declareVariable(qname: QNameValue): XPathVariable =
    declareVariable(qname.getStructuredQName)

  def declareVariable(namespaceURI: String, localName: String): XPathVariable =
    declareVariable(new StructuredQName("", namespaceURI, localName))

  def declareVariable(qName: StructuredQName): XPathVariable = {
    var variable = variables.get(qName)
    if (variable != null) {
      variable
    } else {
      variable = XPathVariable(qName, slotNumber = variables.size)
      variables.put(qName, variable)
      variable
    }
  }

  def iterateExternalVariables(): ju.Iterator[XPathVariable] =
    variables.values.iterator

  def getExternalVariable(qName: StructuredQName): XPathVariable =
    variables.get(qName)

  def getSlotNumber(qname: QNameValue): Int = {
    val sq = qname.getStructuredQName
    val variable = variables.get(sq)
    if (variable == null)
      -1
    else
      variable.getLocalSlotNumber
  }

  def getNamespaceResolver: NamespaceResolver =
    if (externalResolver != null)
      externalResolver
    else
      this

  def getURIForPrefix(prefix: String, useDefault: Boolean): String =
    if (externalResolver != null) {
      externalResolver.getURIForPrefix(prefix, useDefault)
    } else if (prefix.isEmpty) {
      if (useDefault) getDefaultElementNamespace else ""
    } else {
      namespaces.get(prefix)
    }

  def iteratePrefixes: ju.Iterator[String] =
    if (externalResolver != null)
      externalResolver.iteratePrefixes
    else
      namespaces.keySet.iterator

  def bindVariable(qName: StructuredQName): Expression = {
    val variable = variables.get(qName)
    if (variable == null) {
      if (autoDeclare) {
        new LocalVariableReference(declareVariable(qName))
      } else {
        throw new XPathException(
          "Undeclared variable in XPath expression: $" + qName.getClarkName,
          "XPST0008")
      }
    } else {
      new LocalVariableReference(variable)
    }
  }

  def getStackFrameMap: SlotManager = {
    val map = getConfiguration.makeSlotManager
    val va = Array.ofDim[XPathVariable](variables.size)
    for (value <- variables.values.asScala)
      va(value.getLocalSlotNumber) = value
    for (v <- va)
      map.allocateSlotNumber(v.getVariableQName)
    map
  }

  def getDeclaredVariables: ju.Collection[XPathVariable] = variables.values

  def isImportedSchema(namespace: String): Boolean =
    importedSchemaNamespaces.contains(namespace)

  def getImportedSchemaNamespaces: ju.Set[String] = importedSchemaNamespaces

  def setImportedSchemaNamespaces(namespaces: ju.Set[String]): Unit = {
    importedSchemaNamespaces = namespaces
    if (! namespaces.isEmpty)
      this.setSchemaAware(true)
  }

  def setRequiredContextItemType(`type`: ItemType): Unit =
    requiredContextItemType = `type`

  override def getRequiredContextItemType: ItemType = requiredContextItemType

  def setOptimizerOptions(options: OptimizerOptions): Unit =
    this.optimizerOptions = options

  override def getOptimizerOptions: OptimizerOptions = this.optimizerOptions

  def setExecutable(exec: Executable): Unit =
    executable = exec

  def getExecutable   : Executable = executable
  def getColumnNumber : Int = -1
  def getPublicId     : String = null
  def getLineNumber   : Int = -1

  def isContextItemParentless: Boolean = parentlessContextItem

  def setContextItemParentless(parentless: Boolean): Unit =
    parentlessContextItem = parentless
}
