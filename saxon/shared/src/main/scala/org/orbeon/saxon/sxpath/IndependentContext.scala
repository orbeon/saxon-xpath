package org.orbeon.saxon.sxpath

import java.util._

import org.orbeon.saxon.expr.{Expression, LocalVariableReference, PackageData}
import org.orbeon.saxon.expr.instruct.{Executable, SlotManager}
import org.orbeon.saxon.expr.parser.{OptimizerOptions, RetainedStaticContext}
import org.orbeon.saxon.functions.FunctionLibraryList
import org.orbeon.saxon.lib.NamespaceConstant
import org.orbeon.saxon.model.{AnyItemType, ItemType, Type}
import org.orbeon.saxon.om.{AxisInfo, NamespaceResolver, NodeInfo, StructuredQName}
import org.orbeon.saxon.s9api.HostLanguage
import org.orbeon.saxon.trans.XPathException
import org.orbeon.saxon.tree.iter.AxisIterator
import org.orbeon.saxon.value.QNameValue

//import scala.collection.compat._
import org.orbeon.saxon.utils.Configuration

import scala.jdk.CollectionConverters._


class IndependentContext(config: Configuration)
  extends AbstractStaticContext
    with XPathStaticContext
    with NamespaceResolver {

  var namespaces: HashMap[String, String] = new HashMap(10)
  var variables: HashMap[StructuredQName, XPathVariable] = new HashMap(20)
  var externalResolver: NamespaceResolver = null
  var requiredContextItemType: ItemType = AnyItemType
  var importedSchemaNamespaces: Set[String] = new HashSet()
  var autoDeclare: Boolean = false
  var executable: Executable = _
  var retainedStaticContext: RetainedStaticContext = _
  var optimizerOptions: OptimizerOptions = config.getOptimizerOptions
  var parentlessContextItem: Boolean = _

  this.setConfiguration(config)
  clearNamespaces()
  this.setDefaultFunctionLibrary(31)

  usingDefaultFunctionLibrary = true
  this.defaultCollationName = config.getDefaultCollationName

  val pd: PackageData = new PackageData(config)
  pd.setHostLanguage(HostLanguage.XPATH)
  pd.setSchemaAware(false)
  this.packageData = pd

  def this() = this(new Configuration)

  def this(ic: IndependentContext) = {
    this(ic.getConfiguration)

    this.packageData = ic.getPackageData
    this.setBaseURI(ic.getStaticBaseURI)
    this.containingLocation = ic.getContainingLocation
    this.defaultElementNamespace = ic.getDefaultElementNamespace
    this.defaultFunctionNamespace = ic.getDefaultFunctionNamespace
    this.setBackwardsCompatibilityMode(ic.isInBackwardsCompatibleMode)

    namespaces = new HashMap(ic.namespaces)
    variables = new HashMap(10)

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
    val kind: Int = nodeInf.getNodeKind
    if (kind == Type.ATTRIBUTE || kind == Type.TEXT || kind == Type.COMMENT ||
      kind == Type.PROCESSING_INSTRUCTION ||
      kind == Type.NAMESPACE) {
      nodeInf = nodeInf.getParent
    }
    if (nodeInf == null) {
      return
    }
    val iter: AxisIterator = nodeInf.iterateAxis(AxisInfo.NAMESPACE)
    while (true) {
      val ns: NodeInfo = iter.next()
      if (ns == null) {
        return
      }
      val prefix: String = ns.getLocalPart
      if ("" == prefix) {
        this.defaultElementNamespace = ns.getStringValue
      } else {
        declareNamespace(ns.getLocalPart, ns.getStringValue)
      }
    }
  }

  def setNamespaceResolver(resolver: NamespaceResolver): Unit = {
    externalResolver = resolver
  }

  def setAllowUndeclaredVariables(allow: Boolean): Unit = {
    autoDeclare = allow
  }

  def isAllowUndeclaredVariables: Boolean = autoDeclare

  def declareVariable(qname: QNameValue): XPathVariable =
    declareVariable(qname.getStructuredQName)

  def declareVariable(namespaceURI: String, localName: String): XPathVariable = {
    val qName: StructuredQName =
      new StructuredQName("", namespaceURI, localName)
    declareVariable(qName)
  }

  def declareVariable(qName: StructuredQName): XPathVariable = {
    var `var`: XPathVariable = variables.get(qName)
    if (`var` != null) {
      `var`
    } else {
      `var` = XPathVariable.make(qName)
      val slot: Int = variables.size
      `var`.setSlotNumber(slot)
      variables.put(qName, `var`)
      `var`
    }
  }

  def iterateExternalVariables(): Iterator[XPathVariable] =
    variables.values.iterator

  def getExternalVariable(qName: StructuredQName): XPathVariable =
    variables.get(qName)

  def getSlotNumber(qname: QNameValue): Int = {
    val sq: StructuredQName = qname.getStructuredQName
    val `var`: XPathVariable = variables.get(sq)
    if (`var` == null) return -1
    `var`.getLocalSlotNumber
  }

  def getNamespaceResolver: NamespaceResolver =
    if (externalResolver != null) {
      externalResolver
    } else {
      this
    }

  def getURIForPrefix(prefix: String, useDefault: Boolean): String = {
    if (externalResolver != null) {
      externalResolver.getURIForPrefix(prefix, useDefault)
    }
    if (prefix.isEmpty) {
      if (useDefault) getDefaultElementNamespace else ""
    } else {
      namespaces.get(prefix)
    }
  }

  def iteratePrefixes: Iterator[String] =
    if (externalResolver != null) {
      externalResolver.iteratePrefixes
    } else {
      namespaces.keySet.iterator
    }

  def bindVariable(qName: StructuredQName): Expression = {
    val `var`: XPathVariable = variables.get(qName)
    if (`var` == null) {
      if (autoDeclare) {
        new LocalVariableReference(declareVariable(qName))
      } else {
        throw new XPathException(
          "Undeclared variable in XPath expression: $" + qName.getClarkName,
          "XPST0008")
      }
    } else {
      new LocalVariableReference(`var`)
    }
  }

  def getStackFrameMap: SlotManager = {
    val map: SlotManager = getConfiguration.makeSlotManager
    val va: Array[XPathVariable] = Array.ofDim[XPathVariable](variables.size)
    for (value <- variables.values.asScala) {
      va(value.getLocalSlotNumber) = value
    }
    for (v <- va) {
      map.allocateSlotNumber(v.getVariableQName)
    }
    map
  }

  def getDeclaredVariables: Collection[XPathVariable] = variables.values

  def isImportedSchema(namespace: String): Boolean =
    importedSchemaNamespaces.contains(namespace)

  def getImportedSchemaNamespaces: collection.Set[String] = importedSchemaNamespaces.asScala.toSet

  def setImportedSchemaNamespaces(namespaces: Set[String]): Unit = {
    importedSchemaNamespaces = namespaces
    if (!namespaces.isEmpty) {
      this.setSchemaAware(true)
    }
  }

  def setRequiredContextItemType(`type`: ItemType): Unit = {
    requiredContextItemType = `type`
  }

  override def getRequiredContextItemType: ItemType = requiredContextItemType

  def setOptimizerOptions(options: OptimizerOptions): Unit =
    this.optimizerOptions = options

  override def getOptimizerOptions: OptimizerOptions = this.optimizerOptions

  def setExecutable(exec: Executable): Unit =
    executable = exec

  def getExecutable: Executable = executable
  def getColumnNumber: Int = -1
  def getPublicId: String = null
  def getLineNumber: Int = -1

  override def isContextItemParentless: Boolean = parentlessContextItem

  def setContextItemParentless(parentless: Boolean): Unit =
    parentlessContextItem = parentless
}
