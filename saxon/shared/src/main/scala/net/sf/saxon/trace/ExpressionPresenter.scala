package net.sf.saxon.trace

import net.sf.saxon.utils.Configuration
import net.sf.saxon.event._
import net.sf.saxon.expr.Component
import net.sf.saxon.expr.Expression
import net.sf.saxon.expr.parser.Loc
import net.sf.saxon.expr.parser.RetainedStaticContext
import net.sf.saxon.lib.Logger
import net.sf.saxon.lib.NamespaceConstant
import net.sf.saxon.lib.SaxonOutputKeys
import net.sf.saxon.model.BuiltInAtomicType
import net.sf.saxon.model.TypeHierarchy
import net.sf.saxon.model.Untyped
import net.sf.saxon.om._
import net.sf.saxon.serialize.SerializationProperties
import net.sf.saxon.serialize.charcode.UTF16CharacterSet
import net.sf.saxon.trans.XPathException
import net.sf.saxon.tree.util.FastStringBuffer
import net.sf.saxon.value.Whitespace
import javax.xml.transform.OutputKeys
import javax.xml.transform.stream.StreamResult
import java.util.Iterator
import java.util.Map
import java.util.Stack

import ExpressionPresenter._
import net.sf.saxon.style.StylesheetPackage

import scala.beans.{BeanProperty, BooleanBeanProperty}

object ExpressionPresenter {

  def defaultDestination(config: Configuration, out: Logger): Receiver = {
    val props: SerializationProperties = makeDefaultProperties(config)
    config.getSerializerFactory.getReceiver(out.asStreamResult(), props)
  }

  def makeDefaultProperties(config: Configuration): SerializationProperties = {
    val props: SerializationProperties = new SerializationProperties()
    props.setProperty(OutputKeys.METHOD, "xml")
    props.setProperty(OutputKeys.INDENT, "yes")
    if (config.isLicensedFeature(
      Configuration.LicenseFeature.PROFESSIONAL_EDITION)) {
      props.setProperty(SaxonOutputKeys.INDENT_SPACES, "1")
      props.setProperty(SaxonOutputKeys.LINE_LENGTH, "4096")
    }
    props.setProperty(OutputKeys.OMIT_XML_DECLARATION, "no")
    props.setProperty(OutputKeys.ENCODING, "utf-8")
    props.setProperty(OutputKeys.VERSION, "1.0")
    props.setProperty(SaxonOutputKeys.SINGLE_QUOTES, "yes")
    props
  }

  def jsEscape(in: String): String = {
    val out: FastStringBuffer = new FastStringBuffer(in.length)
    for (i <- 0 until in.length) {
      val c: Char = in.charAt(i)
      c match {
        case '\'' => out.append("\\'")
        case '"' => out.append("\\\"")
        case '\b' => out.append("\\b")
        case '\f' => out.append("\\f")
        case '\n' => out.append("\\n")
        case '\r' => out.append("\\r")
        case '\t' => out.append("\\t")
        case '\\' => out.append("\\\\")
        case _ =>
          if (c < 32 || (c > 127 && c < 160) || c > UTF16CharacterSet.SURROGATE1_MIN) {
            out.append("\\u")
            val hex: StringBuilder = new StringBuilder(
              java.lang.Integer.toHexString(c).toUpperCase())
            while (hex.length < 4) hex.insert(0, "0")
            out.append(hex.toString)
          } else {
            out.cat(c)
          }

      }
    }
    out.toString
  }

  trait Options

  class ExportOptions extends Options {

    var target: String = ""

    var targetVersion: Int = 0

    var componentMap: Map[Component, Integer] = _

    var explaining: Boolean = _

    var suppressStaticContext: Boolean = _

    var addStaticType: Boolean = _

    var rootPackage: StylesheetPackage = _

    var packageMap: Map[StylesheetPackage, Integer] = _
  }

}

class ExpressionPresenter {

  private var config: Configuration = _

  private var receiver: Receiver = _

  private var cco: ComplexContentOutputter = _

  private var depth: Int = 0

  private var inStartTag: Boolean = false

  private var nextRole: String = null

  private var expressionStack: Stack[Expression] = new Stack()

  private var nameStack: Stack[String] = new Stack()

  private var namespaceMap: NamespaceMap = NamespaceMap.emptyMap

  private var defaultNamespace: String = _

  @BeanProperty
  var options: Options = new ExportOptions()

  @BooleanBeanProperty
  var relocatable: Boolean = false

  def this(config: Configuration, receiver: Receiver) = {
    this()
    this.config = config
    this.receiver = receiver
    this.cco = new ComplexContentOutputter(receiver)
    try {
      cco.open()
      cco.startDocument(ReceiverOption.NONE)
    } catch {
      case err: XPathException => {
        err.printStackTrace()
        throw new InternalError(err.getMessage)
      }

    }
  }

  def this(config: Configuration, out: StreamResult, checksum: Boolean) = {
    this()
    init(config, out, checksum)
  }

  def this(config: Configuration, out: StreamResult) = this(config, out, false)

  def this(config: Configuration, out: Logger) = this(config, out.asStreamResult())

  def this(config: Configuration) = this(config, config.getLogger)


  def init(config: Configuration, out: StreamResult, checksum: Boolean): Unit = {
    val props: SerializationProperties = makeDefaultProperties(config)
    if (config.getXMLVersion == Configuration.XML11) {
      if ("JS" == getOptions.asInstanceOf[ExportOptions].target) {
        config.getLogger.warning(
          "For target=JS, the SEF file will use XML 1.0, which disallows control characters")
      } else {
        props.setProperty(OutputKeys.VERSION, "1.1")
      }
    }
    try {
      receiver = config.getSerializerFactory.getReceiver(out, props)
      receiver = new NamespaceReducer(receiver)
      if (checksum) {
        receiver = new CheckSumFilter(receiver)
      }
      cco = new ComplexContentOutputter(receiver)
    } catch {
      case err: XPathException => {
        err.printStackTrace()
        throw new InternalError(err.getMessage)
      }

    }
    this.config = config
    try {
      cco.open()
      cco.startDocument(ReceiverOption.NONE)
    } catch {
      case err: XPathException => {
        err.printStackTrace()
        throw new InternalError(err.getMessage)
      }

    }
  }

  def init(config: Configuration, out: Receiver, checksum: Boolean): Unit = {
    receiver = out
    receiver = new NamespaceReducer(receiver)
    if (checksum) {
      receiver = new CheckSumFilter(receiver)
    }
    cco = new ComplexContentOutputter(receiver)
    this.config = config
    try {
      cco.open()
      cco.startDocument(ReceiverOption.NONE)
    } catch {
      case err: XPathException => {
        err.printStackTrace()
        throw new InternalError(err.getMessage)
      }

    }
  }


  def setDefaultNamespace(namespace: String): Unit = {
    defaultNamespace = namespace
    namespaceMap = namespaceMap.put("", namespace)
  }

  def startElement(name: String, expr: Expression): Int = {
    val parent: Expression =
      if (expressionStack.isEmpty) null else expressionStack.peek()
    expressionStack.push(expr)
    nameStack.push("*" + name)
    val n: Int = _startElement(name)
    if (parent == null ||
      expr.getRetainedStaticContext != parent.getRetainedStaticContext) {
      if (expr.getRetainedStaticContext == null) {} else {
        emitRetainedStaticContext(
          expr.getRetainedStaticContext,
          if (parent == null) null else parent.getRetainedStaticContext)
      }
    }
    val mod: String = expr.getLocation.getSystemId
    if (mod != null && parent != null &&
      (parent.getLocation.getSystemId == null || parent.getLocation.getSystemId != mod)) {
      emitAttribute("module", truncatedModuleName(mod))
    }
    val lineNr: Int = expr.getLocation.getLineNumber
    if (parent == null ||
      (parent.getLocation.getLineNumber != lineNr && lineNr != -1)) {
      emitAttribute("line", lineNr + "")
    }
    n
  }

  private def truncatedModuleName(module: String): String =
    if (!relocatable) {
      module
    } else {
      val parts: Array[String] = module.split("/")
      var p: Int = parts.length - 1
      while (p >= 0) {
        if (!parts(p).isEmpty) {
          parts(p)
        }
        {
          p -= 1;
          p + 1
        }
      }
      module
    }

  def emitRetainedStaticContext(sc: RetainedStaticContext,
                                parentSC: RetainedStaticContext): Unit = {
    if (!options.asInstanceOf[ExportOptions].suppressStaticContext &&
      !relocatable &&
      sc.getStaticBaseUri != null &&
      (parentSC == null || sc.getStaticBaseUri != parentSC.getStaticBaseUri)) {
      emitAttribute("baseUri", sc.getStaticBaseUriString.asInstanceOf[String])
    }
    if (sc.getDefaultCollationName != NamespaceConstant.CODEPOINT_COLLATION_URI &&
      (parentSC == null ||
        sc.getDefaultCollationName != parentSC.getDefaultCollationName)) {
      emitAttribute("defaultCollation", sc.getDefaultCollationName.asInstanceOf[String])
    }
    if (!sc.getDefaultElementNamespace.asInstanceOf[String].isEmpty &&
      (parentSC == null ||
        sc.getDefaultElementNamespace != parentSC.getDefaultElementNamespace)) {
      emitAttribute("defaultElementNS", sc.getDefaultElementNamespace.asInstanceOf[String])
    }
    if (NamespaceConstant.FN != sc.getDefaultFunctionNamespace) {
      emitAttribute("defaultFunctionNS", sc.getDefaultFunctionNamespace)
    }
    if (!options.asInstanceOf[ExportOptions].suppressStaticContext &&
      (parentSC == null || !sc.declaresSameNamespaces(parentSC))) {
      val fsb: FastStringBuffer = new FastStringBuffer(FastStringBuffer.C256)
      var iter: Iterator[String] = sc.iteratePrefixes.asInstanceOf[Iterator[String]]
      while (iter.hasNext) {
        val p: String = iter.next()
        var uri: String = sc.getURIForPrefix(p, useDefault = true).asInstanceOf[String]
        fsb.append(p)
        fsb.append("=")
        if (Whitespace.containsWhitespace(uri)) {
          throw new XPathException(
            "Cannot export a stylesheet if namespaces contain whitespace: '" +
              uri +
              "'")
        }
        if (uri == NamespaceConstant.getUriForConventionalPrefix(p)) {
          uri = "~"
        }
        fsb.append(uri)
        fsb.append(" ")
      }
      emitAttribute("ns", Whitespace.trim(fsb))
    }
  }

  def startElement(name: String): Int = {
    nameStack.push(name)
    _startElement(name)
  }

  private def _startElement(name: String): Int = {
    try {
      if (inStartTag) {
        cco.startContent()
        inStartTag = false
      }
      var nodeName: NodeName = null
      nodeName =
        if (defaultNamespace == null) new NoNamespaceName(name)
        else new FingerprintedQName("", defaultNamespace, name)
      cco.startElement(nodeName,
        Untyped.getInstance,
        Loc.NONE,
        ReceiverOption.NONE)
      if (nextRole != null) {
        emitAttribute("role", nextRole)
        nextRole = null
      }
    } catch {
      case err: XPathException => {
        err.printStackTrace()
        throw new InternalError(err.getMessage)
      }

    }
    inStartTag = true
    depth += 1
    depth
  }

  def setChildRole(role: String): Unit = {
    nextRole = role
  }

  def emitAttribute(name: String, value: String): Unit = {
    var valueVar = value;
    if (valueVar != null) {
      if (name.==("module")) {
        valueVar = truncatedModuleName(valueVar)
      }
      try cco.attribute(new NoNamespaceName(name),
        BuiltInAtomicType.UNTYPED_ATOMIC,
        valueVar,
        Loc.NONE,
        ReceiverOption.NONE)
      catch {
        case err: XPathException => {
          err.printStackTrace()
          throw new InternalError(err.getMessage)
        }

      }
    }
  }

  def emitAttribute(name: String, value: StructuredQName): Unit = {
    val attVal: String = value.getEQName
    try cco.attribute(new NoNamespaceName(name),
      BuiltInAtomicType.UNTYPED_ATOMIC,
      attVal,
      Loc.NONE,
      ReceiverOption.NONE)
    catch {
      case err: XPathException => {
        err.printStackTrace()
        throw new InternalError(err.getMessage)
      }

    }
  }

  def namespace(prefix: String, uri: String): Unit = {
    try cco.namespace(prefix, uri, ReceiverOption.NONE)
    catch {
      case e: XPathException => {
        e.printStackTrace()
        throw new InternalError(e.getMessage)
      }

    }
  }

  def endElement(): Int = {
    try {
      if (inStartTag) {
        cco.startContent()
        inStartTag = false
      }
      cco.endElement()
    } catch {
      case err: XPathException => {
        err.printStackTrace()
        throw new InternalError(err.getMessage)
      }

    }
    val name: String = nameStack.pop()
    if (name.startsWith("*")) {
      expressionStack.pop()
    }
    depth -= 1
    depth
  }

  def startSubsidiaryElement(name: String): Unit = {
    startElement(name)
  }

  def endSubsidiaryElement(): Unit = {
    endElement()
  }

  def close(): Unit = {
    try {
      if (receiver.isInstanceOf[CheckSumFilter]) {
        val c: Int = receiver.asInstanceOf[CheckSumFilter].getChecksum
        cco.processingInstruction(CheckSumFilter.SIGMA,
          java.lang.Integer.toHexString(c),
          Loc.NONE,
          ReceiverOption.NONE)
      }
      cco.endDocument()
      cco.close()
    } catch {
      case err: XPathException => {
        err.printStackTrace()
        throw new InternalError(err.getMessage)
      }

    }
  }

  def getConfiguration(): Configuration = config

  def getNamePool(): NamePool = config.getNamePool

  def getTypeHierarchy(): TypeHierarchy = config.getTypeHierarchy

}
