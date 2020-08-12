package net.sf.saxon.expr.instruct

import net.sf.saxon.utils.Configuration

import net.sf.saxon.event.Outputter

import net.sf.saxon.event.ReceiverOption

import net.sf.saxon.expr._

import net.sf.saxon.expr.parser._

import net.sf.saxon.lib.NamespaceConstant

import net.sf.saxon.lib.StandardURIChecker

import net.sf.saxon.model.ItemType

import net.sf.saxon.om._

import net.sf.saxon.pattern.NodeKindTest

import net.sf.saxon.trace.ExpressionPresenter

import net.sf.saxon.trans.XPathException

import net.sf.saxon.value.AnyURIValue

import net.sf.saxon.value.AtomicValue

import net.sf.saxon.value.SequenceType

import net.sf.saxon.value.Whitespace

class NamespaceConstructor(name: Expression) extends SimpleNodeConstructor {

  private var nameOp: Operand =
    new Operand(this, name, OperandRole.SINGLE_ATOMIC)

  def getNameExp(): Expression = nameOp.getChildExpression

  def setNameExp(nameExp: Expression): Unit = {
    nameOp.setChildExpression(nameExp)
  }

  override def operands(): java.lang.Iterable[Operand] =
    operandList(selectOp, nameOp)

  override def getInstructionNameCode(): Int = StandardNames.XSL_NAMESPACE

  override def getItemType(): ItemType = NodeKindTest.NAMESPACE

  override def getCardinality(): Int = StaticProperty.EXACTLY_ONE

  def localTypeCheck(visitor: ExpressionVisitor,
                     contextItemType: ContextItemStaticInfo): Unit = {
    val env: StaticContext = visitor.getStaticContext
    nameOp.typeCheck(visitor, contextItemType)
    val role: RoleDiagnostic =
      new RoleDiagnostic(RoleDiagnostic.INSTRUCTION, "namespace/name", 0)
    this.setNameExp(env.getConfiguration
      .getTypeChecker(false)
      .staticTypeCheck(getNameExp, SequenceType.OPTIONAL_ATOMIC, role, visitor))
    adoptChildExpression(getNameExp)
    if (getNameExp.isInstanceOf[Literal]) {
      evaluatePrefix(env.makeEarlyEvaluationContext())
    }
  }

  def copy(rebindings: RebindingMap): Expression = {
    val exp: NamespaceConstructor = new NamespaceConstructor(
      getNameExp.copy(rebindings))
    exp.setSelect(getSelect.copy(rebindings))
    ExpressionTool.copyLocationInfo(this, exp)
    exp
  }

  override def evaluateNodeName(context: XPathContext): NodeName = {
    val prefix: String = evaluatePrefix(context)
    new NoNamespaceName(prefix)
  }

  private def evaluatePrefix(context: XPathContext): String = {
    val value: AtomicValue =
      getNameExp.evaluateItem(context).asInstanceOf[AtomicValue]
    if (value == null) {
      ""
    }
    if (!(value.isInstanceOf[net.sf.saxon.value.StringValue]) ||
      value.isInstanceOf[AnyURIValue]) {
      val err: XPathException = new XPathException(
        "Namespace prefix is not an xs:string or xs:untypedAtomic",
        "XPTY0004",
        getLocation)
      err.setIsTypeError(true)
      throw dynamicError(getLocation.toString, err.toString, context)
    }
    val prefix: String = Whitespace.trim(value.getStringValueCS)
    if (!(prefix.isEmpty || NameChecker.isValidNCName(prefix))) {
      val errorCode: String = if (isXSLT) "XTDE0920" else "XQDY0074"
      val err: XPathException = new XPathException(
        "Namespace prefix is invalid: " + prefix,
        errorCode,
        getLocation)
      throw dynamicError(getLocation.toString, err.toString, context)
    }
    if (prefix.==("xmlns")) {
      val errorCode: String = if (isXSLT) "XTDE0920" else "XQDY0101"
      val err: XPathException = new XPathException(
        "Namespace prefix 'xmlns' is not allowed",
        errorCode,
        getLocation)
      throw dynamicError(getLocation.toString, err.toString, context)
    }
    prefix
  }

  def processValue(value: CharSequence,
                   output: Outputter,
                   context: XPathContext): Unit = {
    val prefix: String = evaluatePrefix(context)
    val uri: String = value.toString
    checkPrefixAndUri(prefix, uri, context)
    output.namespace(prefix, uri, ReceiverOption.REJECT_DUPLICATES)
  }

  override def evaluateItem(context: XPathContext): NodeInfo = {
    val node: NodeInfo = super.evaluateItem(context).asInstanceOf[NodeInfo]
    assert(node != null)
    val prefix: String = node.getLocalPart
    val uri: String = node.getStringValue
    checkPrefixAndUri(prefix, uri, context)
    node
  }

  private def checkPrefixAndUri(prefix: String,
                                uri: String,
                                context: XPathContext): Unit = {
    if (prefix.==("xml") != uri == NamespaceConstant.XML) {
      val errorCode: String = if (isXSLT) "XTDE0925" else "XQDY0101"
      val err: XPathException = new XPathException(
        "Namespace prefix 'xml' and namespace uri " + NamespaceConstant.XML +
          " must only be used together",
        errorCode,
        getLocation)
      throw dynamicError(getLocation.toString, err.toString, context)
    }
    if (uri.isEmpty) {
      val errorCode: String = if (isXSLT) "XTDE0930" else "XQDY0101"
      val err: XPathException = new XPathException(
        "Namespace URI is an empty string",
        errorCode,
        getLocation)
      throw dynamicError(getLocation.toString, err.toString, context)
    }
    if (uri == NamespaceConstant.XMLNS) {
      val errorCode: String = if (isXSLT) "XTDE0905" else "XQDY0101"
      val err: XPathException = new XPathException(
        "A namespace node cannot have the reserved namespace " +
          NamespaceConstant.XMLNS,
        errorCode,
        getLocation)
      throw dynamicError(getLocation.toString, err.toString, context)
    }
    if (context.getConfiguration.getXsdVersion == Configuration.XSD10 &&
      !StandardURIChecker.getInstance.isValidURI(uri)) {
      val de: XPathException = new XPathException(
        "The string value of the constructed namespace node must be a valid URI",
        "XTDE0905",
        getLocation)
      throw dynamicError(getLocation.toString, de.toString, context)
    }
  }

  def export(out: ExpressionPresenter): Unit = {
    out.startElement("namespace", this)
    var flags: String = ""
    if (isLocal) {
      flags += "l"
    }
    if (!flags.isEmpty) {
      out.emitAttribute("flags", flags)
    }
    out.setChildRole("name")
    getNameExp.export(out)
    out.setChildRole("select")
    getSelect.export(out)
    out.endElement()
  }

}
