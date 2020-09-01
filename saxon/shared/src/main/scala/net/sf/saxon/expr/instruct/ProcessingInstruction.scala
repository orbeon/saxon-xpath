package net.sf.saxon.expr.instruct

import net.sf.saxon.event.Outputter

import net.sf.saxon.event.ReceiverOption

import net.sf.saxon.expr._

import net.sf.saxon.expr.parser._

import net.sf.saxon.model.ItemType

import net.sf.saxon.om.NameChecker

import net.sf.saxon.om.NoNamespaceName

import net.sf.saxon.om.NodeName

import net.sf.saxon.om.StandardNames

import net.sf.saxon.pattern.NodeKindTest

import net.sf.saxon.trace.ExpressionPresenter

import net.sf.saxon.trans.Err

import net.sf.saxon.trans.XPathException

import net.sf.saxon.value._

import ProcessingInstruction._

object ProcessingInstruction {

  def checkContentXSLT(data: String): String = {
    var hh: Int = 0
    var strData = data
    while ((hh = strData.indexOf("?>")) .asInstanceOf[Int] >= 0) strData = strData.substring(0, hh + 1) + ' ' + strData
      .substring(hh + 1)
    Whitespace.removeLeadingWhitespace(strData).toString
  }

  def checkContentXQuery(data: String): String = {
    if (data.contains("?>")) {
      throw new XPathException(
        "Invalid characters (?>) in processing instruction",
        "XQDY0026")
    }
    Whitespace.removeLeadingWhitespace(data).toString
  }

}

class ProcessingInstruction(name: Expression) extends SimpleNodeConstructor {

  private var nameOp: Operand =
    new Operand(this, name, OperandRole.SINGLE_ATOMIC)

  def getNameExp: Expression = nameOp.getChildExpression

  def setNameExp(nameExp: Expression): Unit = {
    nameOp.setChildExpression(nameExp)
  }

  override def operands: java.lang.Iterable[Operand] =
    operandList(selectOp, nameOp)

 override def getInstructionNameCode(): Int = StandardNames.XSL_PROCESSING_INSTRUCTION

 override def getItemType: ItemType = NodeKindTest.PROCESSING_INSTRUCTION

 override def getCardinality(): Int = StaticProperty.EXACTLY_ONE

  def copy(rebindings: RebindingMap): Expression = {
    val exp: ProcessingInstruction = new ProcessingInstruction(
      getNameExp.copy(rebindings))
    ExpressionTool.copyLocationInfo(this, exp)
    exp.setSelect(getSelect.copy(rebindings))
    exp
  }

  def localTypeCheck(visitor: ExpressionVisitor,
                     contextItemType: ContextItemStaticInfo): Unit = {
    val env: StaticContext = visitor.getStaticContext
    nameOp.typeCheck(visitor, contextItemType)
    val role: RoleDiagnostic = new RoleDiagnostic(
      RoleDiagnostic.INSTRUCTION,
      "processing-instruction/name",
      0)
    this.setNameExp(visitor.getConfiguration
      .getTypeChecker(false)
      .staticTypeCheck(getNameExp, SequenceType.SINGLE_ATOMIC, role, visitor))
    val nameExp: Expression = getNameExp
    adoptChildExpression(nameExp)
    if (nameExp.isInstanceOf[Literal] &&
      nameExp.asInstanceOf[Literal].value.isInstanceOf[AtomicValue]) {
      val `val`: AtomicValue =
        nameExp.asInstanceOf[Literal].value.asInstanceOf[AtomicValue]
      checkName(`val`, env.makeEarlyEvaluationContext())
    }
    if (getSelect.isInstanceOf[Literal]) {
      val s: String = getSelect.asInstanceOf[Literal].value.getStringValue
      val s2: String = checkContent(s, env.makeEarlyEvaluationContext())
      if (s2 != s) {
        this.setSelect(new StringLiteral(s2))
      }
    }
  }

  override def getDependencies(): Int =
    getNameExp.getDependencies | super.getDependencies

  def processValue(value: CharSequence,
                   output: Outputter,
                   context: XPathContext): Unit = {
    val expandedName: String = evaluateName(context)
    if (expandedName != null) {
      val data: String = checkContent(value.toString, context)
      output.processingInstruction(expandedName,
        data,
        getLocation,
        ReceiverOption.NONE)
    }
  }

  override  def checkContent(data: String, context: XPathContext): String =
    if (isXSLT) {
      checkContentXSLT(data)
    } else {
      try checkContentXQuery(data)
      catch {
        case err: XPathException => {
          err.setXPathContext(context)
          err.setLocation(getLocation)
          throw err
        }

      }
    }

  override def evaluateNodeName(context: XPathContext): NodeName = {
    val expandedName: String = evaluateName(context)
    new NoNamespaceName(expandedName)
  }

  private def evaluateName(context: XPathContext): String = {
    val av: AtomicValue =
      getNameExp.evaluateItem(context).asInstanceOf[AtomicValue]
    if (av.isInstanceOf[StringValue] && !(av.isInstanceOf[AnyURIValue])) {
      checkName(av, context)
    } else {
      val e: XPathException = new XPathException(
        "Processing instruction name is not a string")
      e.setXPathContext(context)
      e.setErrorCode("XPTY0004")
      throw dynamicError(getLocation.toString, e.toString, context)
    }
  }

  private def checkName(name: AtomicValue, context: XPathContext): String =
    if (name.isInstanceOf[StringValue] && !(name.isInstanceOf[AnyURIValue])) {
      val expandedName: String = Whitespace.trim(name.getStringValue)
      if (!NameChecker.isValidNCName(expandedName)) {
        val e: XPathException = new XPathException(
          "Processing instruction name " + Err.wrap(expandedName) +
            " is not a valid NCName")
        e.setXPathContext(context)
        e.setErrorCode(if (isXSLT) "XTDE0890" else "XQDY0041")
        throw dynamicError(getLocation.toString, e.toString, context)
      }
      if (expandedName.equalsIgnoreCase("xml")) {
        val e: XPathException = new XPathException(
          "Processing instructions cannot be named 'xml' in any combination of upper/lower case")
        e.setXPathContext(context)
        e.setErrorCode(if (isXSLT) "XTDE0890" else "XQDY0064")
        throw dynamicError(getLocation.toString, e.toString, context)
      }
      expandedName
    } else {
      val e: XPathException = new XPathException(
        "Processing instruction name " + Err.wrap(name.getStringValue) +
          " is not of type xs:string or xs:untypedAtomic")
      e.setXPathContext(context)
      e.setErrorCode("XPTY0004")
      e.setIsTypeError(true)
      throw dynamicError(getLocation.toString, e.toString, context)
    }

  def export(out: ExpressionPresenter): Unit = {
    out.startElement("procInst", this)
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
