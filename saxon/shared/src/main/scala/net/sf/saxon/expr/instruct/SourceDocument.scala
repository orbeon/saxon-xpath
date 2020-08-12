package net.sf.saxon.expr.instruct

import net.sf.saxon.utils.Controller

import net.sf.saxon.event.Outputter

import net.sf.saxon.expr._

import net.sf.saxon.expr.accum.Accumulator

import net.sf.saxon.expr.parser._

import net.sf.saxon.functions.DocumentFn

import net.sf.saxon.lib.ParseOptions

import net.sf.saxon.lib.Validation

import net.sf.saxon.model.SchemaType

import net.sf.saxon.om.NodeInfo

import net.sf.saxon.om.SpaceStrippingRule

import net.sf.saxon.pattern.NodeKindTest

import net.sf.saxon.style.StylesheetPackage

import net.sf.saxon.trace.ExpressionPresenter

import net.sf.saxon.trans.QuitParsingException

import net.sf.saxon.trans.SaxonErrorCode

import net.sf.saxon.trans.XPathException

import net.sf.saxon.trans.XsltController

import net.sf.saxon.tree.iter.ManualIterator

import net.sf.saxon.tree.util.FastStringBuffer

import net.sf.saxon.value.SequenceType

import java.util.Set

import scala.jdk.CollectionConverters._

class SourceDocument(hrefExp: Expression,
                     body: Expression,
                     options: ParseOptions)
  extends Instruction {

   var hrefOp: Operand =
    new Operand(this, hrefExp, OperandRole.SINGLE_ATOMIC)

   var bodyOp: Operand = new Operand(
    this,
    body,
    new OperandRole(OperandRole.HAS_SPECIAL_FOCUS_RULES,
      OperandUsage.TRANSMISSION))

   var parseOptions: ParseOptions = options

   var accumulators: Set[_ <: Accumulator] =
    options.getApplicableAccumulators

  override def getExpressionName(): String = "xsl:source-document"

  def getExportTag(): String = "sourceDoc"

  def getHref(): Expression = hrefOp.getChildExpression

  def setHref(href: Expression): Unit = {
    hrefOp.setChildExpression(href)
  }

  def getBody(): Expression = bodyOp.getChildExpression

  def setBody(body: Expression): Unit = {
    bodyOp.setChildExpression(body)
  }

  def setUsedAccumulators(used: Set[_ <: Accumulator]): Unit = {
    accumulators = used
  }

  override def operands(): java.lang.Iterable[Operand] =
    operandList(hrefOp, bodyOp)

  override def allowExtractingCommonSubexpressions(): Boolean = false

  override def typeCheck(visitor: ExpressionVisitor,
                         contextInfo: ContextItemStaticInfo): Expression = {
    hrefOp.typeCheck(visitor, contextInfo)
    val role: RoleDiagnostic =
      new RoleDiagnostic(RoleDiagnostic.INSTRUCTION, "xsl:stream/href", 0)
    val tc: TypeChecker = visitor.getConfiguration.getTypeChecker(false)
    hrefOp.setChildExpression(
      tc.staticTypeCheck(hrefOp.getChildExpression,
        SequenceType.SINGLE_STRING,
        role,
        visitor))
    val newType: ContextItemStaticInfo =
      getConfiguration.makeContextItemStaticInfo(NodeKindTest.DOCUMENT, false)
    newType.setContextPostureStriding()
    bodyOp.typeCheck(visitor, newType)
    this
  }

  override def optimize(visitor: ExpressionVisitor,
                        contextItemType: ContextItemStaticInfo): Expression = {
    val newType: ContextItemStaticInfo =
      getConfiguration.makeContextItemStaticInfo(NodeKindTest.DOCUMENT, false)
    newType.setContextPostureStriding()
    hrefOp.optimize(visitor, contextItemType)
    bodyOp.optimize(visitor, newType)
    this
  }

  override def mayCreateNewNodes(): Boolean =
    !getBody.hasSpecialProperty(StaticProperty.NO_NODES_NEWLY_CREATED)

 override def computeDependencies(): Int = {
    var dependencies: Int = 0
    dependencies |= getHref.getDependencies
    dependencies |= getBody.getDependencies & ~StaticProperty.DEPENDS_ON_FOCUS
    dependencies
  }

  override def computeSpecialProperties(): Int = {
    val body: Expression = getBody
    if ((body.getSpecialProperties & StaticProperty.ALL_NODES_NEWLY_CREATED) !=
      0) {
      StaticProperty.ORDERED_NODESET | StaticProperty.PEER_NODESET
    }
    super.computeSpecialProperties()
  }

  override def export(out: ExpressionPresenter): Unit = {
    val options: ExpressionPresenter.ExportOptions =
      out.getOptions.asInstanceOf[ExpressionPresenter.ExportOptions]
    if ("JS" == options.target && options.targetVersion == 1) {
      throw new XPathException(
        "xsl:source-document is not supported in Saxon-JS 1.*",
        SaxonErrorCode.SXJS0001)
    }
    out.startElement(getExportTag, this)
    val validation: Int = parseOptions.getSchemaValidationMode
    if (validation != Validation.SKIP && validation != Validation.BY_TYPE) {
      out.emitAttribute("validation", validation + "")
    }
    val schemaType: SchemaType = parseOptions.getTopLevelType
    if (schemaType != null) {
      out.emitAttribute("schemaType", schemaType.getStructuredQName)
    }
    val xsltStripSpace: SpaceStrippingRule =
      if (getPackageData.isInstanceOf[StylesheetPackage])
        getPackageData.asInstanceOf[StylesheetPackage].getSpaceStrippingRule
      else null
    var flags: String = ""
    if (parseOptions.getSpaceStrippingRule == xsltStripSpace) {
      flags += "s"
    }
    if (parseOptions.isLineNumbering) {
      flags += "l"
    }
    if (parseOptions.isExpandAttributeDefaults) {
      flags += "a"
    }
    if (parseOptions.getDTDValidationMode == Validation.STRICT) {
      flags += "d"
    }
    if (parseOptions.isXIncludeAware) {
      flags += "i"
    }
    out.emitAttribute("flags", flags)
    if (accumulators != null && !accumulators.isEmpty) {
      val fsb: FastStringBuffer = new FastStringBuffer(256)
      for (acc <- accumulators.asScala) {
        if (!fsb.isEmpty) {
          fsb.append(" ")
        }
        fsb.append(acc.getAccumulatorName.getEQName)
      }
      out.emitAttribute("accum", fsb.toString)
    }
    out.setChildRole("href")
    getHref.export(out)
    out.setChildRole("body")
    getBody.export(out)
    out.endElement()
  }

  override def copy(rebindings: RebindingMap): Expression = {
    val exp: SourceDocument = new SourceDocument(getHref.copy(rebindings),
      getBody.copy(rebindings),
      parseOptions)
    exp.setRetainedStaticContext(getRetainedStaticContext)
    ExpressionTool.copyLocationInfo(this, exp)
    exp
  }

  override def processLeavingTail(output: Outputter,
                                  context: XPathContext): TailCall = {
    try push(output, context)
    catch {
      case q: QuitParsingException => {}

      case e: XPathException => {
        e.maybeSetLocation(getLocation)
        if (e.getErrorCodeQName == null) {
          e.setErrorCode("FODC0002")
        }
        throw e
      }

    }
    null
  }

  def push(output: Outputter, context: XPathContext): Unit = {
    val href: String =
      hrefOp.getChildExpression.evaluateAsString(context).toString
    val doc: NodeInfo = DocumentFn.makeDoc(href,
      getStaticBaseURIString,
      getPackageData,
      parseOptions,
      context,
      getLocation,
      false)
    val controller: Controller = context.getController
    if (accumulators != null && controller.isInstanceOf[XsltController]) {
      controller
        .asInstanceOf[XsltController]
        .getAccumulatorManager
        .setApplicableAccumulators(doc.getTreeInfo, accumulators)
    }
    val c2: XPathContext = context.newMinorContext()
    c2.setCurrentIterator(new ManualIterator(doc))
    bodyOp.getChildExpression.process(output, c2)
  }

}
