package org.orbeon.saxon.expr.instruct

import org.orbeon.saxon.utils.Configuration

import org.orbeon.saxon.event.ReceiverOption

import scala.util.control.Breaks._

import org.orbeon.saxon.expr._

import org.orbeon.saxon.expr.parser.ContextItemStaticInfo

import org.orbeon.saxon.expr.parser.ExpressionTool

import org.orbeon.saxon.expr.parser.ExpressionVisitor

import org.orbeon.saxon.expr.parser.RebindingMap

import org.orbeon.saxon.functions.NormalizeSpace_1

import org.orbeon.saxon.functions.SystemFunction

import org.orbeon.saxon.lib.ConversionRules

import org.orbeon.saxon.lib.Validation

import org.orbeon.saxon.model._

import org.orbeon.saxon.om.NodeInfo

import org.orbeon.saxon.om.NodeName

import org.orbeon.saxon.om.StandardNames

import org.orbeon.saxon.trace.ExpressionPresenter

import org.orbeon.saxon.trans.Err

import org.orbeon.saxon.trans.XPathException

import org.orbeon.saxon.tree.util.Orphan

import java.util.function.BiConsumer

class FixedAttribute(private var nodeName: NodeName,
                     validationAction: Int,
                     schemaType: SimpleType)
  extends AttributeCreator {

  var simpleSchema = schemaType
  var valAct = validationAction
  simpleSchema = schemaType
  valAct = validationAction

  this.options = ReceiverOption.NONE

  override def getInstructionNameCode: Int = StandardNames.XSL_ATTRIBUTE

  override def getExpressionName: String = "att"

  def getAttributeName: NodeName = nodeName

  override def gatherProperties(consumer: BiConsumer[String, Any]): Unit =
    consumer.accept("name", getAttributeName)

  def localTypeCheck(visitor: ExpressionVisitor,
                     contextItemType: ContextItemStaticInfo): Unit = {
    if (nodeName == StandardNames.XML_ID_NAME && !getSelect.isCallOn(
      classOf[NormalizeSpace_1])) {
      val select: Expression = SystemFunction.makeCall(
        "normalize-space",
        getRetainedStaticContext,
        getSelect)
      this.setSelect(select)
    }
    val config: Configuration = visitor.getConfiguration
    val rules: ConversionRules = config.getConversionRules
    var schemaType: SimpleType = getSchemaType
    var errorCode: String = "XTTE1540"
    if (schemaType == null) {
      val validation = getValidationAction
      if (validation == Validation.STRICT) {
        val decl =
          config.getAttributeDeclaration(nodeName.getStructuredQName)
        if (decl == null) {
          val se = new XPathException(
            "Strict validation fails: there is no global attribute declaration for " +
              nodeName.getDisplayName)
          se.setErrorCode("XTTE1510")
          se.setLocation(getLocation)
          throw se
        }
        schemaType = decl.getType.asInstanceOf[SimpleType]
        errorCode = "XTTE1510"
      } else if (validation == Validation.LAX) {
        val decl = config.getAttributeDeclaration(nodeName.getStructuredQName)
        if (decl != null) {
          schemaType = decl.getType.asInstanceOf[SimpleType]
          errorCode = "XTTE1515"
        } else {
          visitor.getStaticContext.issueWarning(
            "Lax validation has no effect: there is no global attribute declaration for " +
              nodeName.getDisplayName,
            getLocation)
        }
      }
    }
    if (Literal.isAtomic(getSelect) && schemaType != null && !schemaType.isNamespaceSensitive) {
      val value = getSelect.asInstanceOf[Literal].value.getStringValueCS
      val err = schemaType.validateContent(
        value,
        DummyNamespaceResolver.getInstance,
        rules)
      if (err != null) {
        val se = new XPathException("Attribute value " + Err
          .wrap(value, Err.VALUE) + " does not the match the required type " +
          schemaType.getDescription +
          ". " +
          err.getMessage)
        se.setErrorCode(errorCode)
        throw se
      }
    }
    getSelect match {
      case literal: StringLiteral =>
        var special: Boolean = false
        val `val` = literal.getStringValue
        breakable {
          for (k <- 0 until `val`.length) {
            val c = `val`.charAt(k)
            if (c.toInt < 33 || c.toInt > 126 || c == '<' || c == '>' ||
              c == '&' ||
              c == '\"' ||
              c == '\'') {
              special = true
              break()
            }
          }
        }
        if (! special) {
          setNoSpecialChars()
        }
      case _ =>
    }
  }

  def getAttributeFingerprint: Int = nodeName.getFingerprint

  override def getCardinality: Int = StaticProperty.EXACTLY_ONE

  def copy(rebindings: RebindingMap): Expression = {
    val exp = new FixedAttribute(nodeName, getValidationAction, getSchemaType)
    ExpressionTool.copyLocationInfo(this, exp)
    exp.setSelect(getSelect.copy(rebindings))
    exp.setInstruction(isInstruction)
    exp
  }

  override def evaluateNodeName(context: XPathContext): NodeName = nodeName

  override def checkPermittedContents(parentType: SchemaType, whole: Boolean): Unit = {
    val fp = nodeName.getFingerprint
    if (fp == StandardNames.XSI_TYPE || fp == StandardNames.XSI_SCHEMA_LOCATION ||
      fp == StandardNames.XSI_NIL ||
      fp == StandardNames.XSI_NO_NAMESPACE_SCHEMA_LOCATION) {
      return
    }
    if (parentType.isInstanceOf[SimpleType]) {
      val err = new XPathException(
        "Attribute " + nodeName.getDisplayName +
          " is not permitted in the content model of the simple type " +
          parentType.getDescription)
      err.setIsTypeError(true)
      err.setLocation(getLocation)
      err.setErrorCode(if (getPackageData.isXSLT) "XTTE1510" else "XQDY0027")
      throw err
    }
    val `type`: SchemaType =
      parentType
        .asInstanceOf[ComplexType]
        .getAttributeUseType(nodeName.getStructuredQName)
    if (`type` == null) {
      val err = new XPathException(
        "Attribute " + nodeName.getDisplayName +
          " is not permitted in the content model of the complex type " +
          parentType.getDescription)
      err.setIsTypeError(true)
      err.setLocation(getLocation)
      err.setErrorCode(if (getPackageData.isXSLT) "XTTE1510" else "XQDY0027")
      throw err
    }
    try getSelect.checkPermittedContents(`type`, whole = true)
    catch {
      case e: XPathException =>
        e.maybeSetLocation(getLocation)
        throw e
    }
  }

  override def evaluateItem(context: XPathContext): NodeInfo = {
    val o = super.evaluateItem(context).asInstanceOf[Orphan]
    assert(o ne null)
    validateOrphanAttribute(o, context)
    o
  }

  def export(out: ExpressionPresenter): Unit = {
    out.startElement("att", this)
    out.emitAttribute("name", nodeName.getDisplayName)
    if (! nodeName.getStructuredQName.hasURI(""))
      out.emitAttribute("nsuri", nodeName.getStructuredQName.getURI)
    if (getValidationAction != Validation.SKIP && getValidationAction != Validation.BY_TYPE)
      out.emitAttribute("validation", Validation.toString(getValidationAction))
    if (getSchemaType != null)
      out.emitAttribute("type", getSchemaType.getStructuredQName)
    var flags = ""
    if (isLocal)
      flags += "l"
    if (! flags.isEmpty)
      out.emitAttribute("flags", flags)
    getSelect.export(out)
    out.endElement()
  }

  override def toShortString: String =
    "attr{" + nodeName.getDisplayName + "=...}"
}
