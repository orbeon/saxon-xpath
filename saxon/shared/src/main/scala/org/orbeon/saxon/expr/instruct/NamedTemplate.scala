package org.orbeon.saxon.expr.instruct

import org.orbeon.saxon.event.Outputter

import org.orbeon.saxon.expr.Expression

import org.orbeon.saxon.expr.StaticProperty

import org.orbeon.saxon.expr.XPathContext

import org.orbeon.saxon.expr.parser.RoleDiagnostic

import org.orbeon.saxon.model.AnyItemType

import org.orbeon.saxon.model.ItemType

import org.orbeon.saxon.model.TypeHierarchy

import org.orbeon.saxon.om.Item

import org.orbeon.saxon.om.StandardNames

import org.orbeon.saxon.om.StructuredQName

import org.orbeon.saxon.trace.ExpressionPresenter

import org.orbeon.saxon.trace.TraceableComponent

import org.orbeon.saxon.trans.SymbolicName

import org.orbeon.saxon.trans.XPathException

import org.orbeon.saxon.value.SequenceType

import java.util.ArrayList

import java.util.List

import java.util.function.BiConsumer

import NamedTemplate._

import scala.beans.{BeanProperty, BooleanBeanProperty}

//import scala.collection.compat._
import scala.jdk.CollectionConverters._

object NamedTemplate {

  class LocalParamInfo {

    var name: StructuredQName = _

    var requiredType: SequenceType = _

    var isRequired: Boolean = _

    var isTunnel: Boolean = _

  }

}

class NamedTemplate(@BeanProperty var templateName: StructuredQName)
  extends Actor
    with TraceableComponent {

  var hasRequiredParams: Boolean = _

  private var bodyIsTailCallReturner: Boolean = _

  private var requiredType: SequenceType = _

  @BeanProperty
  var requiredContextItemType: ItemType = AnyItemType

  @BooleanBeanProperty
  var mayOmitContextItem: Boolean = true

  @BooleanBeanProperty
  var absentFocus: Boolean = false

  @BeanProperty
  var localParamDetails: List[LocalParamInfo] = new ArrayList(4)

  def setContextItemRequirements(`type`: ItemType,
                                 mayBeOmitted: Boolean,
                                 absentFocus: Boolean): Unit = {
    requiredContextItemType = `type`
    mayOmitContextItem = mayBeOmitted
    this.absentFocus = absentFocus
  }

  def getSymbolicName: SymbolicName =
    if (getTemplateName == null) {
      null
    } else {
      new SymbolicName(StandardNames.XSL_TEMPLATE, getTemplateName)
    }

  override def getTracingTag(): String = "xsl:template"

  override def gatherProperties(consumer: BiConsumer[String, Any]): Unit = {
    consumer.accept("name", getTemplateName)
  }

  override def setBody(body: Expression): Unit = {
    super.setBody(body)
    bodyIsTailCallReturner = (body.isInstanceOf[TailCallReturner])
  }

  def getObjectName: StructuredQName = templateName

  def setHasRequiredParams(has: Boolean): Unit = {
    hasRequiredParams = has
  }

  def setRequiredType(`type`: SequenceType): Unit = {
    requiredType = `type`
  }

  def getRequiredType: SequenceType =
    if (requiredType == null) {
      SequenceType.ANY_SEQUENCE
    } else {
      requiredType
    }

  def getLocalParamInfo(id: StructuredQName): LocalParamInfo = {
    val params: List[LocalParamInfo] = getLocalParamDetails
    for (lp <- params.asScala if lp.name == id) {
      return lp
    }
    null
  }

  def expand(output: Outputter, context: XPathContext): TailCall = {
    var contxt = context
    val contextItem: Item = context.getContextItem
    if (contextItem == null) {
      if (!mayOmitContextItem) {
        val err = new XPathException(
          "The template requires a context item, but none has been supplied",
          "XTTE3090")
        err.setLocation(getLocation)
        err.setIsTypeError(true)
        throw err
      }
    } else {
      val th = contxt.getConfiguration.getTypeHierarchy
      if (requiredContextItemType != AnyItemType && !requiredContextItemType
        .matches(contextItem, th)) {
        val role: RoleDiagnostic = new RoleDiagnostic(
          RoleDiagnostic.MISC,
          "context item for the named template",
          0)
        val message: String =
          role.composeErrorMessage(requiredContextItemType, contextItem, th)
        val err = new XPathException(message, "XTTE0590")
        err.setLocation(getLocation)
        err.setIsTypeError(true)
        throw err
      }
      if (absentFocus) {
        contxt = contxt.newMinorContext()
        contxt.setCurrentIterator(null)
      }
    }
    if (bodyIsTailCallReturner) {
      return body.asInstanceOf[TailCallReturner].processLeavingTail(output, contxt)
    } else if (body != null) {
      body.process(output, contxt)
    }
    null
  }

  def export(presenter: ExpressionPresenter): Unit = {
    presenter.startElement("template")
    presenter.emitAttribute("name", getTemplateName)
    explainProperties(presenter)
    presenter.emitAttribute("slots",
      "" + getStackFrameMap.getNumberOfVariables)
    if (getBody != null) {
      presenter.setChildRole("body")
      getBody.export(presenter)
    }
    presenter.endElement()
  }

  def explainProperties(presenter: ExpressionPresenter): Unit = {
    if (getRequiredContextItemType != AnyItemType) {
      val st: SequenceType = SequenceType.makeSequenceType(
        getRequiredContextItemType,
        StaticProperty.EXACTLY_ONE)
      presenter.emitAttribute("cxt", st.toAlphaCode)
    }
    var flags: String = ""
    if (mayOmitContextItem) {
      flags = "o"
    }
    if (!absentFocus) {
      flags += "s"
    }
    presenter.emitAttribute("flags", flags)
    if (getRequiredType != SequenceType.ANY_SEQUENCE) {
      presenter.emitAttribute("as", getRequiredType.toAlphaCode)
    }
    presenter.emitAttribute("line", getLineNumber.toString)
    presenter.emitAttribute("module", getSystemId)
  }

}
