package net.sf.saxon.expr.instruct

import Instruction._
import net.sf.saxon.event.Outputter

import net.sf.saxon.expr._

import net.sf.saxon.expr.parser._

import net.sf.saxon.model.AnyItemType

import net.sf.saxon.model.ItemType

import net.sf.saxon.om.StandardNames

import net.sf.saxon.om.StructuredQName

import net.sf.saxon.trace.ExpressionPresenter

import net.sf.saxon.trans.SaxonErrorCode

import net.sf.saxon.trans.SymbolicName

import net.sf.saxon.trans.Visibility

import net.sf.saxon.trans.XPathException

import net.sf.saxon.tree.util.FastStringBuffer

import net.sf.saxon.value.SequenceType

import java.util.ArrayList

import java.util.Arrays

import CallTemplate._

import scala.beans.{BeanProperty, BooleanBeanProperty}


object CallTemplate {

  class CallTemplatePackage(private var targetComponent: Component,
                            private var params: ParameterSet,
                            private var tunnelParams: ParameterSet,
                            private var instruction: CallTemplate,
                            private var output: Outputter,
                            private var evaluationContext: XPathContext)
    extends TailCall {

    if (!(targetComponent.getActor.isInstanceOf[NamedTemplate])) {
      throw new ClassCastException(
        "Target of call-template must be a named template")
    }

    def processLeavingTail(): TailCall = {
      val template: NamedTemplate =
        targetComponent.getActor.asInstanceOf[NamedTemplate]
      val c2: XPathContextMajor = evaluationContext.newContext()
      c2.setCurrentComponent(targetComponent)
      c2.setOrigin(instruction)
      c2.setLocalParameters(params)
      c2.setTunnelParameters(tunnelParams)
      c2.openStackFrame(template.getStackFrameMap)
      c2.setCurrentMergeGroupIterator(null)
      template.expand(output, c2)
    }

  }

}

class CallTemplate(private var template: NamedTemplate,
                   @BeanProperty var calledTemplateName: StructuredQName,
                   private var useTailRecursion: Boolean,
                   inStreamable: Boolean)
  extends Instruction
    with ITemplateCall
    with ComponentInvocation {

  @BeanProperty
  var actualParams: Array[WithParam] = WithParam.EMPTY_ARRAY

  @BeanProperty
  var tunnelParams: Array[WithParam] = WithParam.EMPTY_ARRAY

  var bindingSlot = -1

  def setBindingSlot(slot: Int) = bindingSlot = slot

  def getBindingSlot: Int = bindingSlot

  private var isWithinDeclaredStreamableConstruct: Boolean = inStreamable

  def setActualParameters(actualParams: Array[WithParam],
                          tunnelParams: Array[WithParam]): Unit = {
    this.actualParams = actualParams
    this.tunnelParams = tunnelParams
    for (actualParam <- actualParams) {
      adoptChildExpression(actualParam.getSelectExpression)
    }
    for (tunnelParam <- tunnelParams) {
      adoptChildExpression(tunnelParam.getSelectExpression)
    }
  }

  def getSymbolicName(): SymbolicName =
    if (calledTemplateName == null) null
    else new SymbolicName(StandardNames.XSL_TEMPLATE, calledTemplateName)

  def getTarget(): Component = template.getDeclaringComponent

  def getFixedTarget(): Component = {
    val c: Component = getTarget
    val v: Visibility.Visibility = c.getVisibility
    if (v == Visibility.PRIVATE || v == Visibility.FINAL) {
      c
    } else {
      null
    }
  }

  def setTargetTemplate(target: NamedTemplate): Unit = {
    this.template = target
  }

  def getTargetTemplate(): NamedTemplate = template

  def usesTailRecursion(): Boolean = useTailRecursion

  override def getInstructionNameCode(): Int = StandardNames.XSL_CALL_TEMPLATE

  override def simplify(): Expression = {
    WithParam.simplify(actualParams)
    WithParam.simplify(tunnelParams)
    this
  }

  override def typeCheck(visitor: ExpressionVisitor,
                         contextInfo: ContextItemStaticInfo): Expression = {
    WithParam.typeCheck(actualParams, visitor, contextInfo)
    WithParam.typeCheck(tunnelParams, visitor, contextInfo)
    val backwards: Boolean =
      visitor.getStaticContext.isInBackwardsCompatibleMode
    val tc: TypeChecker = visitor.getConfiguration.getTypeChecker(backwards)
    for (p <- 0 until actualParams.length) {
      val wp: WithParam = actualParams(p)
      val lp: NamedTemplate.LocalParamInfo =
        template.getLocalParamInfo(wp.getVariableQName)
      if (lp != null) {
        val req: SequenceType = lp.requiredType
        val role: RoleDiagnostic = new RoleDiagnostic(
          RoleDiagnostic.PARAM,
          wp.getVariableQName.getDisplayName,
          p)
        role.setErrorCode("XTTE0590")
        val select: Expression =
          tc.staticTypeCheck(wp.getSelectExpression, req, role, visitor)
        wp.setSelectExpression(this, select)
        wp.setTypeChecked(true)
      }
    }
    this
  }

  override def optimize(visitor: ExpressionVisitor,
                        contextItemType: ContextItemStaticInfo): Expression = {
    WithParam.optimize(visitor, actualParams, contextItemType)
    WithParam.optimize(visitor, tunnelParams, contextItemType)
    this
  }

  override def computeCardinality(): Int =
    if (template == null) {
      StaticProperty.ALLOWS_ZERO_OR_MORE
    } else {
      template.getRequiredType.getCardinality
    }

  override def getItemType(): ItemType =
    if (template == null) {
      AnyItemType.getInstance
    } else {
      template.getRequiredType.getPrimaryType
    }

  def copy(rebindings: RebindingMap): Expression = {
    val ct: CallTemplate = new CallTemplate(
      template,
      calledTemplateName,
      useTailRecursion,
      isWithinDeclaredStreamableConstruct)
    ExpressionTool.copyLocationInfo(this, ct)
    ct.actualParams = WithParam.copy(ct, actualParams, rebindings)
    ct.tunnelParams = WithParam.copy(ct, tunnelParams, rebindings)
    ct
  }

  override def getIntrinsicDependencies(): Int =
    StaticProperty.DEPENDS_ON_XSLT_CONTEXT | StaticProperty.DEPENDS_ON_FOCUS

  override def mayCreateNewNodes(): Boolean = true

  override def operands(): java.lang.Iterable[Operand] = {
    val list: ArrayList[Operand] = new ArrayList[Operand](10)
    WithParam.gatherOperands(this, actualParams, list)
    WithParam.gatherOperands(this, tunnelParams, list)
    list
  }

  override def process(output: Outputter, context: XPathContext): Unit = {
    var t: NamedTemplate = null
    var target: Component = getFixedTarget
    if (bindingSlot >= 0) {
      target = context.getTargetComponent(bindingSlot)
      if (target.isHiddenAbstractComponent) {
        val err: XPathException = new XPathException(
          "Cannot call an abstract template (" + calledTemplateName.getDisplayName +
            ") with no implementation",
          "XTDE3052")
        err.setLocation(getLocation)
        throw err
      }
    }
    t = target.getActor.asInstanceOf[NamedTemplate]
    val c2: XPathContextMajor = context.newContext()
    c2.setCurrentComponent(target)
    c2.setOrigin(this)
    c2.openStackFrame(t.getStackFrameMap)
    c2.setLocalParameters(assembleParams(context, actualParams))
    c2.setTunnelParameters(assembleTunnelParams(context, tunnelParams))
    if (isWithinDeclaredStreamableConstruct) {
      c2.setCurrentGroupIterator(null)
    }
    c2.setCurrentMergeGroupIterator(null)
    try {
      var tc: TailCall = t.expand(output, c2)
      while (tc != null) tc = tc.processLeavingTail()
    } catch {
      case e: StackOverflowError => {
        val err: XPathException = new XPathException.StackOverflow(
          "Too many nested template or function calls. The stylesheet may be looping.",
          SaxonErrorCode.SXLM0001,
          getLocation)
        err.setXPathContext(context)
        throw err
      }

    }
  }

  def processLeavingTail(output: Outputter, context: XPathContext): TailCall =
    if (useTailRecursion) {
      var targetComponent: Component = null
      targetComponent =
        if (bindingSlot >= 0) context.getTargetComponent(bindingSlot)
        else getFixedTarget
      if (targetComponent == null) {
        throw new XPathException(
          "Internal Saxon error: No binding available for call-template instruction",
          SaxonErrorCode.SXPK0001,
          this.getLocation)
      }
      if (targetComponent.isHiddenAbstractComponent) {
        throw new XPathException(
          "Cannot call an abstract template (" + calledTemplateName.getDisplayName +
            ") with no implementation",
          "XTDE3052",
          this.getLocation)
      }
      var params: ParameterSet = assembleParams(context, actualParams)
      val tunnels: ParameterSet = assembleTunnelParams(context, tunnelParams)
      if (params == null) {
        params = ParameterSet.EMPTY_PARAMETER_SET
      }
      Arrays.fill(context.getStackFrame.getStackFrameValues.asInstanceOf[Array[Object]], null)
      new CallTemplatePackage(targetComponent,
        params,
        tunnels,
        this,
        output,
        context)
    } else {
      process(output, context)
      null
    }

  override def getObjectName(): StructuredQName =
    if (template == null) null else template.getTemplateName

  def export(out: ExpressionPresenter): Unit = {
    out.startElement("callT", this)
    var flags: String = ""
    if (template != null && template.getTemplateName != null) {
      out.emitAttribute("name", template.getTemplateName)
    }
    out.emitAttribute("bSlot", "" + getBindingSlot)
    if (isWithinDeclaredStreamableConstruct) {
      flags += "d"
    }
    if (useTailRecursion) {
      flags += "t"
    }
    if (!flags.isEmpty) {
      out.emitAttribute("flags", flags)
    }
    if (actualParams.length > 0) {
      WithParam.exportParameters(actualParams, out, tunnel = false)
    }
    if (tunnelParams.length > 0) {
      WithParam.exportParameters(tunnelParams, out, tunnel = true)
    }
    out.endElement()
  }

  override def toString(): String = {
    val buff: FastStringBuffer = new FastStringBuffer(FastStringBuffer.C64)
    buff.append("CallTemplate#")
    if (template.getObjectName != null) {
      buff.append(template.getObjectName.getDisplayName)
    }
    var first: Boolean = true
    for (p <- getActualParams) {
      buff.append(if (first) "(" else ", ")
      buff.append(p.getVariableQName.getDisplayName)
      buff.append("=")
      buff.append(p.getSelectExpression.toString)
      first = false
    }
    if (!first) {
      buff.append(")")
    }
    buff.toString
  }

  override def toShortString(): String = {
    val buff: FastStringBuffer = new FastStringBuffer(FastStringBuffer.C64)
    buff.append("CallTemplate#")
    buff.append(template.getObjectName.getDisplayName)
    buff.toString
  }

  override def getStreamerName(): String = "CallTemplate"

}
