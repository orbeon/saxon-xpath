package net.sf.saxon.expr.instruct


import scala.beans.{BeanProperty, BooleanBeanProperty}
import net.sf.saxon.event.Outputter
import net.sf.saxon.expr.StaticProperty
import net.sf.saxon.expr.XPathContext
import net.sf.saxon.om.StandardNames
import net.sf.saxon.om.StructuredQName
import net.sf.saxon.trace.ExpressionPresenter
import net.sf.saxon.trans.SymbolicName
import net.sf.saxon.trans.XPathException
import net.sf.saxon.trans.XsltController
import java.util.Stack

import scala.beans.{BeanProperty, BooleanBeanProperty}

class AttributeSet extends Actor {

  var attributeSetName: StructuredQName = _

  @BooleanBeanProperty
  var declaredStreamable: Boolean = _

  override def getSymbolicName(): SymbolicName =
    new SymbolicName(StandardNames.XSL_ATTRIBUTE_SET, attributeSetName)

  def setName(attributeSetName: StructuredQName): Unit = {
    this.attributeSetName = attributeSetName
  }

 override def setStackFrameMap(stackFrameMap: SlotManager): Unit = {
    if (stackFrameMap != null) {
     super.setStackFrameMap(stackFrameMap)
    }
  }

  def getFocusDependencies(): Int =
    body.getDependencies & StaticProperty.DEPENDS_ON_FOCUS

  def expand(output: Outputter, context: XPathContext): Unit = {
    val stack: Stack[AttributeSet] = context.getController
      .asInstanceOf[XsltController]
      .getAttributeSetEvaluationStack
    if (stack.contains(this)) {
      throw new XPathException(
        "Attribute set " + getObjectName.getEQName + " invokes itself recursively",
        "XTDE0640")
    }
    stack.push(this)
    getBody.process(output, context)
    stack.pop()
    if (stack.isEmpty) {
      context.getController
        .asInstanceOf[XsltController]
        .releaseAttributeSetEvaluationStack()
    }
  }

  def getObjectName(): StructuredQName = attributeSetName

  override def export(presenter: ExpressionPresenter): Unit = {
    presenter.startElement("attributeSet")
    presenter.emitAttribute("name", getObjectName)
    presenter.emitAttribute("line", getLineNumber + "")
    presenter.emitAttribute("module", getSystemId)
    presenter.emitAttribute("slots",
      getStackFrameMap.getNumberOfVariables + "")
    presenter.emitAttribute(
      "binds",
      getDeclaringComponent.getComponentBindings.size + "")
    if (isDeclaredStreamable) {
      presenter.emitAttribute("flags", "s")
    }
    getBody.export(presenter)
    presenter.endElement()
  }

}
