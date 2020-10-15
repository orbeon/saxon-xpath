package org.orbeon.saxon.expr.instruct

import org.orbeon.saxon.expr.{Component, ContextOriginator, XPathContext}
import org.orbeon.saxon.functions.AbstractFunction
import org.orbeon.saxon.model.FunctionItemType
import org.orbeon.saxon.om.{Function, Sequence, StructuredQName}
import org.orbeon.saxon.trace.ExpressionPresenter

import scala.beans.BeanProperty


/**
 * This class represents a function invoked using xsl:original from within an xs:override element.
 */
class OriginalFunction(@BeanProperty var component: Component)
  extends AbstractFunction
    with Function
    with ContextOriginator {

  private val userFunction =
    component.getActor.asInstanceOf[UserFunction]

  def call(context: XPathContext, args: Array[Sequence]): Sequence = {
    val c2 = userFunction.makeNewContext(context, this)
    c2.setCurrentComponent(component)
    userFunction.call(c2, args)
  }

  def getFunctionItemType: FunctionItemType = userFunction.getFunctionItemType
  def getFunctionName    : StructuredQName  = userFunction.getFunctionName
  def getArity           : Int              = userFunction.getArity
  def getDescription     : String           = userFunction.getDescription

  def getContainingPackageName: String =
    component.getContainingPackage.getPackageName

  override def export(out: ExpressionPresenter): Unit = {
    val options = out.getOptions.asInstanceOf[ExpressionPresenter.ExportOptions]

    out.startElement("origF")
    out.emitAttribute("name", getFunctionName)
    out.emitAttribute("arity", "" + getArity)
    out.emitAttribute("pack", options.componentMap.get(component.getContainingPackage).toString)
    out.endElement()
  }
}
