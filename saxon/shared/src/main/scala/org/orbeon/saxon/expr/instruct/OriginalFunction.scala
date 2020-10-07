package org.orbeon.saxon.expr.instruct

import org.orbeon.saxon.expr.Component

import org.orbeon.saxon.expr.ContextOriginator

import org.orbeon.saxon.expr.XPathContext

import org.orbeon.saxon.expr.XPathContextMajor

import org.orbeon.saxon.functions.AbstractFunction

import org.orbeon.saxon.model.FunctionItemType

import org.orbeon.saxon.om.Function

import org.orbeon.saxon.om.Sequence

import org.orbeon.saxon.om.StructuredQName

import org.orbeon.saxon.trace.ExpressionPresenter

import org.orbeon.saxon.trans.XPathException

import scala.beans.{BeanProperty, BooleanBeanProperty}

class OriginalFunction(@BeanProperty var component: Component)
  extends AbstractFunction
    with Function
    with ContextOriginator {

  private var userFunction: UserFunction =
    component.getActor.asInstanceOf[UserFunction]

  def call(context: XPathContext, args: Array[Sequence]): Sequence = {
    val c2: XPathContextMajor = userFunction.makeNewContext(context, this)
    c2.setCurrentComponent(component)
    userFunction.call(c2, args)
  }

  def getFunctionItemType: FunctionItemType =
    userFunction.getFunctionItemType

  def getFunctionName: StructuredQName = userFunction.getFunctionName

  def getArity: Int = userFunction.getArity

  def getDescription: String = userFunction.getDescription

  def getContainingPackageName: String =
    component.getContainingPackage.getPackageName

  override def export(out: ExpressionPresenter): Unit = {
    val options: ExpressionPresenter.ExportOptions =
      out.getOptions.asInstanceOf[ExpressionPresenter.ExportOptions]
    out.startElement("origF")
    out.emitAttribute("name", getFunctionName)
    out.emitAttribute("arity", "" + getArity)
    out.emitAttribute("pack", options.componentMap.get(component.getContainingPackage).toString)
    out.endElement()
  }

}
