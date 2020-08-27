package net.sf.saxon.expr.instruct

import net.sf.saxon.expr.Component

import net.sf.saxon.expr.ContextOriginator

import net.sf.saxon.expr.XPathContext

import net.sf.saxon.expr.XPathContextMajor

import net.sf.saxon.functions.AbstractFunction

import net.sf.saxon.model.FunctionItemType

import net.sf.saxon.om.Function

import net.sf.saxon.om.Sequence

import net.sf.saxon.om.StructuredQName

import net.sf.saxon.trace.ExpressionPresenter

import net.sf.saxon.trans.XPathException

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

  def getFunctionItemType(): FunctionItemType =
    userFunction.getFunctionItemType

  def getFunctionName(): StructuredQName = userFunction.getFunctionName

  def getArity(): Int = userFunction.getArity

  def getDescription(): String = userFunction.getDescription

  def getContainingPackageName(): String =
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
