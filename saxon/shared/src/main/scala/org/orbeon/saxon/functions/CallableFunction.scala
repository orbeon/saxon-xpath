package org.orbeon.saxon.functions

import org.orbeon.saxon.expr.Callable
import org.orbeon.saxon.expr.XPathContext
import org.orbeon.saxon.expr.instruct.UserFunction
import org.orbeon.saxon.model.AnyFunctionType
import org.orbeon.saxon.model.FunctionItemType
import org.orbeon.saxon.om.Sequence
import org.orbeon.saxon.om.StructuredQName
import org.orbeon.saxon.query.{AnnotationList, XQueryFunctionLibrary}
import org.orbeon.saxon.trace.ExpressionPresenter
import org.orbeon.saxon.trans.SymbolicName

import scala.beans.BeanProperty

class CallableFunction extends AbstractFunction {

  @BeanProperty
  var annotationList: AnnotationList = _

  @BeanProperty
  var callable: Callable = _
  private var name: SymbolicName.F = _
  private var `type`: FunctionItemType = _

  def this(name: SymbolicName.F, callable: Callable, `type`: FunctionItemType) {
    this()
    this.name = name
    this.callable = callable
    this.`type` = `type`
  }

  def this(arity: Int, callable: Callable, `type`: FunctionItemType) = {
    this()
    this.name = new SymbolicName.F(new StructuredQName("", "anon", "anon"), arity)
    this.callable = callable
    this.`type` = `type`
  }

  def setType(`type`: FunctionItemType): Unit = {
    this.`type` = `type`
  }

  override def getFunctionItemType: FunctionItemType = {
    if (`type` == AnyFunctionType &&
      callable.isInstanceOf[XQueryFunctionLibrary.UnresolvedCallable]) {
      val uf: UserFunction = callable
        .asInstanceOf[XQueryFunctionLibrary.UnresolvedCallable]
        .getFunction
      if (uf != null) {
        `type` = uf.getFunctionItemType
      }
    }
    `type`
  }

  def getFunctionName: StructuredQName = name.getComponentName

  def getDescription: String = callable.toString

  def getArity: Int = name.getArity

  def call(context: XPathContext, args: Array[Sequence]): Sequence =
    callable.call(context, args)

  override def export(out: ExpressionPresenter): Unit = {
    throw new UnsupportedOperationException(
      "A CallableFunction is a transient value that cannot be exported")
  }

}
