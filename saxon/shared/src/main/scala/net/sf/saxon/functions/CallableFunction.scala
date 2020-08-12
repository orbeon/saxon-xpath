package net.sf.saxon.functions

import net.sf.saxon.expr.Callable
import net.sf.saxon.expr.XPathContext
import net.sf.saxon.expr.instruct.UserFunction
import net.sf.saxon.model.AnyFunctionType
import net.sf.saxon.model.FunctionItemType
import net.sf.saxon.om.Sequence
import net.sf.saxon.om.StructuredQName
import net.sf.saxon.query.{AnnotationList, XQueryFunctionLibrary}
import net.sf.saxon.trace.ExpressionPresenter
import net.sf.saxon.trans.SymbolicName

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

  override def getFunctionItemType(): FunctionItemType = {
    if (`type` == AnyFunctionType.getInstance &&
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

  def getFunctionName(): StructuredQName = name.getComponentName

  def getDescription(): String = callable.toString

  def getArity(): Int = name.getArity

  def call(context: XPathContext, args: Array[Sequence]): Sequence =
    callable.call(context, args)

  override def export(out: ExpressionPresenter): Unit = {
    throw new UnsupportedOperationException(
      "A CallableFunction is a transient value that cannot be exported")
  }

}
