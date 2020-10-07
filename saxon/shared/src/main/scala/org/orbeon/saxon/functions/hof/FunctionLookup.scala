
package org.orbeon.saxon.functions.hof

import org.orbeon.saxon.expr._
import org.orbeon.saxon.expr.instruct.UserFunction
import org.orbeon.saxon.expr.parser.ExpressionTool
import org.orbeon.saxon.functions._
import org.orbeon.saxon.om._
import org.orbeon.saxon.sxpath.IndependentContext
import org.orbeon.saxon.trans.{SymbolicName, Visibility, XPathException}
import org.orbeon.saxon.tree.iter.ManualIterator
import org.orbeon.saxon.value.{IntegerValue, QNameValue}

/**
 * This class supports the function-lookup() function in XPath 3.0. It takes as arguments
 * a function name (QName) and arity, and returns a function item representing that
 * function if found, or an empty sequence if not found.
 */
class FunctionLookup() extends ContextAccessorFunction {
  private var boundContext :XPathContext = _

  override def makeFunctionCall(arguments: Expression*):Expression = {
    val pack = getRetainedStaticContext.getPackageData
    //if (pack.isInstanceOf[StylesheetPackage]) pack.asInstanceOf[StylesheetPackage].setRetainUnusedFunctions()
    super.makeFunctionCall(arguments:_*)
  }

  /**
   * Determine whether two expressions are equivalent
   */
  override def equals(o: Any) = super.equals(o) && ExpressionTool.equalOrNull(getRetainedStaticContext, o.asInstanceOf[FunctionLookup].getRetainedStaticContext)

  /**
   * Bind a context item to appear as part of the function's closure. If this method
   * has been called, the supplied context item will be used in preference to the
   * context item at the point where the function is actually called.
   *
   * @param context the context to which the function applies. Must not be null.
   */
  override def bindContext(context: XPathContext):Function = {
    val bound = SystemFunction.makeFunction("function-lookup", getRetainedStaticContext, 2).asInstanceOf[FunctionLookup]
    val focusIterator:FocusIterator = context.getCurrentIterator
    if (focusIterator != null) {
      val c2 : XPathContextMinor = context.newMinorContext
      val mi = new ManualIterator(context.getContextItem, focusIterator.position)
      c2.setCurrentIterator(mi)
      bound.boundContext = c2
    }
    else bound.boundContext = context
    bound
  }

  @throws[XPathException]
  def lookup(name: StructuredQName, arity: Int, context: XPathContext): Function = {
    val controller = context.getController
    val exec = controller.getExecutable
    val rsc = getRetainedStaticContext
    val pd = rsc.getPackageData
    val lib = exec.getFunctionLibrary
    val sn = new SymbolicName.F(name, arity)
    val ic = new IndependentContext(controller.getConfiguration)
    ic.setDefaultCollationName(rsc.getDefaultCollationName)
    ic.setBaseURI(rsc.getStaticBaseUriString)
    ic.setDecimalFormatManager(rsc.getDecimalFormatManager)
    ic.setNamespaceResolver(rsc)
    ic.setPackageData(pd)
    try {
      val fi = lib.getFunctionItem(sn, ic)
      if (fi.isInstanceOf[UserFunction]) {
        val vis = fi.asInstanceOf[UserFunction].getDeclaredVisibility
        if (vis == Visibility.ABSTRACT) return null
      }
      if (fi.isInstanceOf[CallableFunction]) fi.asInstanceOf[CallableFunction].setCallable(new CallableWithBoundFocus(fi.asInstanceOf[CallableFunction].getCallable, context))
      else if (fi.isInstanceOf[ContextItemAccessorFunction]) return fi.asInstanceOf[ContextItemAccessorFunction].bindContext(context)
      else if (fi.isInstanceOf[SystemFunction] && fi.asInstanceOf[SystemFunction].dependsOnContextItem) return new SystemFunctionWithBoundContextItem(fi.asInstanceOf[SystemFunction], context)
      fi
    } catch {
      case e: XPathException =>
        if ("XPST0017" == e.getErrorCodeLocalPart) return null
        throw e
    }
  }

  /**
   * Evaluate the expression
   *
   * @param context   the dynamic evaluation context
   * @param arguments the values of the arguments, supplied as SequenceIterators
   * @return the result of the evaluation, in the form of a SequenceIterator
   * if a dynamic error occurs during the evaluation of the expression
   */
  @throws[XPathException]
  override def call(context: XPathContext, arguments: Array[Sequence]): ZeroOrOne[_ <: Item] = {
    var c :XPathContext=boundContext
    if (boundContext ==null){
      c= context
    }
    val qname = arguments(0).head.asInstanceOf[QNameValue]
    val arity = arguments(1).head.asInstanceOf[IntegerValue]
    var fi = lookup(qname.getStructuredQName, arity.longValue.toInt, c)
    if (fi == null) return ZeroOrOne.empty
    if (fi.isInstanceOf[ContextAccessorFunction]) fi = fi.asInstanceOf[ContextAccessorFunction].bindContext(c)
    val target = if (fi.isInstanceOf[UserFunction]) fi.asInstanceOf[UserFunction].getDeclaringComponent
    else null
    val agent:ExportAgent = (out) => makeFunctionCall(Literal.makeLiteral(qname), Literal.makeLiteral(arity)).`export`(out)
    val result = new UserFunctionReference.BoundUserFunction(agent, fi, target, c.getController)
    new ZeroOrOne[Item](result)
  }
}

// Copyright (c) 2011-2020 Saxonica Limited