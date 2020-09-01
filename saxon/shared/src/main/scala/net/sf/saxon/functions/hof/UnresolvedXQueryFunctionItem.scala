////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
package net.sf.saxon.functions.hof

import net.sf.saxon.expr.XPathContext
import net.sf.saxon.functions.AbstractFunction
import net.sf.saxon.model.FunctionItemType
import net.sf.saxon.model.SpecificFunctionType
import net.sf.saxon.om.Function
import net.sf.saxon.om.Sequence
import net.sf.saxon.om.StructuredQName
import net.sf.saxon.query.XQueryFunction
import net.sf.saxon.trans.SymbolicName

class UnresolvedXQueryFunctionItem(private val fd: XQueryFunction,
                                   private val functionName: SymbolicName.F,
                                   private val ref: UserFunctionReference)
  extends AbstractFunction {

  override def getFunctionItemType(): FunctionItemType =
    new SpecificFunctionType(fd.getArgumentTypes, fd.getResultType)

  override def getFunctionName: StructuredQName =
    functionName.getComponentName

  override def getArity(): Int = fd.getNumberOfArguments

  override def call(context: XPathContext, args: Array[Sequence]): Sequence =
    ref.evaluateItem(context).call(context, args)

  override def getDescription(): String = functionName.toString

  def getFunctionReference: UserFunctionReference = ref

}