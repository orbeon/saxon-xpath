////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
package org.orbeon.saxon.functions.hof

import org.orbeon.saxon.expr.XPathContext
import org.orbeon.saxon.functions.AbstractFunction
import org.orbeon.saxon.model.FunctionItemType
import org.orbeon.saxon.model.SpecificFunctionType
import org.orbeon.saxon.om.Function
import org.orbeon.saxon.om.Sequence
import org.orbeon.saxon.om.StructuredQName
import org.orbeon.saxon.query.XQueryFunction
import org.orbeon.saxon.trans.SymbolicName

class UnresolvedXQueryFunctionItem(private val fd: XQueryFunction,
                                   private val functionName: SymbolicName.F,
                                   private val ref: UserFunctionReference)
  extends AbstractFunction {

  override def getFunctionItemType: FunctionItemType =
    new SpecificFunctionType(fd.getArgumentTypes, fd.getResultType)

  override def getFunctionName: StructuredQName =
    functionName.getComponentName

  override def getArity: Int = fd.getNumberOfArguments

  override def call(context: XPathContext, args: Array[Sequence]): Sequence =
    ref.evaluateItem(context).call(context, args)

  override def getDescription: String = functionName.toString

  def getFunctionReference: UserFunctionReference = ref

}