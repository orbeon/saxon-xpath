package net.sf.saxon.functions

import net.sf.saxon.expr.Expression

import net.sf.saxon.expr.Literal

import net.sf.saxon.expr.PackageData

import net.sf.saxon.expr.XPathContext

import net.sf.saxon.om.Sequence

import net.sf.saxon.trans.XPathException

import net.sf.saxon.value.AnyURIValue

class StaticBaseUri extends SystemFunction {

  override def call(context: XPathContext,
                    args: Array[Sequence]): AnyURIValue =
    new AnyURIValue(getRetainedStaticContext.getStaticBaseUriString)

  def makeFunctionCall(arguments: Array[Expression]): Expression = {
    val pd: PackageData = getRetainedStaticContext.getPackageData
    if (pd.isRelocatable) {
      super.makeFunctionCall(arguments.toIndexedSeq: _*)
    } else {
      Literal.makeLiteral(
        new AnyURIValue(getRetainedStaticContext.getStaticBaseUriString))
    }
  }

}
