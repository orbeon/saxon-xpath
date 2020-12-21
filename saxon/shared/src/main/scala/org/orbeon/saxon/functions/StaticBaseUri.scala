package org.orbeon.saxon.functions

import org.orbeon.saxon.expr.Expression

import org.orbeon.saxon.expr.Literal

import org.orbeon.saxon.expr.PackageData

import org.orbeon.saxon.expr.XPathContext

import org.orbeon.saxon.om.Sequence

import org.orbeon.saxon.trans.XPathException

import org.orbeon.saxon.value.AnyURIValue

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
