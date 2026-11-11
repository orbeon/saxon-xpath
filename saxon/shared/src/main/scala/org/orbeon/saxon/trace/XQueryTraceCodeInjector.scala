package org.orbeon.saxon.trace

import org.orbeon.saxon.expr.{Expression, LetExpression}
import org.orbeon.saxon.expr.instruct.FixedAttribute

class XQueryTraceCodeInjector extends TraceCodeInjector {
  override def isApplicable(exp: Expression): Boolean =
    exp.isInstruction || exp.isInstanceOf[LetExpression] || exp.isInstanceOf[FixedAttribute]
}
