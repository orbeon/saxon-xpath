////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
package org.orbeon.saxon.expr.instruct

import org.orbeon.saxon.event.Outputter

import org.orbeon.saxon.expr._

import org.orbeon.saxon.expr.parser.ExpressionTool

import org.orbeon.saxon.expr.parser.RebindingMap

import org.orbeon.saxon.om.StandardNames

import org.orbeon.saxon.trace.ExpressionPresenter

import org.orbeon.saxon.trans.XPathException

import java.util.Collections




/**
  * A compiled xsl:break instruction. The effect of executing this instruction is to register with the
  * dynamic context that a tail call on a pseudo-function break() has been made; the enclosing xsl:iterate
  * loop detects this tail call request and uses it as a signal to terminate execution of the loop.
  */
class BreakInstr extends Instruction with TailCallLoop.TailCallInfo {

  override def operands: java.lang.Iterable[Operand] =
    Collections.emptyList()

  /*@NotNull*/

  def copy(rebindings: RebindingMap): Expression = {
    val b2: BreakInstr = new BreakInstr()
    ExpressionTool.copyLocationInfo(this, b2)
    b2
  }

  override def mayCreateNewNodes(): Boolean = true

  override def isLiftable(forStreaming: Boolean): Boolean = false

  override def getInstructionNameCode(): Int = StandardNames.XSL_BREAK

  def processLeavingTail(output: Outputter, context: XPathContext): TailCall = {
    markContext(context)
    null
  }

  def markContext(context: XPathContext): Unit = {
    var c: XPathContext = context
    while (!(c.isInstanceOf[XPathContextMajor])) c = c.getCaller
    c.asInstanceOf[XPathContextMajor].requestTailCall(this, null)
  }

  override def getExpressionName: String = "xsl:break"

  def export(out: ExpressionPresenter): Unit = {
    out.startElement("break", this)
    out.endElement()
  }

}

// Copyright (c) 2018-2020 Saxonica Limited
// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is "Incompatible With Secondary Licenses", as defined by the Mozilla Public License, v. 2.0.
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
