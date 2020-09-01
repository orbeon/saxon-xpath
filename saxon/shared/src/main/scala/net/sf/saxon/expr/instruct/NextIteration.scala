////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
package net.sf.saxon.expr.instruct

import net.sf.saxon.event.Outputter

import net.sf.saxon.expr._

import net.sf.saxon.expr.parser.ContextItemStaticInfo

import net.sf.saxon.expr.parser.ExpressionTool

import net.sf.saxon.expr.parser.ExpressionVisitor

import net.sf.saxon.expr.parser.RebindingMap

import net.sf.saxon.om.Sequence

import net.sf.saxon.om.StandardNames

import net.sf.saxon.trace.ExpressionPresenter

import net.sf.saxon.trans.XPathException

import java.util.ArrayList

import java.util.Arrays

import java.util.List




/**
  * Implements a xsl:next-iteration instruction within the body of xsl:iterate
  */
class NextIteration extends Instruction with TailCallLoop.TailCallInfo {

  private var actualParams: Array[WithParam] = null

  def setParameters(actualParams: Array[WithParam]): Unit = {
    this.actualParams = actualParams
  }

  def getParameters: Array[WithParam] = actualParams

  override def isLiftable(forStreaming: Boolean): Boolean = false

  /**
    * Get the namecode of the instruction for use in diagnostics
    *
    * @return a code identifying the instruction: typically but not always
    * the fingerprint of a name in the XSLT namespace
    */
  override def getInstructionNameCode(): Int = StandardNames.XSL_NEXT_ITERATION

  /*@NotNull*/

  override def simplify(): Expression = {
    WithParam.simplify(actualParams)
    this
  }

  /*@NotNull*/

  override def typeCheck(visitor: ExpressionVisitor,
                         contextInfo: ContextItemStaticInfo): Expression = {
    WithParam.typeCheck(actualParams, visitor, contextInfo)
    this
  }

  /*@NotNull*/

  def copy(rebindings: RebindingMap): Expression = {
    val c2: NextIteration = new NextIteration()
    ExpressionTool.copyLocationInfo(this, c2)
    c2.actualParams = WithParam.copy(c2, actualParams, rebindings)
    c2
  }

  /**
    * Get the immediate sub-expressions of this expression, with information about the relationship
    * of each expression to its parent expression. Default implementation
    * works off the results of iterateSubExpressions()
    *
    * @return an iterator containing the sub-expressions of this expression
    */
  override def operands: java.lang.Iterable[Operand] = {
    val list: List[Operand] = new ArrayList[Operand]()
    WithParam.gatherOperands(this, actualParams, list)
    list
  }

  /**
    * Get the (partial) name of a class that supports streaming of this kind of expression
    *
    * @return the partial name of a class that can be instantiated to provide streaming support in Saxon-EE,
    * or null if there is no such class
    */
  override def getStreamerName(): String = "NextIteration"

  /*@Nullable*/

  def processLeavingTail(output: Outputter, context: XPathContext): TailCall = {
    var c: XPathContext = context
    while (!(c.isInstanceOf[XPathContextMajor])) c = c.getCaller
    val cm: XPathContextMajor = c.asInstanceOf[XPathContextMajor]
    if (actualParams.length == 1) {
      cm.setLocalVariable(actualParams(0).getSlotNumber,
                          actualParams(0).getSelectValue(context))
    } else {
// we can't overwrite any of the parameters until we've evaluated all of them: test iterate012
      val oldVars: Array[Sequence] = cm.getAllVariableValues
      val newVars: Array[Sequence] = Arrays.copyOf(oldVars, oldVars.length)
      for (wp <- actualParams) {
        newVars(wp.getSlotNumber) = wp.getSelectValue(context)
      }
      cm.resetAllVariableValues(newVars)
    }
    cm.requestTailCall(this, null)
    null
  }

  def export(out: ExpressionPresenter): Unit = {
    out.startElement("nextIteration", this)
    if (actualParams != null && actualParams.length > 0) {
      WithParam.exportParameters(actualParams, out, tunnel = false)
    }
    out.endElement()
  }

}

// Copyright (c) 2018-2020 Saxonica Limited
// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is "Incompatible With Secondary Licenses", as defined by the Mozilla Public License, v. 2.0.
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
