////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
package net.sf.saxon.expr.flwor

import net.sf.saxon.event.Outputter

import net.sf.saxon.expr.XPathContext

import net.sf.saxon.expr.parser.PathMap

import net.sf.saxon.expr.parser.RebindingMap

import net.sf.saxon.trace.ExpressionPresenter

import net.sf.saxon.trans.XPathException

import net.sf.saxon.tree.util.FastStringBuffer

import net.sf.saxon.expr.flwor.Clause.ClauseName.COUNT

import scala.beans.{BeanProperty, BooleanBeanProperty}




/**
  * A "count" clause in a FLWOR expression
  */
class CountClause extends Clause {

  @BeanProperty
  var rangeVariable: LocalVariableBinding = _

  override def getClauseKey(): Clause.ClauseName.ClauseName = COUNT

  def copy(flwor: FLWORExpression, rebindings: RebindingMap): CountClause = {
    val c2: CountClause = new CountClause()
    c2.rangeVariable = rangeVariable.copy()
    c2.setPackageData(getPackageData)
    c2.setLocation(getLocation)
    c2
  }

  /**
    * Get the number of variables bound by this clause
    *
    * @return the number of variable bindings
    */
  override def getRangeVariables(): Array[LocalVariableBinding] =
    Array(rangeVariable)

  override def getPullStream(base: TuplePull,
                             context: XPathContext): TuplePull =
    new CountClausePull(base, this)

  /**
    * Get a push-mode tuple stream that implements the functionality of this clause, supplying its
    * output to another tuple stream
    *
    * @param destination the output tuple stream
    * @param output the destination for the result
    * @param context
    * @return the push tuple stream that implements the functionality of this clause of the FLWOR
    *         expression
    */
  override def getPushStream(destination: TuplePush,
                             output: Outputter,
                             context: XPathContext): TuplePush =
    new CountClausePush(output, destination, this)

  /**
    * Process the subexpressions of this clause
    *
    * @param processor the expression processor used to process the subexpressions
    */
  override def processOperands(processor: OperandProcessor): Unit = ()
// no action
// no action

  override def addToPathMap(pathMap: PathMap,
                            pathMapNodeSet: PathMap.PathMapNodeSet): Unit = ()
// no action
// no action

  /**
    * Diagnostic print of expression structure. The abstract expression tree
    * is written to the supplied output destination.
    *
    * @param out the expression presenter used to display the structure
    */
  override def explain(out: ExpressionPresenter): Unit = {
    out.startElement("count")
    out.emitAttribute("var", getRangeVariable.getVariableQName)
    out.endElement()
  }

  override def toString: String = {
    val fsb: FastStringBuffer = new FastStringBuffer(FastStringBuffer.C64)
    fsb.append("count $")
    fsb.append(rangeVariable.getVariableQName.getDisplayName)
    fsb.toString
  }

}

// Copyright (c) 2018-2020 Saxonica Limited
// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is "Incompatible With Secondary Licenses", as defined by the Mozilla Public License, v. 2.0.
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// Copyright (c) 2011-2020 Saxonica Limited
