////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
package org.orbeon.saxon.expr.flwor

import org.orbeon.saxon.event.Outputter

import org.orbeon.saxon.expr._

import org.orbeon.saxon.expr.parser._

import org.orbeon.saxon.model.ItemType

import org.orbeon.saxon.trace.ExpressionPresenter

//import scala.collection.compat._
import scala.jdk.CollectionConverters._

import org.orbeon.saxon.tree.util.FastStringBuffer

import java.util.List

import org.orbeon.saxon.expr.flwor.Clause.ClauseName.WHERE




/**
  * A "where" clause in a FLWOR expression
  */
class WhereClause(flwor: FLWORExpression, predicate: Expression)
    extends Clause {

  private var predicateOp: Operand =
    new Operand(flwor, predicate, OperandRole.INSPECT)

   override def setRepeated(repeated: Boolean): Unit = {
    super.setRepeated(repeated)
    if (repeated) {
      this.predicateOp.setOperandRole(OperandRole.REPEAT_INSPECT)
    }
  }

  override def getClauseKey(): Clause.ClauseName.ClauseName = WHERE

  def getPredicate: Expression = predicateOp.getChildExpression

  def setPredicate(predicate: Expression): Unit = {
    predicateOp.setChildExpression(predicate)
  }

  def copy(flwor: FLWORExpression, rebindings: RebindingMap): WhereClause = {
    val w2: WhereClause = new WhereClause(flwor, getPredicate.copy(rebindings))
    w2.setLocation(getLocation)
    w2.setPackageData(getPackageData)
    w2
  }

  /**
    * Type-check the expression
    */
  override def typeCheck(visitor: ExpressionVisitor,
                         contextInfo: ContextItemStaticInfo): Unit = {
    super.typeCheck(visitor, contextInfo)
  }

  /**
    * Get a tuple stream that implements the functionality of this clause, taking its
    * input from another tuple stream which this clause modifies
    *
    * @param base    the input tuple stream
    * @param context the dynamic evaluation context
    * @return the output tuple stream
    */
  override def getPullStream(base: TuplePull,
                             context: XPathContext): TuplePull =
    new WhereClausePull(base, getPredicate)

  override def gatherVariableReferences(
      visitor: ExpressionVisitor,
      binding: Binding,
      references: List[VariableReference]): Unit = {
    ExpressionTool.gatherVariableReferences(getPredicate, binding, references)
  }

  override def refineVariableType(visitor: ExpressionVisitor,
                                  references: List[VariableReference],
                                  returnExpr: Expression): Unit = {
    val actualItemType: ItemType = getPredicate.getItemType
    for (ref <- references.asScala) {
      ref.refineVariableType(
        actualItemType,
        getPredicate.getCardinality,
        if (getPredicate.isInstanceOf[Literal])
          getPredicate.asInstanceOf[Literal].getValue
        else null,
        getPredicate.getSpecialProperties
      )
      ExpressionTool.resetStaticProperties(returnExpr)
    }
  }

  /**
    * Get a push-mode tuple stream that implements the functionality of this clause, supplying its
    * output to another tuple stream
    *
    * @param destination the output tuple stream
    * @param output the destination for the result
    * @param context     the dynamic evaluation context
    * @return the push tuple stream that implements the functionality of this clause of the FLWOR
    *         expression
    */
  override def getPushStream(destination: TuplePush,
                             output: Outputter,
                             context: XPathContext): TuplePush =
    new WhereClausePush(output, destination, getPredicate)

  /**
    * Process the subexpressions of this clause
    *
    * @param processor the expression processor used to process the subexpressions
    */
  override def processOperands(processor: OperandProcessor): Unit = {
    processor.processOperand(predicateOp)
  }

  override def addToPathMap(pathMap: PathMap,
                            pathMapNodeSet: PathMap.PathMapNodeSet): Unit = {
    getPredicate.addToPathMap(pathMap, pathMapNodeSet)
  }

  /**
    * Diagnostic print of expression structure. The abstract expression tree
    * is written to the supplied output destination.
    *
    * @param out the expression presenter used to display the structure
    */
  override def explain(out: ExpressionPresenter): Unit = {
    out.startElement("where")
    getPredicate.export(out)
    out.endElement()
  }

  override def toShortString: String = {
    val fsb: FastStringBuffer = new FastStringBuffer(FastStringBuffer.C64)
    fsb.append("where ")
    fsb.append(getPredicate.toShortString)
    fsb.toString
  }

  override def toString: String = {
    val fsb: FastStringBuffer = new FastStringBuffer(FastStringBuffer.C64)
    fsb.append("where ")
    fsb.append(getPredicate.toString)
    fsb.toString
  }

}

// Copyright (c) 2018-2020 Saxonica Limited
// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is "Incompatible With Secondary Licenses", as defined by the Mozilla Public License, v. 2.0.
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
