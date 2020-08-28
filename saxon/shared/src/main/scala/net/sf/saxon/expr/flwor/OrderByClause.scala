////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
package net.sf.saxon.expr.flwor

import net.sf.saxon.event.Outputter

import net.sf.saxon.expr._

import net.sf.saxon.expr.parser._

import net.sf.saxon.expr.sort.AtomicComparer

import net.sf.saxon.expr.sort.SortKeyDefinition

import net.sf.saxon.expr.sort.SortKeyDefinitionList

import net.sf.saxon.trace.ExpressionPresenter

import net.sf.saxon.trans.XPathException

import net.sf.saxon.tree.util.FastStringBuffer

import net.sf.saxon.value.AtomicValue

import net.sf.saxon.value.SequenceType

import net.sf.saxon.expr.flwor.Clause.ClauseName.ORDER_BY
import Clause.ClauseName.ClauseName
import scala.jdk.CollectionConverters._
import scala.util.control.Breaks._


/**
  * This class represents an "order by" clause in a FLWOR expression
  */
class OrderByClause(flwor: FLWORExpression,
                    sortKeys: Array[SortKeyDefinition],
                    tupleExpression: TupleExpression)
    extends Clause {

// Holds a SortKeyDefinitionList
  var sortKeysOp: Operand = new Operand(
    flwor,
    new SortKeyDefinitionList(sortKeys),
    OperandRole.REPEAT_NAVIGATE_CONSTRAINED)

  var comparators: Array[AtomicComparer] = _

// Holds a TupleExpression
  var tupleOp: Operand = new Operand(flwor,
                                     tupleExpression,
                                     OperandRole.REPEAT_NAVIGATE_CONSTRAINED)

  override def getClauseKey(): ClauseName = ORDER_BY

  override def containsNonInlineableVariableReference(
      binding: Binding): Boolean = getTupleExpression.includesBinding(binding)

  def copy(flwor: FLWORExpression, rebindings: RebindingMap): OrderByClause = {
    val sortKeys: SortKeyDefinitionList = getSortKeyDefinitions
    val sk2: Array[SortKeyDefinition] =
      Array.ofDim[SortKeyDefinition](sortKeys.size)
    for (i <- 0 until sortKeys.size) {
      sk2(i) = sortKeys.getSortKeyDefinition(i).copy(rebindings)
    }
    val obc: OrderByClause = new OrderByClause(
      flwor,
      sk2,
      getTupleExpression.copy(rebindings).asInstanceOf[TupleExpression])
    obc.setLocation(getLocation)
    obc.setPackageData(getPackageData)
    obc.comparators = comparators
    obc
  }

  def getSortKeyDefinitions(): SortKeyDefinitionList =
    sortKeysOp.getChildExpression.asInstanceOf[SortKeyDefinitionList]

  def getAtomicComparers(): Array[AtomicComparer] = comparators

  def getTupleExpression(): TupleExpression =
    tupleOp.getChildExpression.asInstanceOf[TupleExpression]

  /**
    * Get a tuple stream that implements the functionality of this clause, taking its
    * input from another tuple stream which this clause modifies
    *
    * @param base    the input tuple stream
    * @param context XQuery dynamic context
    * @return the output tuple stream
    */
  override def getPullStream(base: TuplePull,
                             context: XPathContext): TuplePull =
    new OrderByClausePull(base, getTupleExpression, this, context)

  /**
    * Get a push-mode tuple stream that implements the functionality of this clause, supplying its
    * output to another tuple stream
    *
    * @param destination the output tuple stream
    * @param output the destination for the result
    * @param context     XQuery dynamic context
    * @return the push tuple stream that implements the functionality of this clause of the FLWOR
    *         expression
    */
  override def getPushStream(destination: TuplePush,
                             output: Outputter,
                             context: XPathContext): TuplePush =
    new OrderByClausePush(output,
                          destination,
                          getTupleExpression,
                          this,
                          context)

  /**
    * Process the subexpressions of this clause
    *
    * @param processor the expression processor used to process the subexpressions
    */
  override def processOperands(processor: OperandProcessor): Unit = {
    processor.processOperand(tupleOp)
    processor.processOperand(sortKeysOp)
  }
//        for (SortKeyDefinition sortKey : sortKeys) {
//            sortKey.processSubExpressions(processor);
//        }
//        for (SortKeyDefinition sortKey : sortKeys) {
//            sortKey.processSubExpressions(processor);
//        }

  override def typeCheck(visitor: ExpressionVisitor,
                contextInfo: ContextItemStaticInfo): Unit = {
    var allKeysFixed: Boolean = true
    val sortKeys: SortKeyDefinitionList = getSortKeyDefinitions
    breakable {
      for (sk <- sortKeys.asScala if !sk.isFixed) {
        allKeysFixed = false
        break()
      }
    }
    if (allKeysFixed) {
      comparators = Array.ofDim[AtomicComparer](sortKeys.size)
    }
    var i: Int = 0
    val tc: TypeChecker = visitor.getConfiguration.getTypeChecker(false)
    for (skd <- sortKeys.asScala) {
      var sortKey: Expression = skd.getSortKey
      val role: RoleDiagnostic =
        new RoleDiagnostic(RoleDiagnostic.ORDER_BY, "", i)
      role.setErrorCode("XPTY0004")
      sortKey = tc.staticTypeCheck(sortKey,
                                   SequenceType.OPTIONAL_ATOMIC,
                                   role,
                                   visitor)
      skd.setSortKey(sortKey, setContext = false)
      skd.typeCheck(visitor, contextInfo)
      if (skd.isFixed) {
        val comp: AtomicComparer = skd.makeComparator(
          visitor.getStaticContext.makeEarlyEvaluationContext())
        skd.setFinalComparator(comp)
        if (allKeysFixed) {
          comparators(i) = comp
        }
      }
      { i += 1; i - 1 }
    }
  }

  def addToPathMap(pathMap: PathMap,
                   pathMapNodeSet: PathMap.PathMapNodeSet): Unit = {
    val sortKeys: SortKeyDefinitionList = getSortKeyDefinitions
    for (skd <- sortKeys.asScala) {
      val sortKey: Expression = skd.getSortKey
      sortKey.addToPathMap(pathMap, pathMapNodeSet)
    }
  }

  /**
    * Diagnostic print of expression structure. The abstract expression tree
    * is written to the supplied output destination.
    *
    * @param out the expression presenter used to display the structure
    */
  override def explain(out: ExpressionPresenter): Unit = {
    out.startElement("order-by")
    for (k <- getSortKeyDefinitions.asScala) {
      out.startSubsidiaryElement("key")
      k.getSortKey.export(out)
      out.endSubsidiaryElement()
    }
    out.endElement()
  }

  override def toString(): String = {
    val fsb: FastStringBuffer = new FastStringBuffer(FastStringBuffer.C64)
    fsb.append("order by ... ")
    fsb.toString
  }

  /*@Nullable*/

  def evaluateSortKey(n: Int, c: XPathContext): AtomicValue = {
    val sortKeys: SortKeyDefinitionList = getSortKeyDefinitions
    sortKeys
      .getSortKeyDefinition(n)
      .getSortKey
      .evaluateItem(c)
      .asInstanceOf[AtomicValue]
  }

}

// Copyright (c) 2018-2020 Saxonica Limited
// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is "Incompatible With Secondary Licenses", as defined by the Mozilla Public License, v. 2.0.
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
