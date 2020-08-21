////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
package net.sf.saxon.expr.flwor

import net.sf.saxon.utils.Configuration

import net.sf.saxon.event.Outputter

import net.sf.saxon.expr._

import net.sf.saxon.expr.parser.ContextItemStaticInfo

import net.sf.saxon.expr.parser.ExpressionVisitor

import net.sf.saxon.expr.parser.PathMap

import net.sf.saxon.expr.parser.RebindingMap

import net.sf.saxon.expr.sort.GenericAtomicComparer

import net.sf.saxon.functions.DeepEqual

import net.sf.saxon.om.Item

import net.sf.saxon.om.Sequence

import net.sf.saxon.om.SequenceIterator

import net.sf.saxon.trace.ExpressionPresenter

import net.sf.saxon.trans.XPathException

import net.sf.saxon.tree.util.FastStringBuffer

import net.sf.saxon.value.AtomicValue

import net.sf.saxon.value.SequenceExtent

import java.util.ArrayList

import java.util.Arrays

import java.util.LinkedList

import java.util.List

import net.sf.saxon.expr.flwor.Clause.ClauseName.GROUP_BY

import GroupByClause._

import scala.jdk.CollectionConverters._
import scala.util.control.Breaks._


object GroupByClause {

  class ObjectToBeGrouped {

    var groupingValues: Tuple = _

    var retainedValues: Tuple = _

  }

}

/**
 * This class represents an "group by" clause in a FLWOR expression
 */
class GroupByClause(var config: Configuration) extends Clause {

  // Variables bound in the output tuple stream.
  var bindings: Array[LocalVariableBinding] = _

  // One comparer per grouping variable
  var comparers: Array[GenericAtomicComparer] = _

  var retainedTupleOp: Operand = _

  var groupingTupleOp: Operand = _

  override def getClauseKey(): Clause.ClauseName.ClauseName = GROUP_BY

  override def containsNonInlineableVariableReference(
                                                       binding: Binding): Boolean =
    getRetainedTupleExpression.includesBinding(binding) ||
      getGroupingTupleExpression.includesBinding(binding)

  def copy(flwor: FLWORExpression, rebindings: RebindingMap): GroupByClause = {
    val g2: GroupByClause = new GroupByClause(config)
    g2.setLocation(getLocation)
    g2.setPackageData(getPackageData)
    g2.bindings = Array.ofDim[LocalVariableBinding](bindings.length)
    for (i <- 0 until bindings.length) {
      g2.bindings(i) = bindings(i).copy()
    }
    g2.comparers = comparers
    g2.initRetainedTupleExpression(flwor,
      getRetainedTupleExpression
        .copy(rebindings)
        .asInstanceOf[TupleExpression])
    g2.initGroupingTupleExpression(flwor,
      getGroupingTupleExpression
        .copy(rebindings)
        .asInstanceOf[TupleExpression])
    g2
  }

  def initRetainedTupleExpression(flwor: FLWORExpression,
                                  expr: TupleExpression): Unit = {
    retainedTupleOp =
      new Operand(flwor, expr, OperandRole.REPEAT_NAVIGATE_CONSTRAINED)
  }

  def setRetainedTupleExpression(expr: TupleExpression): Unit = {
    retainedTupleOp.setChildExpression(expr)
  }

  def getRetainedTupleExpression(): TupleExpression =
    retainedTupleOp.getChildExpression.asInstanceOf[TupleExpression]

  override def optimize(visitor: ExpressionVisitor,
                        contextItemType: ContextItemStaticInfo): Unit = {
    val list: LinkedList[LocalVariableBinding] =
      new LinkedList[LocalVariableBinding](Arrays.asList(bindings: _*))
    val retainingExpr: LinkedList[LocalVariableReference] =
      new LinkedList[LocalVariableReference]()
    for (o <- getRetainedTupleExpression.operands().asScala) {
      retainingExpr.add(
        o.getChildExpression.asInstanceOf[LocalVariableReference])
    }
    val groupingSize: Int = getGroupingTupleExpression.getSize
    for (i <- groupingSize until list.size
         if list.get(i).getNominalReferenceCount == 0) {
      list.remove(i)
      retainingExpr.remove(i - groupingSize)
    }
    bindings = list.toArray(Array.ofDim[LocalVariableBinding](0))
    getRetainedTupleExpression.setVariables(retainingExpr)
  }

  def initGroupingTupleExpression(flwor: FLWORExpression,
                                  expr: TupleExpression): Unit = {
    groupingTupleOp =
      new Operand(flwor, expr, OperandRole.REPEAT_NAVIGATE_CONSTRAINED)
  }

  def setGroupingTupleExpression(expr: TupleExpression): Unit = {
    groupingTupleOp.setChildExpression(expr)
  }

  def getGroupingTupleExpression(): TupleExpression =
    groupingTupleOp.getChildExpression.asInstanceOf[TupleExpression]

  def setVariableBindings(bindings: Array[LocalVariableBinding]): Unit = {
    this.bindings = bindings
  }

  override def getRangeVariables(): Array[LocalVariableBinding] = bindings

  def setComparers(comparers: Array[GenericAtomicComparer]): Unit = {
    this.comparers = comparers
  }

  /**
   * Get a tuple stream that implements the functionality of this clause, taking its
   * input from another tuple stream which this clause modifies
   *
   * @param base    the input tuple stream
   * @param context the XPath dynamic evaluation context
   * @return the output tuple stream
   */
  override def getPullStream(base: TuplePull,
                             context: XPathContext): TuplePull =
    new GroupByClausePull(base, this, context)

  /**
   * Get a push-mode tuple stream that implements the functionality of this clause, supplying its
   * output to another tuple stream
   *
   * @param destination the output tuple stream
   * @param output      the destination for the result
   * @param context     the dynamic evaluation context
   * @return the push tuple stream that implements the functionality of this clause of the FLWOR
   *         expression
   */
  override def getPushStream(destination: TuplePush,
                             output: Outputter,
                             context: XPathContext): TuplePush =
    new GroupByClausePush(output, destination, this, context)

  /**
   * Process the subexpressions of this clause
   *
   * @param processor the expression processor used to process the subexpressions
   */
  override def processOperands(processor: OperandProcessor): Unit = {
    processor.processOperand(groupingTupleOp)
    processor.processOperand(retainedTupleOp)
  }

  /**
   * Diagnostic print of expression structure. The abstract expression tree
   * is written to the supplied output destination.
   *
   * @param out the expression presenter used to display the structure
   */
  override def explain(out: ExpressionPresenter): Unit = {
    out.startElement("group-by")
    for (o <- getRetainedTupleExpression.operands().asScala) {
      val ref: LocalVariableReference =
        o.getChildExpression.asInstanceOf[LocalVariableReference]
      out.startSubsidiaryElement("by")
      out.emitAttribute("var", ref.getDisplayName)
      out.emitAttribute("slot", ref.getBinding.getLocalSlotNumber.toString)
      out.endSubsidiaryElement()
    }
    out.endElement()
  }

  override def toString(): String = {
    val fsb: FastStringBuffer = new FastStringBuffer(FastStringBuffer.C64)
    fsb.append("group by ... ")
    fsb.toString
  }

  def processGroup(group: List[ObjectToBeGrouped],
                   context: XPathContext): Unit = {
    val bindings: Array[LocalVariableBinding] = getRangeVariables
    val groupingValues: Array[Sequence] =
      group.get(0).groupingValues.getMembers
    for (j <- 0 until groupingValues.length) {
      val v: Sequence = groupingValues(j)
      context.setLocalVariable(bindings(j).getLocalSlotNumber, v)
    }
    for (j <- groupingValues.length until bindings.length) {
      val concatenatedValue: List[Item] = new ArrayList[Item]()
      for (otbg <- group.asScala) {
        val `val`: Sequence =
          otbg.retainedValues.getMembers()(j - groupingValues.length)
        val si: SequenceIterator = `val`.iterate()
        var it: Item = null
        while (({
          it = si.next()
          it
        }) != null) concatenatedValue.add(it)
      }
      val se: SequenceExtent = new SequenceExtent(concatenatedValue)
      context.setLocalVariable(bindings(j).getLocalSlotNumber, se)
    }
  }

  def getComparisonKey(
                        t: Tuple,
                        comparers: Array[GenericAtomicComparer]): TupleComparisonKey =
    new TupleComparisonKey(t.getMembers, comparers)

  override def addToPathMap(pathMap: PathMap,
                            pathMapNodeSet: PathMap.PathMapNodeSet): Unit = {
    throw new UnsupportedOperationException(
      "Cannot use document projection with group-by")
  }

  class TupleComparisonKey(
                            private var groupingValues: Array[Sequence],
                            private var comparers: Array[GenericAtomicComparer]) {

    override def hashCode(): Int = {
      var h: Int = 0x77557755 ^ groupingValues.length
      for (i <- 0 until groupingValues.length) {
        val comparer: GenericAtomicComparer = comparers(i)
        val implicitTimezone: Int = comparer.getContext.getImplicitTimezone
        try {
          val atoms: SequenceIterator = groupingValues(i).iterate()
          breakable {
            while (true) {
              val `val`: AtomicValue = atoms.next().asInstanceOf[AtomicValue]
              if (`val` == null) {
                break()
              }
              h ^= i +
                `val`
                  .getXPathComparable(false,
                    comparer.getCollator,
                    implicitTimezone)
                  .hashCode
            }
          }
        } catch {
          case e: XPathException => {}

        }
      }
      h
    }

    override def equals(other: Any): Boolean = {
      if (!(other.isInstanceOf[TupleComparisonKey])) {
        false
      }
      if (groupingValues.length !=
        other.asInstanceOf[TupleComparisonKey].groupingValues.length) {
        false
      }
      for (i <- 0 until groupingValues.length) {
        try if (!DeepEqual.deepEqual(groupingValues(i).iterate(),
          other
            .asInstanceOf[TupleComparisonKey]
            .groupingValues(i)
            .iterate(),
          comparers(i),
          comparers(i).getContext,
          0)) {
          false
        } catch {
          case e: XPathException => false

        }
      }
      true
    }

  }

}

// Copyright (c) 2018-2020 Saxonica Limited
// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is "Incompatible With Secondary Licenses", as defined by the Mozilla Public License, v. 2.0.
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// Copyright (c) 2011-2020 Saxonica Limited
