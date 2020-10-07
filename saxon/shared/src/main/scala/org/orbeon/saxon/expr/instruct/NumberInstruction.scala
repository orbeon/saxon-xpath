////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// Copyright (c) 2018-2020 Saxonica Limited
// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is "Incompatible With Secondary Licenses", as defined by the Mozilla Public License, v. 2.0.
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
package org.orbeon.saxon.expr.instruct

import org.orbeon.saxon.expr._
import org.orbeon.saxon.expr.parser.ContextItemStaticInfo
import org.orbeon.saxon.expr.parser.ExpressionTool
import org.orbeon.saxon.expr.parser.ExpressionVisitor
import org.orbeon.saxon.expr.parser.RebindingMap
import org.orbeon.saxon.model.BuiltInAtomicType
import org.orbeon.saxon.model.ItemType
import org.orbeon.saxon.model.Type
import org.orbeon.saxon.om.NodeInfo
//import scala.collection.compat._
import scala.jdk.CollectionConverters._
import org.orbeon.saxon.pattern.Pattern
import org.orbeon.saxon.trace.ExpressionPresenter
import org.orbeon.saxon.trans.XPathException
import org.orbeon.saxon.tree.iter.ListIterator
import org.orbeon.saxon.tree.util.Navigator
import org.orbeon.saxon.value.Int64Value
import org.orbeon.saxon.value.IntegerValue
import org.orbeon.saxon.value.SequenceType
import java.util

/**
 * This instruction performs the node-counting function of the xsl:number instruction.
 * It delivers as its result a sequence of integers. The parent expression is typically
 * a {@link NumberSequenceFormatter} which takes this sequence of integers and performs
 * the necessary formatting.
 */
object NumberInstruction {
  val SINGLE = 0
  val MULTI = 1
  val ANY = 2
  val SIMPLE = 3
  val LEVEL_NAMES = Array[String]("single", "multi", "any", "simple")
}

class NumberInstruction(val select: Expression, var level: Int, val count: Pattern, val from: Pattern) extends Expression {
  private var selectOp: Operand = null
  private var countOp: Operand = null
  private var fromOp: Operand = null
  private var hasVariablesInPatterns = false
  assert(select != null)
  selectOp = new Operand(this, select, new OperandRole(0, OperandUsage.NAVIGATION, SequenceType.SINGLE_NODE))
  if (count != null) countOp = new Operand(this, count, OperandRole.INSPECT)
  if (from != null) fromOp = new Operand(this, from, OperandRole.INSPECT)
  this.hasVariablesInPatterns = Pattern.patternContainsVariable(count) || Pattern.patternContainsVariable(from)

  override def isInstruction = true

  override def operands = operandSparseList(selectOp, countOp, fromOp)

  /**
   * Get the level attribute
   *
   * @return the coded value of the level attribute
   */
  def getLevel = level

  /**
   * Get the count pattern, if specified
   *
   * @return the count pattern if there is one, otherwise null
   */
  def getCount = if (countOp == null) null
  else countOp.getChildExpression.asInstanceOf[Pattern]

  /**
   * Get the from pattern, if specified
   *
   * @return the from pattern if there is one, otherwise null
   */
  def getFrom = if (fromOp == null) null
  else fromOp.getChildExpression.asInstanceOf[Pattern]

  /**
   * Get the select expression
   *
   * @return the select expression (which defaults to a ContextItemExpression)
   */
  def getSelect = selectOp.getChildExpression

  /**
   * Copy an expression. This makes a deep copy.
   *
   * @param rebindings a mutable list of (old binding, new binding) pairs
   *                   *                   that is used to update the bindings held in any
   *                   *                   local variable references that are copied.
   * @return the copy of the original expression
   */
  /*@NotNull*/ override def copy(rebindings: RebindingMap) = {
    val exp = new NumberInstruction(copy(selectOp, rebindings), level, copy(getCount, rebindings), copy(getFrom, rebindings))
    ExpressionTool.copyLocationInfo(this, exp)
    exp
  }

  private def copy(op: Operand, rebindings: RebindingMap) = if (op == null) null
  else op.getChildExpression.copy(rebindings)

  private def copy(op: Pattern, rebindings: RebindingMap) = if (op == null) null
  else op.copy(rebindings)

  override def getItemType = BuiltInAtomicType.INTEGER

  override def computeCardinality = level match {
    case NumberInstruction.SIMPLE => 0
    case NumberInstruction.SINGLE => 0
    case NumberInstruction.ANY =>
      StaticProperty.ALLOWS_ZERO_OR_ONE
    case NumberInstruction.MULTI => 0
    case _ =>
      StaticProperty.ALLOWS_ZERO_OR_MORE
  }

  /**
   * An implementation of Expression must provide at least one of the methods evaluateItem(), iterate(), or process().
   * This method indicates which of these methods is provided directly. The other methods will always be available
   * indirectly, using an implementation that relies on one of the other methods.
   *
   * @return the implementation method, for example { @link #ITERATE_METHOD} or { @link #EVALUATE_METHOD} or
   *         { @link #PROCESS_METHOD}
   */
  override def getImplementationMethod = Expression.ITERATE_METHOD

  /**
   * Perform optimisation of an expression and its subexpressions. This is the third and final
   * phase of static optimization.
   * <p>This method is called after all references to functions and variables have been resolved
   * to the declaration of the function or variable, and after all type checking has been done.</p>
   *
   * @param visitor     an expression visitor
   * @param contextInfo the static type of "." at the point where this expression is invoked.
   *                    The parameter is set to null if it is known statically that the context item will be undefined.
   *                    If the type of the context item is not known statically, the argument is set to
   *                    { @link Type#ITEM_TYPE}
   * @return the original expression, rewritten if appropriate to optimize execution
   * @throws XPathException if an error is discovered during this phase
   *                        (typically a type error)
   */
  @throws[XPathException]
  override def optimize(visitor: ExpressionVisitor, contextInfo: ContextItemStaticInfo): Expression = {
    var e = super.optimize(visitor, contextInfo)
    if (e ne this) return e
    if ("EE" == getPackageData.getTargetEdition) {
      e = visitor.obtainOptimizer.optimizeNumberInstruction(this, contextInfo)
      if (e != null) return e
    }
    this
  }

  @throws[XPathException]
  override def iterate(context: XPathContext): ListIterator[IntegerValue] = {
    val vec = new util.ArrayList[IntegerValue](1)
    val source = selectOp.getChildExpression.evaluateItem(context).asInstanceOf[NodeInfo]
    level match {
      case NumberInstruction.SIMPLE =>
        val value = Navigator.getNumberSimple(source, context)
        if (value != 0) vec.add(Int64Value.makeIntegerValue(value))
      /* case NumberInstruction.SINGLE =>
         val value = Navigator.getNumberSingle(source, getCount, getFrom, context)  // getNumberSingle method has commented out in Navigator
         if (value != 0) vec.add(Int64Value.makeIntegerValue(value))*/
      case NumberInstruction.ANY =>
        val value = Navigator.getNumberAny(this, source, getCount, getFrom, context, hasVariablesInPatterns)
        if (value != 0) vec.add(Int64Value.makeIntegerValue(value))
      case NumberInstruction.MULTI =>

      /* for (n <- Navigator.getNumberMulti(source, getCount, getFrom, context).asScala) { // getNumberMulti method has commented out in Navigator
         vec.add(Int64Value.makeIntegerValue(n))
       }*/
    }
    new ListIterator[IntegerValue](vec)
  }

  override def getExpressionName = "xsl:number"

  /**
   * Diagnostic print of expression structure. The abstract expression tree
   * is written to the supplied output destination.
   */
  @throws[XPathException]
  override def `export`(out: ExpressionPresenter) = {
    out.startElement("nodeNum", this)
    out.emitAttribute("level", NumberInstruction.LEVEL_NAMES(level))
    out.setChildRole("select")
    selectOp.getChildExpression.`export`(out)
    if (countOp != null) {
      out.setChildRole("count")
      getCount.`export`(out)
    }
    if (fromOp != null) {
      out.setChildRole("from")
      getFrom.`export`(out)
    }
    out.endElement
  }
}