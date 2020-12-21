////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// Copyright (c) 2018-2020 Saxonica Limited
// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is "Incompatible With Secondary Licenses", as defined by the Mozilla Public License, v. 2.0.
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
package org.orbeon.saxon.expr

import org.orbeon.saxon.expr.parser._
import org.orbeon.saxon.expr.sort.{AtomicComparer, CodepointCollator}
import org.orbeon.saxon.model._
import org.orbeon.saxon.trace.ExpressionPresenter
import org.orbeon.saxon.trans.XPathException
import org.orbeon.saxon.value.{AtomicValue, BooleanValue, SequenceType}

/**
 * Class to handle equivalence comparisons of singletons. This only handles equality comparison.
 * It follows the rules used for grouping and for XQuery 3.0 switch expressions:
 * - each operand must be zero or one atomic values
 * - untypedAtomic is treated as string
 * - non-comparable values are not equal (no type errors)
 * - two empty sequences are equal to each other
 * - two NaN values are equal to each other
 */
class EquivalenceComparison(val p1: Expression, override val operator: Int, val p2: Expression) extends BinaryExpression(p1, operator, p2) with ComparisonExpression {
  private var comparer: AtomicComparer = null
  private var knownToBeComparable = false

  /**
   * Type-check the expression. Default implementation for binary operators that accept
   * any kind of operand
   */
  /*@NotNull*/ @throws[XPathException]
  override def typeCheck(visitor: ExpressionVisitor, contextInfo: ContextItemStaticInfo): Expression = {
    val env = visitor.getStaticContext
    val defaultCollationName = env.getDefaultCollationName
    val config = visitor.getConfiguration
    var collation = config.getCollation(defaultCollationName)
    if (collation == null) collation = CodepointCollator.getInstance
    comparer = new EquivalenceComparer(collation, config.getConversionContext.asInstanceOf[XPathContext])
    val oldOp0 = getLhsExpression
    val oldOp1 = getRhsExpression
    getLhs.typeCheck(visitor, contextInfo)
    getRhs.typeCheck(visitor, contextInfo)
    // Neither operand needs to be sorted
    setLhsExpression(getLhsExpression.unordered(retainAllNodes = false, forStreaming = false))
    setRhsExpression(getRhsExpression.unordered(retainAllNodes = false, forStreaming = false))
    val atomicType = SequenceType.OPTIONAL_ATOMIC
    val tc = config.getTypeChecker(false)
    val role0 = new RoleDiagnostic(RoleDiagnostic.BINARY_EXPR, "eq", 0)
    setLhsExpression(tc.staticTypeCheck(getLhsExpression, atomicType, role0, visitor))
    val role1 = new RoleDiagnostic(RoleDiagnostic.BINARY_EXPR, "eq", 1)
    setRhsExpression(tc.staticTypeCheck(getRhsExpression, atomicType, role1, visitor))
    if (getLhsExpression ne oldOp0) adoptChildExpression(getLhsExpression)
    if (getRhsExpression ne oldOp1) adoptChildExpression(getRhsExpression)
    var t0 = getLhsExpression.getItemType // this is always an atomic type or empty-sequence()
    var t1 = getRhsExpression.getItemType
    if (t0 eq ErrorType)
      t0 = BuiltInAtomicType.ANY_ATOMIC
    if (t1 eq ErrorType)
      t1 = BuiltInAtomicType.ANY_ATOMIC
    if (t0.getUType.union(t1.getUType).overlaps(UType.EXTENSION)) {
      val err = new XPathException("Cannot perform comparisons involving external objects")
      err.setIsTypeError(true)
      err.setErrorCode("XPTY0004")
      err.setLocation(getLocation)
      throw err
    }
    val pt0 = t0.getPrimitiveItemType.asInstanceOf[BuiltInAtomicType]
    val pt1 = t1.getPrimitiveItemType.asInstanceOf[BuiltInAtomicType]
    if (t0 == BuiltInAtomicType.ANY_ATOMIC || t0 == BuiltInAtomicType.UNTYPED_ATOMIC || t1 == BuiltInAtomicType.ANY_ATOMIC || t1 == BuiltInAtomicType.UNTYPED_ATOMIC) {
      // then no static type checking is possible
    }
    else if (Type.isGuaranteedComparable(pt0, pt1, ordered = false)) knownToBeComparable = true
    else if (!Type.isPossiblyComparable(pt0, pt1, ordered = false)) {
      env.issueWarning("Cannot compare " + t0.toString + " to " + t1.toString, getLocation)
      // This is not an error in a switch statement, but it means the branch will never be chosen
    }
    try if (getLhsExpression.isInstanceOf[Literal] && getRhsExpression.isInstanceOf[Literal]) {
      val v = evaluateItem(visitor.getStaticContext.makeEarlyEvaluationContext).materialize
      return Literal.makeLiteral(v, this)
    }
    catch {
      case err: XPathException =>

      // if early evaluation fails, suppress the error: the value might
      // not be needed at run-time
    }
    //        comparer = GenericAtomicComparer.makeAtomicComparer(
    //                    pt0, pt1, collation, getConfiguration.getConversionContext());
    this
  }

  override def getAtomicComparer: AtomicComparer = comparer

  override def getSingletonOperator = operator

  /**
   * Determine whether untyped atomic values should be converted to the type of the other operand
   *
   * @return true if untyped values should be converted to the type of the other operand, false if they
   *         should be converted to strings.
   */
  override def convertsUntypedToOther = false

  /**
   * Determine the static cardinality. Returns [1..1]
   */
  override def computeCardinality = StaticProperty.EXACTLY_ONE

  /**
   * Determine the data type of the expression
   *
   * @return Type.BOOLEAN
   */
  override def getItemType = BuiltInAtomicType.BOOLEAN

  def isKnownToBeComparable = knownToBeComparable

  def getComparer = comparer

  /**
   * Copy an expression. This makes a deep copy.
   *
   * @return the copy of the original expression
   * @param rebindings
   */
  override def copy(rebindings: RebindingMap) = {
    val sc = new EquivalenceComparison(getLhsExpression.copy(rebindings), operator, getRhsExpression.copy(rebindings))
    ExpressionTool.copyLocationInfo(this, sc)
    sc.comparer = comparer
    sc.knownToBeComparable = knownToBeComparable
    sc
  }

  /**
   * Evaluate the expression in a given context
   *
   * @param context the given context for evaluation
   * @return a BooleanValue representing the result of the numeric comparison of the two operands
   */
  @throws[XPathException]
  override def evaluateItem(context: XPathContext) = BooleanValue.get(effectiveBooleanValue(context))

  /**
   * Evaluate the expression in a boolean context
   *
   * @param context the given context for evaluation
   * @return a boolean representing the result of the numeric comparison of the two operands
   */
  @throws[XPathException]
  override def effectiveBooleanValue(context: XPathContext): Boolean = {
    val v0 = getLhsExpression.evaluateItem(context).asInstanceOf[AtomicValue]
    val v1 = getRhsExpression.evaluateItem(context).asInstanceOf[AtomicValue]
    if (v0 == null || v1 == null) return v0 eq v1
    val comp2 = comparer.provideContext(context)
    (knownToBeComparable || Type.isGuaranteedComparable(v0.getPrimitiveType, v1.getPrimitiveType, ordered = false)) && comp2.comparesEqual(v0, v1)
  }

  /**
   * Get a name identifying the kind of expression, in terms meaningful to a user.
   *
   * @return a name identifying the kind of expression, in terms meaningful to a user.
   *         The name will always be in the form of a lexical XML QName, and should match the name used
   *         in export() output displaying the expression.
   */
  override def getExpressionName = "equivalent"

  override  def explainExtraAttributes(out: ExpressionPresenter) = out.emitAttribute("cardinality", "singleton")
}

// Copyright (c) 2010-2020 Saxonica Limited