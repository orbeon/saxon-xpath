package org.orbeon.saxon.expr

import org.orbeon.saxon.expr.ValueComparison._
import org.orbeon.saxon.expr.parser._
import org.orbeon.saxon.expr.sort._
import org.orbeon.saxon.lib.StringCollator
import org.orbeon.saxon.model._
import org.orbeon.saxon.trace.ExpressionPresenter
import org.orbeon.saxon.trans.XPathException
import org.orbeon.saxon.value.{AtomicValue, BooleanValue, Cardinality, SequenceType}

import scala.beans.BeanProperty


object ValueComparison {

  def compare(
    v0         : AtomicValue,
    op         : Int,
    v1         : AtomicValue,
    comparer   : AtomicComparer,
    checkTypes : Boolean
  ): Boolean = {
    if (checkTypes &&
      ! Type.isGuaranteedComparable(v0.getPrimitiveType,
        v1.getPrimitiveType,
        Token.isOrderedOperator(op))) {
      val e2 = new XPathException(
        "Cannot compare " + Type.displayTypeName(v0) + " to " +
          Type.displayTypeName(v1))
      e2.setErrorCode("XPTY0004")
      e2.setIsTypeError(true)
      throw e2
    }
    if (v0.isNaN || v1.isNaN)
      return op == Token.FNE

    try op match {
      case Token.FEQ =>   comparer.comparesEqual(v0, v1)
      case Token.FNE => ! comparer.comparesEqual(v0, v1)
      case Token.FGT =>   comparer.compareAtomicValues(v0, v1) > 0
      case Token.FLT =>   comparer.compareAtomicValues(v0, v1) < 0
      case Token.FGE =>   comparer.compareAtomicValues(v0, v1) >= 0
      case Token.FLE =>   comparer.compareAtomicValues(v0, v1) <= 0
      case _         => throw new UnsupportedOperationException("Unknown operator " + op)
    } catch {
      case err: ComparisonException =>
        throw err.getCause
      case err: ClassCastException =>
        err.printStackTrace()
        val e2 = new XPathException("Cannot compare " + Type.displayTypeName(v0) + " to " + Type.displayTypeName(v1))
        e2.setErrorCode("XPTY0004")
        e2.setIsTypeError(true)
        throw e2
    }
  }
}

class ValueComparison(p1: Expression, op: Int, p2: Expression)
  extends BinaryExpression(p1, op, p2)
    with ComparisonExpression
    with Negatable {

  private var comparer: AtomicComparer = _

  @BeanProperty
  var resultWhenEmpty: BooleanValue = null

  private var needsRuntimeCheck: Boolean = _

  override def getExpressionName: String = "ValueComparison"

  def setAtomicComparer(comparer: AtomicComparer): Unit =
    this.comparer = comparer

  def getAtomicComparer: AtomicComparer = comparer

  def getSingletonOperator: Int = op

  def convertsUntypedToOther(): Boolean =
    comparer.isInstanceOf[UntypedNumericComparer]

  def needsRuntimeComparabilityCheck(): Boolean = needsRuntimeCheck

  override def typeCheck(visitor: ExpressionVisitor, contextInfo: ContextItemStaticInfo): Expression = {
    resetLocalStaticProperties()
    getLhs.typeCheck(visitor, contextInfo)
    getRhs.typeCheck(visitor, contextInfo)

    val config = visitor.getConfiguration
    val env    = visitor.getStaticContext

    if (Literal.isEmptySequence(getLhsExpression))
      return {
        if (resultWhenEmpty == null)
          getLhsExpression
        else
          Literal.makeLiteral(resultWhenEmpty, this)
      }

    if (Literal.isEmptySequence(getRhsExpression))
      return {
        if (resultWhenEmpty == null)
          getRhsExpression
        else
          Literal.makeLiteral(resultWhenEmpty, this)
      }

    if (comparer.isInstanceOf[UntypedNumericComparer])
      return this

    val optionalAtomic        = SequenceType.OPTIONAL_ATOMIC
    val tc = config.getTypeChecker(false)
    val role0 = new RoleDiagnostic(RoleDiagnostic.BINARY_EXPR, Token.tokens(op), 0)
    this.setLhsExpression(tc.staticTypeCheck(getLhsExpression, optionalAtomic, role0, visitor))
    val role1 = new RoleDiagnostic(RoleDiagnostic.BINARY_EXPR, Token.tokens(op), 1)
    this.setRhsExpression(tc.staticTypeCheck(getRhsExpression, optionalAtomic, role1, visitor))
    val t0 = getLhsExpression.getItemType.getAtomizedItemType
    val t1 = getRhsExpression.getItemType.getAtomizedItemType
    if (t0.getUType.union(t1.getUType).overlaps(UType.EXTENSION)) {
      val err = new XPathException("Cannot perform comparisons involving external objects")
      err.setIsTypeError(true)
      err.setErrorCode("XPTY0004")
      err.setLocation(getLocation)
      throw err
    }
    var p0 = t0.getPrimitiveItemType.asInstanceOf[BuiltInAtomicType]
    if (p0 == BuiltInAtomicType.UNTYPED_ATOMIC)
      p0 = BuiltInAtomicType.STRING
    var p1 = t1.getPrimitiveItemType.asInstanceOf[BuiltInAtomicType]
    if (p1 == BuiltInAtomicType.UNTYPED_ATOMIC)
      p1 = BuiltInAtomicType.STRING
    needsRuntimeCheck = p0 == BuiltInAtomicType.ANY_ATOMIC || p1 == BuiltInAtomicType.ANY_ATOMIC
    if (! needsRuntimeCheck && ! Type.isPossiblyComparable(p0, p1, Token.isOrderedOperator(op))) {
      val opt0 = Cardinality.allowsZero(getLhsExpression.getCardinality)
      val opt1 = Cardinality.allowsZero(getRhsExpression.getCardinality)
      if (opt0 || opt1) {
        var which: String = null
        if (opt0)
          which = "the first operand is"
        if (opt1)
          which = "the second operand is"
        if (opt0 && opt1)
          which = "one or both operands are"
        visitor.getStaticContext.issueWarning(
          "Comparison of " + t0.toString + (if (opt0) "?" else "") +
            " to " +
            t1.toString +
            (if (opt1) "?" else "") +
            " will fail unless " +
            which +
            " empty",
          getLocation)
        needsRuntimeCheck = true
      } else {
        val message = "In {" + toShortString + "}: cannot compare " + t0.toString + " to " + t1.toString
        val err     = new XPathException(message)
        err.setIsTypeError(true)
        err.setErrorCode("XPTY0004")
        err.setLocation(getLocation)
        throw err
      }
    }
    if (!(op == Token.FEQ || op == Token.FNE)) {
      mustBeOrdered(t0, p0)
      mustBeOrdered(t1, p1)
    }
    if (comparer == null) {
      val defaultCollationName = env.getDefaultCollationName
      var comp = config.getCollation(defaultCollationName)
      if (comp == null)
        comp = CodepointCollator.getInstance
      comparer = GenericAtomicComparer.makeAtomicComparer(
        p0,
        p1,
        comp,
        env.getConfiguration.getConversionContext
      )
    }
    this
  }

  private def mustBeOrdered(t1: PlainType, p1: BuiltInAtomicType): Unit = {
    if (!p1.isOrdered(true)) {
      val err = new XPathException("Type " + t1.toString + " is not an ordered type")
      err.setErrorCode("XPTY0004")
      err.setIsTypeError(true)
      err.setLocation(getLocation)
      throw err
    }
  }

override  def optimize(visitor: ExpressionVisitor, contextInfo: ContextItemStaticInfo): Expression = {
    getLhs.optimize(visitor, contextInfo)
    getRhs.optimize(visitor, contextInfo)
    visitor
      .obtainOptimizer()
      .optimizeValueComparison(this, visitor, contextInfo)
  }

  def isNegatable(th: TypeHierarchy): Boolean =
    isNeverNaN(getLhsExpression, th) && isNeverNaN(getRhsExpression, th)

  private def isNeverNaN(exp: Expression, th: TypeHierarchy): Boolean =
    th.relationship(exp.getItemType, BuiltInAtomicType.DOUBLE) ==
      Affinity.DISJOINT &&
      th.relationship(exp.getItemType, BuiltInAtomicType.FLOAT) ==
        Affinity.DISJOINT

  def negate(): Expression = {
    val vc = new ValueComparison(getLhsExpression, Token.negate(op), getRhsExpression)
    vc.comparer = comparer
    vc.resultWhenEmpty =
      if (resultWhenEmpty == null || resultWhenEmpty == BooleanValue.FALSE)
        BooleanValue.TRUE
      else
        BooleanValue.FALSE
    ExpressionTool.copyLocationInfo(this, vc)
    vc
  }

  def copy(rebindings: RebindingMap): Expression = {
    val vc = new ValueComparison(
      getLhsExpression.copy(rebindings),
      op,
      getRhsExpression.copy(rebindings))
    ExpressionTool.copyLocationInfo(this, vc)
    vc.comparer = comparer
    vc.resultWhenEmpty = resultWhenEmpty
    vc.needsRuntimeCheck = needsRuntimeCheck
    vc
  }

  override def effectiveBooleanValue(context: XPathContext): Boolean =
    try {
      val v0 = getLhsExpression.evaluateItem(context).asInstanceOf[AtomicValue]
      if (v0 == null)
        return resultWhenEmpty == BooleanValue.TRUE
      val v1 = getRhsExpression.evaluateItem(context).asInstanceOf[AtomicValue]
      if (v1 == null)
        return resultWhenEmpty == BooleanValue.TRUE
      compare(
        v0,
        op,
        v1,
        comparer.provideContext(context),
        needsRuntimeCheck
      )
    } catch {
      case e: XPathException =>
        e.maybeSetLocation(getLocation)
        e.maybeSetContext(context)
        throw e
    }

  override def evaluateItem(context: XPathContext): BooleanValue =
    try {
      val v0 = getLhsExpression.evaluateItem(context).asInstanceOf[AtomicValue]
      if (v0 == null)
        return resultWhenEmpty
      val v1 = getRhsExpression.evaluateItem(context).asInstanceOf[AtomicValue]
      if (v1 == null)
        return resultWhenEmpty
      BooleanValue.get(
        compare(
          v0,
          op,
          v1,
          comparer.provideContext(context),
          needsRuntimeCheck
        )
      )
    } catch {
      case e: XPathException =>
        e.maybeSetLocation(getLocation)
        e.maybeSetContext(context)
        throw e
    }

  override def equals(other: Any): Boolean =
    other.isInstanceOf[ValueComparison] && super.equals(other) && comparer == other.asInstanceOf[ValueComparison].comparer

  def getItemType: ItemType = BuiltInAtomicType.BOOLEAN

  override def getStaticUType(contextItemType: UType): UType = UType.BOOLEAN

  override def computeCardinality(): Int =
    if (resultWhenEmpty != null)
      StaticProperty.EXACTLY_ONE
    else
      super.computeCardinality()

  override def tag(): String = "vc"

  override def explainExtraAttributes(
    out: ExpressionPresenter
  ): Unit = {
    if (resultWhenEmpty != null) {
      out.emitAttribute(
        "onEmpty",
        if (resultWhenEmpty.getBooleanValue) "1" else "0"
      )
    }
    out.emitAttribute("comp", comparer.save())
  }

}
