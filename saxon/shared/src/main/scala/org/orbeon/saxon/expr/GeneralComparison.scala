package org.orbeon.saxon.expr


import java.util.{ArrayList, List}

import org.orbeon.saxon.expr.parser._
import org.orbeon.saxon.expr.sort.{AtomicComparer, CodepointCollator, GenericAtomicComparer, UntypedNumericComparer}
import org.orbeon.saxon.functions.{Minimax, SystemFunction}
import org.orbeon.saxon.model._
import org.orbeon.saxon.om.{GroundedValue, NamespaceResolver, Sequence, SequenceIterator}
import org.orbeon.saxon.trace.ExpressionPresenter
import org.orbeon.saxon.trans.XPathException
import org.orbeon.saxon.utils.Configuration
import org.orbeon.saxon.value._

import scala.jdk.CollectionConverters._


object GeneralComparison {

  object ComparisonCardinality extends Enumeration {

    val ONE_TO_ONE   : ComparisonCardinality = new ComparisonCardinality
    val MANY_TO_ONE  : ComparisonCardinality = new ComparisonCardinality
    val MANY_TO_MANY : ComparisonCardinality = new ComparisonCardinality

    class ComparisonCardinality extends Val

    implicit def convertValue(v: Value): ComparisonCardinality =
      v.asInstanceOf[ComparisonCardinality]

  }

  private def makeMinOrMax(exp: Expression, function: String): Expression =
    if (Cardinality.allowsMany(exp.getCardinality)) {
      val fn = SystemFunction.makeCall(function, exp.getRetainedStaticContext, exp)
      assert(fn != null)
      fn.asInstanceOf[SystemFunctionCall]
        .getTargetFunction
        .asInstanceOf[Minimax]
        .setIgnoreNaN(true)
      fn
    } else {
      exp
    }

  def compare(
    a0         : AtomicValue,
    operator   : Int,
    a1         : AtomicValue,
    comparer   : AtomicComparer,
    checkTypes : Boolean,
    context    : XPathContext,
    nsResolver : NamespaceResolver
  ): Boolean = {

    var atomicVal0 = a0
    var atomicVal1 = a1
    var checkTyps = checkTypes

    val u0 = a0.isInstanceOf[UntypedAtomicValue]
    val u1 = atomicVal1.isInstanceOf[UntypedAtomicValue]

    if (u0 != u1) {
      val rules = context.getConfiguration.getConversionRules
      if (u0) {
        atomicVal1 match {
          case numericValue: NumericValue =>
            return UntypedNumericComparer.quickCompare(
              atomicVal0.asInstanceOf[UntypedAtomicValue],
              numericValue,
              operator,
              rules)
          case _: StringValue =>
          case _ =>
            var sc = atomicVal1.getItemType.getPrimitiveItemType.getStringConverter(rules)
            if (atomicVal1.isInstanceOf[QualifiedNameValue])
              sc = sc.setNamespaceResolver(nsResolver).asInstanceOf[StringConverter]
            atomicVal0 = sc.convertString(atomicVal0.getStringValueCS).asAtomic
        }
      } else {
        atomicVal0 match {
          case numericValue: NumericValue =>
            return UntypedNumericComparer.quickCompare(
              atomicVal1.asInstanceOf[UntypedAtomicValue],
              numericValue,
              Token.inverse(operator),
              rules)
          case _: StringValue =>
          case _ =>
            var sc = atomicVal0.getItemType.getPrimitiveItemType.getStringConverter(rules)
            if (atomicVal0.isInstanceOf[QualifiedNameValue])
              sc = sc.setNamespaceResolver(nsResolver).asInstanceOf[StringConverter]
            atomicVal1 = sc.convertString(atomicVal1.getStringValueCS).asAtomic
        }
      }
      checkTyps = false
    }
    ValueComparison.compare(atomicVal0, operator, atomicVal1, comparer, checkTypes)
  }

  def getCorrespondingSingletonOperator(op: Int): Int = op match {
    case Token.EQUALS => Token.FEQ
    case Token.GE     => Token.FGE
    case Token.NE     => Token.FNE
    case Token.LT     => Token.FLT
    case Token.GT     => Token.FGT
    case Token.LE     => Token.FLE
    case _            => op
  }
}

abstract class GeneralComparison(p0: Expression, op: Int, p1: Expression)
  extends BinaryExpression(p0, op, p1)
    with ComparisonExpression
    with Callable {

  import GeneralComparison.ComparisonCardinality._

  var singletonOperator: Int = GeneralComparison.getCorrespondingSingletonOperator(op)
  var comparer: AtomicComparer = _
  var needsRuntimeCheck: Boolean = true
  var comparisonCardinality: ComparisonCardinality = MANY_TO_MANY
  var doneWarnings: Boolean = false

  def getNeedsRuntimeCheck: Boolean = needsRuntimeCheck

  def setNeedsRuntimeCheck(runTimeCheck: Boolean): Unit = needsRuntimeCheck = runTimeCheck

  def getComparisonCardinality: ComparisonCardinality = comparisonCardinality

  def setComparisonCardinality(card: ComparisonCardinality): Unit =
    comparisonCardinality = card

  def setAtomicComparer(comparer: AtomicComparer): Unit =
    this.comparer = comparer

  override def getExpressionName: String = "GeneralComparison"

  def getNamespaceResolver: NamespaceResolver = getRetainedStaticContext

  def getAtomicComparer: AtomicComparer = comparer

  def getSingletonOperator: Int = singletonOperator

  def convertsUntypedToOther(): Boolean = true

  override def computeCardinality(): Int = StaticProperty.EXACTLY_ONE

  override def typeCheck(
    visitor     : ExpressionVisitor,
    contextInfo : ContextItemStaticInfo
  ): Expression = {

    val config = visitor.getConfiguration
    val th     = config.getTypeHierarchy

    val oldOp0 = getLhsExpression
    val oldOp1 = getRhsExpression

    getLhs.typeCheck(visitor, contextInfo)
    getRhs.typeCheck(visitor, contextInfo)

    if (Literal.isEmptySequence(getLhsExpression) || Literal.isEmptySequence(getRhsExpression))
      return Literal.makeLiteral(BooleanValue.FALSE, this)

    this.setLhsExpression(getLhsExpression.unordered(retainAllNodes = false, forStreaming = false))
    this.setRhsExpression(getRhsExpression.unordered(retainAllNodes = false, forStreaming = false))

    val atomicType = SequenceType.ATOMIC_SEQUENCE
    val tc = config.getTypeChecker(false)

    val role0 = new RoleDiagnostic(RoleDiagnostic.BINARY_EXPR, Token.tokens(op), 0)
    this.setLhsExpression(tc.staticTypeCheck(getLhsExpression, atomicType, role0, visitor))

    val role1 = new RoleDiagnostic(RoleDiagnostic.BINARY_EXPR, Token.tokens(op), 1)
    this.setRhsExpression(tc.staticTypeCheck(getRhsExpression, atomicType, role1, visitor))

    if (getLhsExpression != oldOp0)
      adoptChildExpression(getLhsExpression)
    if (getRhsExpression != oldOp1)
      adoptChildExpression(getRhsExpression)

    val t0 = getLhsExpression.getItemType
    val t1 = getRhsExpression.getItemType
    if ((t0 eq ErrorType) || (t1 eq ErrorType))
      return Literal.makeLiteral(BooleanValue.FALSE, this)

    if (t0.getUType.union(t1.getUType).overlaps(UType.EXTENSION)) {
      val err = new XPathException(
        "Cannot perform comparisons involving external objects")
      err.setIsTypeError(true)
      err.setErrorCode("XPTY0004")
      err.setLocator(getLocation)
      throw err
    }

    val pt0 = t0.getPrimitiveItemType.asInstanceOf[BuiltInAtomicType]
    val pt1 = t1.getPrimitiveItemType.asInstanceOf[BuiltInAtomicType]

    val c0 = getLhsExpression.getCardinality
    val c1 = getRhsExpression.getCardinality

    if (c0 == StaticProperty.EMPTY || c1 == StaticProperty.EMPTY)
      return Literal.makeLiteral(BooleanValue.FALSE, this)

    if (t0 == BuiltInAtomicType.ANY_ATOMIC || t0 == BuiltInAtomicType.UNTYPED_ATOMIC ||
      t1 == BuiltInAtomicType.ANY_ATOMIC ||
      t1 == BuiltInAtomicType.UNTYPED_ATOMIC) {} else {
      if (! Type.isPossiblyComparable(
        pt0,
        pt1,
        Token.isOrderedOperator(singletonOperator))) {
        val message: String = "In {" + toShortString + "}: cannot compare " + t0 +
          " to " +
          t1
        if (Cardinality.allowsZero(c0) || Cardinality.allowsZero(c1)) {
          if (!doneWarnings) {
            doneWarnings = true
            var which: String = "one"
            if (Cardinality.allowsZero(c0) && !Cardinality.allowsZero(c1)) {
              which = "the first"
            } else if (Cardinality.allowsZero(c1) && !Cardinality.allowsZero(
              c0)) {
              which = "the second"
            }
            visitor.getStaticContext.issueWarning(
              message + ". The comparison can succeed only if " + which +
                " operand is empty, and in that case will always be false",
              getLocation)
          }
        } else {
          val err = new XPathException(message)
          err.setErrorCode("XPTY0004")
          err.setIsTypeError(true)
          err.setLocator(getLocation)
          throw err
        }
      }
    }
    needsRuntimeCheck = ! Type.isGuaranteedGenerallyComparable(
      pt0,
      pt1,
      Token.isOrderedOperator(singletonOperator))
    if (!Cardinality.allowsMany(c0) && !Cardinality.allowsMany(c1) &&
      t0 != BuiltInAtomicType.ANY_ATOMIC &&
      t1 != BuiltInAtomicType.ANY_ATOMIC) {
      var e0 = getLhsExpression
      var e1 = getRhsExpression

      if (t0 == BuiltInAtomicType.UNTYPED_ATOMIC) {
        if (t1 == BuiltInAtomicType.UNTYPED_ATOMIC) {
          e0 = new CastExpression(getLhsExpression,
            BuiltInAtomicType.STRING,
            Cardinality.allowsZero(c0))
          adoptChildExpression(e0)
          e1 = new CastExpression(getRhsExpression,
            BuiltInAtomicType.STRING,
            Cardinality.allowsZero(c1))
          adoptChildExpression(e1)
        } else if (NumericType.isNumericType(t1)) {
          val vun = makeCompareUntypedToNumeric(getLhsExpression,
            getRhsExpression,
            singletonOperator)
          return vun.typeCheck(visitor, contextInfo)
        } else {
          e0 = new CastExpression(getLhsExpression,
            pt1,
            Cardinality.allowsZero(c0))
          adoptChildExpression(e0)
        }
      } else if (t1 == BuiltInAtomicType.UNTYPED_ATOMIC) {
        if (NumericType.isNumericType(t0)) {
          val vun: Expression = makeCompareUntypedToNumeric(
            getRhsExpression,
            getLhsExpression,
            Token.inverse(singletonOperator))
          return vun.typeCheck(visitor, contextInfo)
        } else {
          e1 = new CastExpression(getRhsExpression,
            pt0,
            Cardinality.allowsZero(c1))
          adoptChildExpression(e1)
        }
      }
      val vc: ValueComparison = new ValueComparison(e0, singletonOperator, e1)
      vc.setAtomicComparer(comparer)
      vc.setResultWhenEmpty(BooleanValue.FALSE)
      ExpressionTool.copyLocationInfo(this, vc)
      Optimizer.trace(config,
        "Replaced general comparison by value comparison",
        vc)
      return vc.typeCheck(visitor, contextInfo)
    }
    val env = visitor.getStaticContext
    if (comparer == null) {
      val defaultCollationName = env.getDefaultCollationName
      var collation = config.getCollation(defaultCollationName)
      if (collation == null)
        collation = CodepointCollator.getInstance
      comparer = GenericAtomicComparer.makeAtomicComparer(
        pt0,
        pt1,
        collation,
        config.getConversionContext)
    }
    if (getLhsExpression.isInstanceOf[Literal] && getRhsExpression.isInstanceOf[Literal])
      Literal.makeLiteral(evaluateItem(env.makeEarlyEvaluationContext()), this)
    else
      this
  }

  private def makeCompareUntypedToNumeric(lhs: Expression,
                                          rhs: Expression,
                                          operator: Int): Expression = {
    val vc: ValueComparison = new ValueComparison(lhs, operator, rhs)
    vc.setAtomicComparer(new UntypedNumericComparer())
    ExpressionTool.copyLocationInfo(this, vc)
    Optimizer.trace(
      getConfiguration,
      "Replaced general comparison by untyped-numeric value comparison",
      vc)
    vc
  }

  override def getIntrinsicDependencies: Int = {
    val th = getConfiguration.getTypeHierarchy
    if (mayInvolveCastToQName(th, getLhsExpression, getRhsExpression) ||
      mayInvolveCastToQName(th, getRhsExpression, getLhsExpression)) {
      StaticProperty.DEPENDS_ON_STATIC_CONTEXT
    } else {
      0
    }
  }

  private def mayInvolveCastToQName(th: TypeHierarchy,
                                    e1: Expression,
                                    e2: Expression): Boolean = {
    val s1: SimpleType =
      e1.getItemType.getAtomizedItemType.asInstanceOf[SimpleType]
    (s1 == BuiltInAtomicType.ANY_ATOMIC || s1.isNamespaceSensitive) &&
      th.relationship(e2.getItemType.getAtomizedItemType,
        BuiltInAtomicType.UNTYPED_ATOMIC) !=
        Affinity.DISJOINT &&
      (e2.getSpecialProperties & StaticProperty.NOT_UNTYPED_ATOMIC) ==
        0
  }

  override def optimize(
    visitor     : ExpressionVisitor,
    contextInfo : ContextItemStaticInfo
  ): Expression = {

    val th = visitor.getConfiguration.getTypeHierarchy
    val env = visitor.getStaticContext

    getLhs.optimize(visitor, contextInfo)
    getRhs.optimize(visitor, contextInfo)

    if (Literal.isEmptySequence(getLhsExpression) || Literal.isEmptySequence(getRhsExpression))
      return Literal.makeLiteral(BooleanValue.FALSE, this)

    this.setLhsExpression(getLhsExpression.unordered(retainAllNodes = false, forStreaming = false))
    this.setRhsExpression(getRhsExpression.unordered(retainAllNodes = false, forStreaming = false))

    if (getLhsExpression.isInstanceOf[Literal] && getRhsExpression.isInstanceOf[Literal])
      return Literal.makeLiteral(
        evaluateItem(visitor.getStaticContext.makeEarlyEvaluationContext())
          .materialize,
        this)

    val t0 = getLhsExpression.getItemType
    val t1 = getRhsExpression.getItemType
    val c0 = getLhsExpression.getCardinality
    val c1 = getRhsExpression.getCardinality

    val many0 = Cardinality.allowsMany(c0)
    val many1 = Cardinality.allowsMany(c1)

    if (many0) {
      comparisonCardinality =
        if (many1) MANY_TO_MANY
        else MANY_TO_ONE
    } else {
      if (many1) {
        val mc = getInverseComparison
        mc.comparisonCardinality = MANY_TO_ONE
        ExpressionTool.copyLocationInfo(this, mc)
        mc.comparer = comparer
        mc.needsRuntimeCheck = needsRuntimeCheck
        return mc.optimize(visitor, contextInfo)
      } else {
        comparisonCardinality = ONE_TO_ONE
      }
    }
    if (op == Token.EQUALS) {
      getLhsExpression match {
        case expression1: RangeExpression =>
          val min = expression1.getLhsExpression
          val max = expression1.getRhsExpression
          val ir = new IntegerRangeTest(getRhsExpression, min, max)
          ExpressionTool.copyLocationInfo(this, ir)
          return ir
        case _ =>
      }
      getRhsExpression match {
        case expression: RangeExpression =>
          val min = expression.getLhsExpression
          val max = expression.getRhsExpression
          val ir = new IntegerRangeTest(getLhsExpression, min, max)
          ExpressionTool.copyLocationInfo(this, ir)
          return ir
        case _ =>
      }
      getLhsExpression match {
        case literal1: Literal =>
          literal1.getValue match {
            case range: IntegerRange =>
              val min = range.getStart
              val max = range.getEnd
              val ir = new IntegerRangeTest(
                getRhsExpression,
                Literal.makeLiteral(Int64Value.makeIntegerValue(min), this),
                Literal.makeLiteral(Int64Value.makeIntegerValue(max), this))
              ExpressionTool.copyLocationInfo(this, ir)
              return ir
            case _ =>
          }
        case _ =>
      }
      getRhsExpression match {
        case literal: Literal =>
          literal.getValue match {
            case range: IntegerRange =>
              val min: Long = range.getStart
              val max: Long = range.getEnd
              val ir: IntegerRangeTest = new IntegerRangeTest(
                getLhsExpression,
                Literal.makeLiteral(Int64Value.makeIntegerValue(min), this),
                Literal.makeLiteral(Int64Value.makeIntegerValue(max), this))
              ExpressionTool.copyLocationInfo(this, ir)
              return ir
            case _ =>
          }
        case _ =>
      }
    }
    if (op != Token.EQUALS && op != Token.NE &&
      (comparisonCardinality == MANY_TO_MANY ||
        comparisonCardinality == MANY_TO_ONE &&
          manyOperandIsLiftable()) &&
      (NumericType.isNumericType(t0) || NumericType.isNumericType(t1))) {
      var vc: ValueComparison = null
      op match {
        case Token.LT | Token.LE =>
          vc = new ValueComparison(GeneralComparison.makeMinOrMax(getLhsExpression, "min"),
            singletonOperator,
            GeneralComparison.makeMinOrMax(getRhsExpression, "max"))
          vc.setResultWhenEmpty(BooleanValue.FALSE)
          vc.setAtomicComparer(comparer)
        case Token.GT | Token.GE =>
          vc = new ValueComparison(GeneralComparison.makeMinOrMax(getLhsExpression, "max"),
            singletonOperator,
            GeneralComparison.makeMinOrMax(getRhsExpression, "min"))
          vc.setResultWhenEmpty(BooleanValue.FALSE)
          vc.setAtomicComparer(comparer)
        case _ =>
          throw new UnsupportedOperationException("Unknown operator " + op)
      }
      ExpressionTool.copyLocationInfo(this, vc)
      vc.setRetainedStaticContext(getRetainedStaticContext)
      return vc.typeCheck(visitor, contextInfo)
    }
    if (getLhsExpression.isInstanceOf[Literal] && getRhsExpression.isInstanceOf[Literal])
      Literal.makeLiteral(evaluateItem(env.makeEarlyEvaluationContext()), this)
    else
      visitor
        .obtainOptimizer()
        .optimizeGeneralComparison(visitor, this, backwardsCompatible = false, contextInfo)
  }

  private def manyOperandIsLiftable(): Boolean = {
    getParentExpression match {
      case _: ContextSwitchingExpression if getParentExpression
        .asInstanceOf[ContextSwitchingExpression]
        .getActionExpression eq this =>
        for (o <- operands.asScala
             if Cardinality.allowsMany(o.getChildExpression.getCardinality)
             if ExpressionTool.dependsOnFocus(o.getChildExpression)) {
          return false
        }
        true
      case _ =>
        false
    }
  }

  override def evaluateItem(context: XPathContext): BooleanValue =
    comparisonCardinality match {
      case ONE_TO_ONE =>
        val value0 = getLhsExpression.evaluateItem(context).asInstanceOf[AtomicValue]
        val value1 = getRhsExpression.evaluateItem(context).asInstanceOf[AtomicValue]
        BooleanValue.get(evaluateOneToOne(value0, value1, context))
      case MANY_TO_ONE =>
        val iter0 = getLhsExpression.iterate(context)
        val value1 = getRhsExpression.evaluateItem(context).asInstanceOf[AtomicValue]
        BooleanValue.get(evaluateManyToOne(iter0, value1, context))
      case MANY_TO_MANY =>
        val iter1 = getLhsExpression.iterate(context)
        val iter2 = getRhsExpression.iterate(context)
        BooleanValue.get(evaluateManyToMany(iter1, iter2, context))
      case _ =>
        null
    }

  def call(context: XPathContext, arguments: Array[Sequence]): BooleanValue =
    comparisonCardinality match {
      case ONE_TO_ONE =>
        val value0: AtomicValue = arguments(0).head.asInstanceOf[AtomicValue]
        val value1: AtomicValue = arguments(1).head.asInstanceOf[AtomicValue]
        BooleanValue.get(evaluateOneToOne(value0, value1, context))
      case MANY_TO_ONE =>
        val iter0: SequenceIterator = arguments(0).iterate()
        val value1: AtomicValue = arguments(1).head.asInstanceOf[AtomicValue]
        BooleanValue.get(evaluateManyToOne(iter0, value1, context))
      case MANY_TO_MANY =>
        val iter1: SequenceIterator = arguments(0).iterate()
        val iter2: SequenceIterator = arguments(1).iterate()
        BooleanValue.get(evaluateManyToMany(iter1, iter2, context))
      case _ =>
        null
    }

  override def effectiveBooleanValue(context: XPathContext): Boolean =
    comparisonCardinality match {
      case ONE_TO_ONE =>
        val value0: AtomicValue =
          getLhsExpression.evaluateItem(context).asInstanceOf[AtomicValue]
        val value1: AtomicValue =
          getRhsExpression.evaluateItem(context).asInstanceOf[AtomicValue]
        evaluateOneToOne(value0, value1, context)
      case MANY_TO_ONE =>
        val iter0: SequenceIterator = getLhsExpression.iterate(context)
        val value1: AtomicValue =
          getRhsExpression.evaluateItem(context).asInstanceOf[AtomicValue]
        evaluateManyToOne(iter0, value1, context)
      case MANY_TO_MANY =>
        val iter1: SequenceIterator = getLhsExpression.iterate(context)
        val iter2: SequenceIterator = getRhsExpression.iterate(context)
        evaluateManyToMany(iter1, iter2, context)
      case _ =>
        false
    }

  private def evaluateOneToOne(value0: AtomicValue,
                               value1: AtomicValue,
                               context: XPathContext): Boolean =
    try
     ! (value0 == null || value1 == null) && GeneralComparison.compare(value0,
        singletonOperator,
        value1,
        comparer.provideContext(context),
        needsRuntimeCheck,
        context,
        getRetainedStaticContext
      )
    catch {
      case e: XPathException =>
        e.maybeSetLocation(getLocation)
        e.maybeSetContext(context)
        throw e
    }

  private def evaluateManyToOne(iter0: SequenceIterator,
                                value1: AtomicValue,
                                context: XPathContext): Boolean =
    try {
      if (value1 == null)
        return false
      var item0: AtomicValue = null
      val boundComparer = comparer.provideContext(context)
      while ({
        item0 = iter0.next().asInstanceOf[AtomicValue]
        item0
      } != null) if (GeneralComparison.compare(
        item0,
        singletonOperator,
        value1,
        boundComparer,
        needsRuntimeCheck,
        context,
        getRetainedStaticContext)) {
        iter0.close()
        return true
      }
      false
    } catch {
      case e: XPathException =>
        e.maybeSetLocation(getLocation)
        e.maybeSetContext(context)
        throw e
    }

  def evaluateManyToMany(
    iter0   : SequenceIterator,
    iter1   : SequenceIterator,
    context : XPathContext
  ): Boolean =
    try {
      var exhausted0 = false
      var exhausted1 = false

      val value0 = new ArrayList[AtomicValue]()
      val value1 = new ArrayList[AtomicValue]()

      val boundComparer = comparer.provideContext(context)

      while (true) {
        if (!exhausted0) {
          val item0 = iter0.next().asInstanceOf[AtomicValue]
          if (item0 == null) {
            if (exhausted1)
              return false
            exhausted0 = true
          } else {
            // ORBEON: Optimize loop with `while` as the non-local return showed in the JavaScript profiler.
            val it1 = value1.iterator
            while (it1.hasNext) {
              val item1 = it1.next()
              if (
                GeneralComparison.compare(
                  item0,
                  singletonOperator,
                  item1,
                  boundComparer,
                  needsRuntimeCheck,
                  context,
                  getRetainedStaticContext
                )
              ) {
                iter0.close()
                iter1.close()
                return true
              }
            }
            if (! exhausted1)
              value0.add(item0)
          }
        }
        if (! exhausted1) {
          val item1 = iter1.next().asInstanceOf[AtomicValue]
          if (item1 == null) {
            if (exhausted0)
              return false
            exhausted1 = true
          } else {
            // ORBEON: Optimize loop with `while` as the non-local return showed in the JavaScript profiler.
            val it0 = value0.iterator
            while (it0.hasNext) {
              val item0 = it0.next()
              if (
                GeneralComparison.compare(
                  item0,
                  singletonOperator,
                  item1,
                  boundComparer,
                  needsRuntimeCheck,
                  context,
                  getRetainedStaticContext
                )
              ) {
                iter0.close()
                iter1.close()
                return true
              }
            }
            if (! exhausted0)
              value1.add(item1)
          }
        }
      }
      false
    } catch {
      case e: XPathException =>
        e.maybeSetLocation(getLocation)
        e.maybeSetContext(context)
        throw e
    }

  def getItemType: ItemType = BuiltInAtomicType.BOOLEAN

  override def getStaticUType(contextItemType: UType): UType = UType.BOOLEAN

  def getInverseComparison: GeneralComparison = {
    val gc2 = new GeneralComparison20(
      getRhsExpression,
      Token.inverse(op),
      getLhsExpression)
    gc2.setRetainedStaticContext(getRetainedStaticContext)
    gc2
  }

  override def getStreamerName: String = "GeneralComparison"

  override def tag(): String = "gc"

  override def explainExtraAttributes(out: ExpressionPresenter): Unit = {
    val cc =
      comparisonCardinality match {
        case ONE_TO_ONE   => "1:1"
        case MANY_TO_ONE  => "N:1"
        case MANY_TO_MANY => "M:N"
        case _            => ""
      }
    out.emitAttribute("card", cc)
    out.emitAttribute("comp", comparer.save())
  }
}
