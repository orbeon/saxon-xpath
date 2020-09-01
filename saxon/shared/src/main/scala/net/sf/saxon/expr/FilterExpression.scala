package net.sf.saxon.expr

import net.sf.saxon.utils.Configuration

import net.sf.saxon.expr.instruct.Choose

import net.sf.saxon.expr.parser._

import net.sf.saxon.functions.LocalName_1

import net.sf.saxon.functions.PositionAndLast

import net.sf.saxon.functions.SystemFunction

import net.sf.saxon.functions.registry.VendorFunctionSetHE

import net.sf.saxon.lib.Feature

import net.sf.saxon.lib.NamespaceConstant

import net.sf.saxon.model._

import net.sf.saxon.om._

import net.sf.saxon.pattern._

import net.sf.saxon.trace.ExpressionPresenter

import net.sf.saxon.trans.XPathException

import net.sf.saxon.tree.iter.EmptyIterator

import net.sf.saxon.value._

import java.math.BigInteger

import FilterExpression._
import Expression._

import scala.beans.{BeanProperty, BooleanBeanProperty}

object FilterExpression {

  val FILTERED: Int = 10000

  val FILTER_PREDICATE: OperandRole = new OperandRole(
    OperandRole.USES_NEW_FOCUS | OperandRole.HIGHER_ORDER,
    OperandUsage.INSPECTION,
    SequenceType.ANY_SEQUENCE)

  private def forceToBoolean(in: Expression): Expression = {
    if (in.getItemType.getPrimitiveType == StandardNames.XS_BOOLEAN) {
      return in
    }
    SystemFunction.makeCall("boolean", in.getRetainedStaticContext, in)
  }

  private def tryToRewritePositionalFilterSupport(
                                                   start: Expression,
                                                   comparand: Expression,
                                                   operator: Int,
                                                   th: TypeHierarchy): Expression =
    if (th.isSubType(comparand.getItemType, BuiltInAtomicType.INTEGER)) {
      operator match {
        case Token.FEQ =>
          if (Literal.isConstantOne(comparand)) {
            FirstItemExpression.makeFirstItemExpression(start)
          } else if (comparand.isInstanceOf[Literal] &&
            comparand
              .asInstanceOf[Literal]
              .getValue
              .asInstanceOf[IntegerValue]
              .asBigInteger()
              .compareTo(BigInteger.ZERO) <=
              0) {
            Literal.makeEmptySequence()
          } else {
            new SubscriptExpression(start, comparand)
          }
        case Token.FLT => {
          val args: Array[Expression] = Array.ofDim[Expression](3)
          args(0) = start
          args(1) = Literal.makeLiteral(Int64Value.makeIntegerValue(1), start)
          if (Literal.isAtomic(comparand)) {
            val n: Long = comparand
              .asInstanceOf[Literal]
              .getValue
              .asInstanceOf[NumericValue]
              .longValue()
            args(2) =
              Literal.makeLiteral(Int64Value.makeIntegerValue(n - 1), start)
          } else {
            val decrement: ArithmeticExpression = new ArithmeticExpression(
              comparand,
              Token.MINUS,
              Literal.makeLiteral(Int64Value.makeIntegerValue(1), start))
            decrement.setCalculator(
              Calculator.getCalculator(StandardNames.XS_INTEGER,
                StandardNames.XS_INTEGER,
                Calculator.MINUS,
                mustResolve = true))
            args(2) = decrement
          }
          SystemFunction.makeCall("subsequence",
            start.getRetainedStaticContext,
            args.toIndexedSeq: _*)
        }
        case Token.FLE => {
          val args: Array[Expression] = Array.ofDim[Expression](3)
          args(0) = start
          args(1) = Literal.makeLiteral(Int64Value.makeIntegerValue(1), start)
          args(2) = comparand
          SystemFunction.makeCall("subsequence",
            start.getRetainedStaticContext,
            args.toIndexedSeq: _*)
        }
        case Token.FNE =>
          SystemFunction.makeCall("remove", start.getRetainedStaticContext, start, comparand)
        case Token.FGT => {
          val args: Array[Expression] = Array.ofDim[Expression](2)
          args(0) = start
          if (Literal.isAtomic(comparand)) {
            val n: Long = comparand
              .asInstanceOf[Literal]
              .getValue
              .asInstanceOf[NumericValue]
              .longValue()
            args(1) =
              Literal.makeLiteral(Int64Value.makeIntegerValue(n + 1), start)
          } else {
            args(1) = new ArithmeticExpression(
              comparand,
              Token.PLUS,
              Literal.makeLiteral(Int64Value.makeIntegerValue(1), start))
          }
          SystemFunction.makeCall("subsequence",
            start.getRetainedStaticContext,
            args.toIndexedSeq: _*)
        }
        case Token.FGE => {
          val args: Array[Expression] = Array.ofDim[Expression](3)
          args(0) = start
          args(1) = comparand
          SystemFunction.makeCall("subsequence",
            start.getRetainedStaticContext,
            args.toIndexedSeq: _*)
        }

        case _ => throw new IllegalArgumentException("operator")

      }
    } else {
      operator match {
        case Token.FEQ => new SubscriptExpression(start, comparand)
        case Token.FLT => {
          val let: LetExpression = new LetExpression()
          let.setRequiredType(
            SequenceType.makeSequenceType(comparand.getItemType,
              StaticProperty.ALLOWS_ONE))
          let.setVariableQName(
            new StructuredQName("pp",
              NamespaceConstant.SAXON,
              "pp" + let.hashCode))
          let.setSequence(comparand)
          val isWholeArg: LocalVariableReference = new LocalVariableReference(let)
          val arithArg: LocalVariableReference = new LocalVariableReference(let)
          val floorArg: LocalVariableReference = new LocalVariableReference(let)
          val isWhole: Expression = VendorFunctionSetHE.getInstance
            .makeFunction("is-whole-number", 1)
            .makeFunctionCall(isWholeArg)
          val minusOne: Expression = new ArithmeticExpression(
            arithArg,
            Token.MINUS,
            Literal.makeLiteral(Int64Value.makeIntegerValue(1), start))
          val floor: Expression = SystemFunction.makeCall(
            "floor",
            start.getRetainedStaticContext,
            floorArg)
          val choice: Expression =
            Choose.makeConditional(isWhole, minusOne, floor)
          val args: Array[Expression] = Array.ofDim[Expression](4)
          args(0) = start
          args(1) = Literal.makeLiteral(Int64Value.makeIntegerValue(1))
          args(2) = start
          args(3) = choice
          val subs: Expression = SystemFunction.makeCall(
            "subsequence",
            start.getRetainedStaticContext,
            args.toIndexedSeq: _*)
          let.setAction(subs)
          let
        }
        case Token.FLE => {
          val floor: Expression = SystemFunction.makeCall(
            "floor",
            start.getRetainedStaticContext,
            comparand)
          SystemFunction.makeCall(
            "subsequence",
            start.getRetainedStaticContext,
            Array(start,
              Literal.makeLiteral(Int64Value.makeIntegerValue(1), start),
              floor).toIndexedSeq: _*)
        }
        case Token.FNE => {
          val let: LetExpression = new LetExpression()
          ExpressionTool.copyLocationInfo(start, let)
          let.setRequiredType(
            SequenceType.makeSequenceType(comparand.getItemType,
              StaticProperty.ALLOWS_ONE))
          let.setVariableQName(
            new StructuredQName("pp",
              NamespaceConstant.SAXON,
              "pp" + let.hashCode))
          let.setSequence(comparand)
          val isWholeArg: LocalVariableReference = new LocalVariableReference(
            let)
          val castArg: LocalVariableReference = new LocalVariableReference(let)
          val isWhole: Expression = VendorFunctionSetHE.getInstance
            .makeFunction("is-whole-number", 1)
            .makeFunctionCall(isWholeArg)
          ExpressionTool.copyLocationInfo(start, isWhole)
          val cast: Expression =
            new CastExpression(castArg, BuiltInAtomicType.INTEGER, false)
          ExpressionTool.copyLocationInfo(start, cast)
          val choice: Expression = Choose.makeConditional(
            isWhole,
            cast,
            Literal.makeLiteral(Int64Value.makeIntegerValue(0), start))
          val rem: Expression = SystemFunction.makeCall(
            "remove",
            start.getRetainedStaticContext,
            start,
            choice)
          let.setAction(rem)
          let
        }
        case Token.FGT => {
          val let: LetExpression = new LetExpression()
          let.setRequiredType(
            SequenceType.makeSequenceType(comparand.getItemType,
              StaticProperty.ALLOWS_ONE))
          let.setVariableQName(
            new StructuredQName("pp",
              NamespaceConstant.SAXON,
              "pp" + let.hashCode))
          let.setSequence(comparand)
          val isWholeArg: LocalVariableReference = new LocalVariableReference(
            let)
          val arithArg: LocalVariableReference = new LocalVariableReference(
            let)
          val ceilingArg: LocalVariableReference = new LocalVariableReference(
            let)
          val isWhole: Expression = VendorFunctionSetHE.getInstance
            .makeFunction("is-whole-number", 1)
            .makeFunctionCall(isWholeArg)
          val plusOne: Expression = new ArithmeticExpression(
            arithArg,
            Token.PLUS,
            Literal.makeLiteral(Int64Value.makeIntegerValue(1), start))
          val ceiling: Expression = SystemFunction.makeCall(
            "ceiling",
            start.getRetainedStaticContext,
            ceilingArg)
          val choice: Expression =
            Choose.makeConditional(isWhole, plusOne, ceiling)
          val subs: Expression = SystemFunction.makeCall(
            "subsequence",
            start.getRetainedStaticContext,
            start,
            choice)
          let.setAction(subs)
          let
        }
        case Token.FGE => {
          val ceiling: Expression = SystemFunction.makeCall(
            "ceiling",
            start.getRetainedStaticContext,
            comparand)
          SystemFunction.makeCall("subsequence",
            start.getRetainedStaticContext,
            start,
            ceiling)
        }
        case _ => throw new IllegalArgumentException("operator")

      }
    }

  def isPositionalFilter(exp: Expression, th: TypeHierarchy): Boolean = {
    val `type`: ItemType = exp.getItemType
    if (`type` == BuiltInAtomicType.BOOLEAN) {
      isExplicitlyPositional(exp)
    }
    `type` == BuiltInAtomicType.ANY_ATOMIC || (`type` eq AnyItemType) ||
      `type` == BuiltInAtomicType.INTEGER ||
      `type` == NumericType.getInstance ||
      NumericType.isNumericType(`type`) ||
      isExplicitlyPositional(exp)
  }

  private def isExplicitlyPositional(exp: Expression): Boolean =
    (exp.getDependencies &
      (StaticProperty.DEPENDS_ON_POSITION | StaticProperty.DEPENDS_ON_LAST)) !=
      0

}

class FilterExpression(base: Expression, filter: Expression)
  extends BinaryExpression(base, Token.LSQB, filter)
    with ContextSwitchingExpression {

  var baseExp = base
  var filterExp = filter

  @BooleanBeanProperty
  var filterIsPositional: Boolean = _

  private var filterIsSingletonBoolean: Boolean = _

  private var filterIsIndependent: Boolean = _

  var doneReorderingPredicates: Boolean = false

  baseExp.setFiltered(true)

  override def getOperandRole(arg: Int): OperandRole =
    if (arg == 0) OperandRole.SAME_FOCUS_ACTION else FILTER_PREDICATE

  def getBase(): Expression = getLhsExpression

  def setBase(baseExp: Expression): Unit = {
    this.setLhsExpression(baseExp)
  }

  def getFilter(): Expression = getRhsExpression

  def setFilter(filter: Expression): Unit = {
    this.setRhsExpression(filter)
  }

  override def getExpressionName(): String = "filter"

  def getItemType(): ItemType = {
    if (getFilter.isInstanceOf[InstanceOfExpression] &&
      getFilter
        .asInstanceOf[InstanceOfExpression]
        .getBaseExpression
        .isInstanceOf[ContextItemExpression]) {
      getFilter.asInstanceOf[InstanceOfExpression].getRequiredItemType
    }
    getBase.getItemType
  }

  override def getStaticUType(contextItemType: UType): UType =
    getBase.getStaticUType(contextItemType)

  def getSelectExpression(): Expression = getBase

  def getActionExpression(): Expression = getFilter

  def isPositional(th: TypeHierarchy): Boolean =
    isPositionalFilter(getFilter, th)

  def isSimpleBooleanFilter(): Boolean = filterIsSingletonBoolean

  def isIndependentFilter(): Boolean = filterIsIndependent

  override def simplify(): Expression = {
    baseExp = getBase.simplify()
    filterExp = getFilter.simplify()
    if (Literal.isEmptySequence(getBase)) {
      getBase
    }
    if (getFilter.isInstanceOf[Literal] &&
      !(getFilter
        .asInstanceOf[Literal]
        .getValue
        .isInstanceOf[NumericValue])) {
      try if (getFilter.effectiveBooleanValue(
        new EarlyEvaluationContext(getConfiguration))) {
        getBase
      } else {
        Literal.makeEmptySequence()
      } catch {
        case e: XPathException => {
          e.maybeSetLocation(getLocation)
          throw e
        }

      }
    }
    if (getFilter.isCallOn(classOf[PositionAndLast.Last])) {
      filterExp = new IsLastExpression(true)
      adoptChildExpression(getFilter)
    }
    this
  }

  override def typeCheck(visitor: ExpressionVisitor,
                         contextInfo: ContextItemStaticInfo): Expression = {
    val config: Configuration = visitor.getConfiguration
    val th: TypeHierarchy = config.getTypeHierarchy
    getLhs.typeCheck(visitor, contextInfo)
    getBase.setFiltered(true)
    if (Literal.isEmptySequence(getBase)) {
      getBase
    }
    val baseItemType: ContextItemStaticInfo =
      config.makeContextItemStaticInfo(getSelectExpression.getItemType, maybeUndefined = false)
    baseItemType.setContextSettingExpression(getBase)
    getRhs.typeCheck(visitor, baseItemType)
    val filter2: Expression = ExpressionTool.unsortedIfHomogeneous(
      getFilter,
      visitor.isOptimizeForStreaming)
    if (filter2 != getFilter) {
      filterExp = filter2
    }
    if (Literal.isConstantOne(getFilter)) {
      val fie: Expression =
        FirstItemExpression.makeFirstItemExpression(getBase)
      ExpressionTool.copyLocationInfo(this, fie)
      return fie
    }
    filterIsPositional = isPositionalFilter(getFilter, th)
    filterIsSingletonBoolean = getFilter.getCardinality == StaticProperty.EXACTLY_ONE &&
      getFilter.getItemType == BuiltInAtomicType.BOOLEAN
    filterIsIndependent = (getFilter.getDependencies & StaticProperty.DEPENDS_ON_FOCUS) ==
      0
    ExpressionTool.resetStaticProperties(this)
    this
  }

  override def optimize(visitor: ExpressionVisitor,
                        contextItemType: ContextItemStaticInfo): Expression = {
    val config: Configuration = visitor.getConfiguration
    val opt: Optimizer = visitor.obtainOptimizer()
    val tracing: Boolean =
      config.getBooleanProperty(Feature.TRACE_OPTIMIZER_DECISIONS)
    val th: TypeHierarchy = config.getTypeHierarchy
    getLhs.optimize(visitor, contextItemType)
    getBase.setFiltered(true)
    val baseItemType: ContextItemStaticInfo =
      config.makeContextItemStaticInfo(getSelectExpression.getItemType, maybeUndefined = false)
    baseItemType.setContextSettingExpression(getBase)
    getRhs.optimize(visitor, baseItemType)
    val filter2: Expression = ExpressionTool.unsortedIfHomogeneous(
      getFilter,
      visitor.isOptimizeForStreaming)
    if (filter2 != getFilter) {
      filterExp = filter2
    }
    if (getFilter.isInstanceOf[IsLastExpression] &&
      getFilter.asInstanceOf[IsLastExpression].getCondition &&
      getBase.isInstanceOf[AxisExpression] &&
      getBase.asInstanceOf[AxisExpression].getAxis == AxisInfo.CHILD) {
      val test: NodeTest = getBase.asInstanceOf[AxisExpression].getNodeTest
      val fs: AxisExpression =
        new AxisExpression(AxisInfo.FOLLOWING_SIBLING, test)
      filterExp =
        SystemFunction.makeCall("empty", getRetainedStaticContext, fs)
      if (tracing) {
        Optimizer.trace(
          config,
          "Replaced [last()] predicate by test for following-sibling",
          this)
      }
    }
    if (getBase.isInstanceOf[AxisExpression] &&
      getBase
        .asInstanceOf[AxisExpression]
        .getNodeTest == NodeKindTest.ELEMENT &&
      getFilter.isInstanceOf[CompareToStringConstant] &&
      getFilter.asInstanceOf[CompareToStringConstant].getSingletonOperator ==
        Token.FEQ &&
      getFilter
        .asInstanceOf[CompareToStringConstant]
        .getLhsExpression
        .isCallOn(classOf[LocalName_1]) &&
      getFilter
        .asInstanceOf[CompareToStringConstant]
        .getLhsExpression
        .asInstanceOf[SystemFunctionCall]
        .getArg(0)
        .isInstanceOf[ContextItemExpression]) {
      val ax2: AxisExpression = new AxisExpression(
        getBase.asInstanceOf[AxisExpression].getAxis,
        new LocalNameTest(
          config.getNamePool,
          Type.ELEMENT,
          getFilter.asInstanceOf[CompareToStringConstant].getComparand)
      )
      ExpressionTool.copyLocationInfo(this, ax2)
      return ax2
    }
    val filterType: ItemType = getFilter.getItemType
    if (!th.isSubType(filterType, BuiltInAtomicType.BOOLEAN) &&
      th.relationship(filterType, NumericType.getInstance) ==
        Affinity.DISJOINT) {
      val f: Expression =
        SystemFunction.makeCall("boolean", getRetainedStaticContext, getFilter)
      filterExp = f.optimize(visitor, baseItemType)
    }
    if (getFilter.isInstanceOf[Literal] &&
      getFilter.asInstanceOf[Literal].getValue.isInstanceOf[BooleanValue]) {
      if (getFilter
        .asInstanceOf[Literal]
        .getValue
        .asInstanceOf[BooleanValue]
        .getBooleanValue) {
        if (tracing) {
          opt.trace("Redundant filter removed", getBase)
        }
        getBase
      } else {
        val result: Expression = Literal.makeEmptySequence()
        ExpressionTool.copyLocationInfo(this, result)
        if (tracing) {
          opt.trace(
            "Filter expression eliminated because predicate is always false",
            result)
        }
        return result
      }
    }
    filterIsPositional = isPositionalFilter(getFilter, th)
    filterIsSingletonBoolean = getFilter.getCardinality == StaticProperty.EXACTLY_ONE &&
      getFilter.getItemType == BuiltInAtomicType.BOOLEAN
    if (!filterIsPositional && !visitor.isOptimizeForStreaming) {
      val isIndexable: Int = opt.isIndexableFilter(getFilter)
      if (isIndexable != 0) {
        val contextIsDoc: Boolean = contextItemType != null &&
          contextItemType.getItemType != ErrorType.getInstance &&
          th.isSubType(contextItemType.getItemType, NodeKindTest.DOCUMENT)
        val f: Expression =
          opt.tryIndexedFilter(this, visitor, isIndexable > 0, contextIsDoc)
        if (f != this) {
          f.typeCheck(visitor, contextItemType)
            .optimize(visitor, contextItemType)
        }
      }
    }
    if (filterIsPositional && getFilter.isInstanceOf[BooleanExpression] &&
      getFilter.asInstanceOf[BooleanExpression].operator == Token.AND) {
      val bf: BooleanExpression = getFilter.asInstanceOf[BooleanExpression]
      if (isExplicitlyPositional(bf.getLhsExpression) && !isExplicitlyPositional(
        bf.getRhsExpression)) {
        val p0: Expression = forceToBoolean(bf.getLhsExpression)
        val p1: Expression = forceToBoolean(bf.getRhsExpression)
        val f1: FilterExpression = new FilterExpression(getBase, p0)
        ExpressionTool.copyLocationInfo(this, f1)
        val f2: FilterExpression = new FilterExpression(f1, p1)
        ExpressionTool.copyLocationInfo(this, f2)
        if (tracing) {
          opt.trace("Composite filter replaced by nested filter expressions",
            f2)
        }
        f2.optimize(visitor, contextItemType)
      }
      if (isExplicitlyPositional(bf.getRhsExpression) && !isExplicitlyPositional(
        bf.getLhsExpression)) {
        val p0: Expression = forceToBoolean(bf.getLhsExpression)
        val p1: Expression = forceToBoolean(bf.getRhsExpression)
        val f1: FilterExpression = new FilterExpression(getBase, p1)
        ExpressionTool.copyLocationInfo(this, f1)
        val f2: FilterExpression = new FilterExpression(f1, p0)
        ExpressionTool.copyLocationInfo(this, f2)
        if (tracing) {
          opt.trace("Composite filter replaced by nested filter expressions",
            f2)
        }
        f2.optimize(visitor, contextItemType)
      }
    }
    if (getFilter.isInstanceOf[IsLastExpression] &&
      getFilter.asInstanceOf[IsLastExpression].getCondition) {
      if (getBase.isInstanceOf[Literal]) {
        filterExp = Literal.makeLiteral(
          new Int64Value(getBase.asInstanceOf[Literal].getValue.getLength),
          this)
      } else {
        new LastItemExpression(getBase)
      }
    }
    val subsequence: Expression =
      tryToRewritePositionalFilter(visitor, tracing)
    if (subsequence != null) {
      if (tracing) {
        subsequence.setRetainedStaticContext(getRetainedStaticContext)
        opt.trace("Rewrote Filter Expression as:", subsequence)
      }
      ExpressionTool.copyLocationInfo(this, subsequence)
      subsequence
        .simplify()
        .typeCheck(visitor, contextItemType)
        .optimize(visitor, contextItemType)
    }
    if (!filterIsPositional && !doneReorderingPredicates &&
      !(getParentExpression.isInstanceOf[FilterExpression])) {
      val f2: FilterExpression =
        opt.reorderPredicates(this, visitor, contextItemType)
      if (f2 != this) {
        f2.doneReorderingPredicates = true
        return f2
      }
    }
    val sequence: Sequence = tryEarlyEvaluation(visitor)
    if (sequence != null) {
      val value: GroundedValue = sequence.materialize()
      Literal.makeLiteral(value, this)
    }
    this
  }

  override def getCost(): Double =
    Math.max(getLhsExpression.getCost + 5 * getRhsExpression.getCost, MAX_COST)

  override def getImplementationMethod(): Int = ITERATE_METHOD

  override def getIntegerBounds(): Array[IntegerValue] =
    getBase.getIntegerBounds

  private def tryEarlyEvaluation(visitor: ExpressionVisitor): Sequence = {
    try if (getBase.isInstanceOf[Literal] &&
      !ExpressionTool.refersToVariableOrFunction(getFilter) &&
      (getFilter.getDependencies & ~StaticProperty.DEPENDS_ON_FOCUS) ==
        0) {
      val context: XPathContext =
        visitor.getStaticContext.makeEarlyEvaluationContext()
      iterate(context).materialize()
    } catch {
      case e: Exception => return null

    }
    null
  }

  override def addToPathMap(
                             pathMap: PathMap,
                             pathMapNodeSet: PathMap.PathMapNodeSet): PathMap.PathMapNodeSet = {
    val target: PathMap.PathMapNodeSet =
      getBase.addToPathMap(pathMap, pathMapNodeSet)
    getFilter.addToPathMap(pathMap, target)
    target
  }

  private def tryToRewritePositionalFilter(visitor: ExpressionVisitor,
                                           tracing: Boolean): Expression = {
    val config: Configuration = visitor.getConfiguration
    val th: TypeHierarchy = config.getTypeHierarchy
    if (getFilter.isInstanceOf[Literal]) {
      val `val`: GroundedValue = getFilter.asInstanceOf[Literal].getValue
      if (`val`.isInstanceOf[NumericValue]) {
        var result: Expression = null
        val lvalue: Int = `val`.asInstanceOf[NumericValue].asSubscript()
        result =
          if (lvalue != -1)
            if (lvalue == 1)
              FirstItemExpression.makeFirstItemExpression(getBase)
            else new SubscriptExpression(getBase, getFilter)
          else Literal.makeEmptySequence()
        if (tracing) {
          Optimizer.trace(
            config,
            "Rewriting numeric filter expression with constant subscript",
            result)
        }
        return result
      } else {
        val result: Expression =
          if (ExpressionTool.effectiveBooleanValue(`val`.iterate())) getBase
          else Literal.makeEmptySequence()
        if (tracing) {
          Optimizer.trace(
            config,
            "Rewriting boolean filter expression with constant subscript",
            result)
        }
        return result
      }
    }
    if (NumericType.isNumericType(getFilter.getItemType) && !Cardinality
      .allowsMany(getFilter.getCardinality) &&
      (getFilter.getDependencies & StaticProperty.DEPENDS_ON_FOCUS) ==
        0) {
      val result: Expression = new SubscriptExpression(getBase, getFilter)
      if (tracing) {
        Optimizer.trace(
          config,
          "Rewriting numeric filter expression with focus-independent subscript",
          result)
      }
      return result
    }
    if (getFilter.isInstanceOf[ComparisonExpression]) {
      val lhs: Expression =
        getFilter.asInstanceOf[ComparisonExpression].getLhsExpression
      val rhs: Expression =
        getFilter.asInstanceOf[ComparisonExpression].getRhsExpression
      var operator: Int =
        getFilter.asInstanceOf[ComparisonExpression].getSingletonOperator
      var comparand: Expression = null
      if (lhs.isCallOn(classOf[PositionAndLast.Position]) && NumericType
        .isNumericType(rhs.getItemType)) {
        comparand = rhs
      } else if (rhs.isCallOn(classOf[PositionAndLast.Position]) && NumericType
        .isNumericType(lhs.getItemType)) {
        comparand = lhs
        operator = Token.inverse(operator)
      } else {
        return null
      }
      if (ExpressionTool.dependsOnFocus(comparand)) {
        return null
      }
      val card: Int = comparand.getCardinality
      if (Cardinality.allowsMany(card)) {
        return null
      }
      if (Cardinality.allowsZero(card)) {
        val let: LetExpression = new LetExpression()
        let.setRequiredType(
          SequenceType.makeSequenceType(comparand.getItemType, card))
        let.setVariableQName(
          new StructuredQName("pp",
            NamespaceConstant.SAXON,
            "pp" + let.hashCode))
        let.setSequence(comparand)
        comparand = new LocalVariableReference(let)
        val existsArg: LocalVariableReference = new LocalVariableReference(let)
        val exists: Expression = SystemFunction.makeCall(
          "exists",
          getRetainedStaticContext,
          existsArg)
        val rewrite: Expression =
          tryToRewritePositionalFilterSupport(getBase, comparand, operator, th)
        if (rewrite == null) {
          return this
        }
        val choice: Expression = Choose.makeConditional(exists, rewrite)
        let.setAction(choice)
        let
      } else {
        tryToRewritePositionalFilterSupport(getBase, comparand, operator, th)
      }
    } else if (getFilter.isInstanceOf[IntegerRangeTest]) {
      val `val`: Expression = getFilter.asInstanceOf[IntegerRangeTest].getValue
      if (!`val`.isCallOn(classOf[PositionAndLast])) {
        return null
      }
      var min: Expression = getFilter.asInstanceOf[IntegerRangeTest].getMin
      val max: Expression = getFilter.asInstanceOf[IntegerRangeTest].getMax
      if (ExpressionTool.dependsOnFocus(min)) {
        return null
      }
      if (ExpressionTool.dependsOnFocus(max)) {
        if (max.isCallOn(classOf[PositionAndLast.Last])) {
          val result: Expression = SystemFunction.makeCall(
            "subsequence",
            getRetainedStaticContext,
            getBase,
            min)
          if (tracing) {
            Optimizer.trace(
              config,
              "Rewriting numeric range filter expression using subsequence()",
              result)
          }
          return result
        } else {
          return null
        }
      }
      val let: LetExpression = new LetExpression()
      let.setRequiredType(SequenceType.SINGLE_INTEGER)
      let.setVariableQName(
        new StructuredQName("nn",
          NamespaceConstant.SAXON,
          "nn" + let.hashCode))
      let.setSequence(min)
      min = new LocalVariableReference(let)
      val min2: LocalVariableReference = new LocalVariableReference(let)
      val minMinusOne: Expression = new ArithmeticExpression(
        min2,
        Token.MINUS,
        Literal.makeLiteral(Int64Value.makeIntegerValue(1), this))
      val length: Expression =
        new ArithmeticExpression(max, Token.MINUS, minMinusOne)
      val subs: Expression = SystemFunction.makeCall("subsequence",
        getRetainedStaticContext,
        getBase,
        min,
        length)
      let.setAction(subs)
      if (tracing) {
        Optimizer.trace(
          config,
          "Rewriting numeric range filter expression using subsequence()",
          subs)
      }
      let
    } else {
      null
    }
  }

  override def unordered(retainAllNodes: Boolean,
                         forStreaming: Boolean): Expression = {
    if (!filterIsPositional) {
      baseExp = getBase.unordered(retainAllNodes, forStreaming)
    }
    this
  }

  private def promoteIndependentPredicates(
                                            bindings: Array[Binding],
                                            opt: Optimizer,
                                            th: TypeHierarchy): FilterExpression = {
    if (!ExpressionTool.dependsOnVariable(getBase, bindings)) {
      return this
    }
    if (isPositional(th)) {
      return this
    }
    if (getBase.isInstanceOf[FilterExpression]) {
      val fe: FilterExpression = getBase.asInstanceOf[FilterExpression]
      if (fe.isPositional(th)) {
        return this
      }
      if (!ExpressionTool.dependsOnVariable(fe.getFilter, bindings)) {
        return this
      }
      if (!ExpressionTool.dependsOnVariable(getFilter, bindings)) {
        val result: FilterExpression = new FilterExpression(
          new FilterExpression(fe.getBase, getFilter)
            .promoteIndependentPredicates(bindings, opt, th),
          fe.getFilter)
        opt.trace("Reordered filter predicates:", result)
        return result
      }
    }
    this
  }

  override def computeCardinality(): Int = {
    if (getFilter.isInstanceOf[Literal] &&
      getFilter.asInstanceOf[Literal].getValue.isInstanceOf[NumericValue]) {
      if (getFilter
        .asInstanceOf[Literal]
        .getValue
        .asInstanceOf[NumericValue]
        .compareTo(1) ==
        0 &&
        !Cardinality.allowsZero(getBase.getCardinality)) {
        StaticProperty.ALLOWS_ONE
      } else {
        StaticProperty.ALLOWS_ZERO_OR_ONE
      }
    }
    if (filterIsIndependent) {
      val filterType: ItemType = getFilter.getItemType.getPrimitiveItemType
      if (filterType == BuiltInAtomicType.INTEGER || filterType == BuiltInAtomicType.DOUBLE ||
        filterType == BuiltInAtomicType.DECIMAL ||
        filterType == BuiltInAtomicType.FLOAT) {
        StaticProperty.ALLOWS_ZERO_OR_ONE
      }
      if (getFilter.isInstanceOf[ArithmeticExpression]) {
        StaticProperty.ALLOWS_ZERO_OR_ONE
      }
    }
    if (getFilter.isInstanceOf[IsLastExpression] &&
      getFilter.asInstanceOf[IsLastExpression].getCondition) {
      StaticProperty.ALLOWS_ZERO_OR_ONE
    }
    if (!Cardinality.allowsMany(getBase.getCardinality)) {
      StaticProperty.ALLOWS_ZERO_OR_ONE
    }
    StaticProperty.ALLOWS_ZERO_OR_MORE
  }

  override def computeSpecialProperties(): Int = getBase.getSpecialProperties

  override def equals(other: Any): Boolean = {
    if (other.isInstanceOf[FilterExpression]) {
      val f: FilterExpression = other.asInstanceOf[FilterExpression]
      getBase.isEqual(f.getBase) && getFilter.isEqual(f.getFilter)
    }
    false
  }

  override def computeHashCode(): Int =
    "FilterExpression".hashCode + getBase.hashCode + getFilter.hashCode

  override def toPattern(config: Configuration): Pattern = {
    val base: Expression = getSelectExpression
    val filter: Expression = getFilter
    val th: TypeHierarchy = config.getTypeHierarchy
    val basePattern: Pattern = base.toPattern(config)
    if (!isPositional(th)) {
      new BasePatternWithPredicate(basePattern, filter)
    } else if (basePattern
      .isInstanceOf[NodeTestPattern] && basePattern.getItemType
      .isInstanceOf[NodeTest] &&
      filterIsPositional &&
      base.isInstanceOf[AxisExpression] &&
      base.asInstanceOf[AxisExpression].getAxis == AxisInfo.CHILD &&
      (filter.getDependencies & StaticProperty.DEPENDS_ON_LAST) ==
        0) {
      if (filter.isInstanceOf[Literal] &&
        filter.asInstanceOf[Literal].getValue.isInstanceOf[IntegerValue]) {
        new SimplePositionalPattern(
          basePattern.getItemType.asInstanceOf[NodeTest],
          filter
            .asInstanceOf[Literal]
            .getValue
            .asInstanceOf[IntegerValue]
            .longValue()
            .toInt)
      } else {
        new GeneralPositionalPattern(
          basePattern.getItemType.asInstanceOf[NodeTest],
          filter)
      }
    }
    if (base.getItemType.isInstanceOf[NodeTest]) {
      new GeneralNodePattern(this, base.getItemType.asInstanceOf[NodeTest])
    } else {
      throw new XPathException(
        "The filtered expression in an XSLT 2.0 pattern must be a simple step")
    }
  }

  override def iterate(context: XPathContext): SequenceIterator = {
    if (filterIsIndependent) {
      try {
        val it: SequenceIterator = getFilter.iterate(context)
        val first: Item = it.next()
        if (first == null) {
          EmptyIterator.emptyIterator()
        }
        if (first.isInstanceOf[NumericValue]) {
          if (it.next() != null) {
            ExpressionTool.ebvError(
              "sequence of two or more items starting with a numeric value",
              getFilter)
          } else {
            val pos: Int = first.asInstanceOf[NumericValue].asSubscript()
            if (pos != -1) {
              if (getBase.isInstanceOf[VariableReference]) {
                val baseVal: Sequence = getBase
                  .asInstanceOf[VariableReference]
                  .evaluateVariable(context)
                if (baseVal.isInstanceOf[MemoClosure]) {
                  val m: Item =
                    baseVal.asInstanceOf[MemoClosure].itemAt(pos - 1)
                  if (m == null) EmptyIterator.emptyIterator() else m.iterate()
                } else {
                  val m: Item = baseVal.materialize().itemAt(pos - 1)
                  if (m == null) EmptyIterator.emptyIterator() else m.iterate()
                }
              } else if (getBase.isInstanceOf[Literal]) {
                val i: Item =
                  getBase.asInstanceOf[Literal].getValue.itemAt(pos - 1)
                if (i == null) EmptyIterator.emptyIterator() else i.iterate()
              } else {
                val baseIter: SequenceIterator = getBase.iterate(context)
                SubsequenceIterator.make(baseIter, pos, pos)
              }
            }
            EmptyIterator.emptyIterator()
          }
        } else {
          var ebv: Boolean = false
          if (first.isInstanceOf[NodeInfo]) {
            ebv = true
          } else if (first.isInstanceOf[BooleanValue]) {
            ebv = first.asInstanceOf[BooleanValue].getBooleanValue
            if (it.next() != null) {
              ExpressionTool.ebvError(
                "sequence of two or more items starting with a boolean value",
                getFilter)
            }
          } else if (first.isInstanceOf[StringValue]) {
            ebv = !first.asInstanceOf[StringValue].isZeroLength
            if (it.next() != null) {
              ExpressionTool.ebvError(
                "sequence of two or more items starting with a boolean value",
                getFilter)
            }
          } else {
            ExpressionTool.ebvError(
              "sequence starting with an atomic value other than a boolean, number, or string",
              getFilter)
          }
          if (ebv) {
            getBase.iterate(context)
          } else {
            EmptyIterator.emptyIterator()
          }
        }
      } catch {
        case e: XPathException => {
          e.maybeSetLocation(getLocation)
          throw e
        }

      }
    }
    val baseIter: SequenceIterator = getBase.iterate(context)
    if (baseIter.isInstanceOf[EmptyIterator]) {
      return baseIter
    }
    if (filterIsPositional && !filterIsSingletonBoolean) {
      new FilterIterator(baseIter, getFilter, context)
    } else {
      new FilterIterator.NonNumeric(baseIter, getFilter, context)
    }
  }

  def copy(rebindings: RebindingMap): Expression = {
    val fe: FilterExpression = new FilterExpression(getBase.copy(rebindings),
      getFilter.copy(rebindings))
    ExpressionTool.copyLocationInfo(this, fe)
    fe.filterIsIndependent = filterIsIndependent
    fe.filterIsPositional = filterIsPositional
    fe.filterIsSingletonBoolean = filterIsSingletonBoolean
    fe
  }

  override def getStreamerName(): String = "FilterExpression"

  override def toString(): String =
    ExpressionTool.parenthesize(getBase) + "[" + getFilter +
      "]"

  override def toShortString(): String =
    getBase.toShortString() + "[" + getFilter.toShortString() +
      "]"

  override def export(out: ExpressionPresenter): Unit = {
    out.startElement("filter", this)
    var flags: String = ""
    if (filterIsIndependent) {
      flags += "i"
    }
    if (filterIsPositional) {
      flags += "p"
    }
    if (filterIsSingletonBoolean) {
      flags += "b"
    }
    out.emitAttribute("flags", flags)
    getBase.export(out)
    getFilter.export(out)
    out.endElement()
  }

  def setFlags(flags: String): Unit = {
    filterIsIndependent = flags.contains("i")
    filterIsPositional = flags.contains("p")
    filterIsSingletonBoolean = flags.contains("b")
  }

}
