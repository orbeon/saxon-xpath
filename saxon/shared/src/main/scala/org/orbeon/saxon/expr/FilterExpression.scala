package org.orbeon.saxon.expr

import org.orbeon.saxon.expr.Expression._
import org.orbeon.saxon.expr.FilterExpression._
import org.orbeon.saxon.expr.instruct.Choose
import org.orbeon.saxon.expr.parser._
import org.orbeon.saxon.functions.registry.VendorFunctionSetHE
import org.orbeon.saxon.functions.{LocalName_1, PositionAndLast, SystemFunction}
import org.orbeon.saxon.lib.{Feature, NamespaceConstant}
import org.orbeon.saxon.model._
import org.orbeon.saxon.om._
import org.orbeon.saxon.pattern._
import org.orbeon.saxon.trace.ExpressionPresenter
import org.orbeon.saxon.trans.XPathException
import org.orbeon.saxon.tree.iter.EmptyIterator
import org.orbeon.saxon.utils.Configuration
import org.orbeon.saxon.value._

import java.math.BigInteger
import scala.beans.BooleanBeanProperty


object FilterExpression {

  val FILTERED: Int = 10000

  val FILTER_PREDICATE: OperandRole = new OperandRole(
    OperandRole.USES_NEW_FOCUS | OperandRole.HIGHER_ORDER,
    OperandUsage.INSPECTION,
    SequenceType.ANY_SEQUENCE)

  private def forceToBoolean(in: Expression): Expression =
    if (in.getItemType.getPrimitiveType == StandardNames.XS_BOOLEAN)
      in
    else
      SystemFunction.makeCall("boolean", in.getRetainedStaticContext, in)

  private def tryToRewritePositionalFilterSupport(
    start     : Expression,
    comparand : Expression,
    operator  : Int,
    th        : TypeHierarchy
  ): Expression =
    if (th.isSubType(comparand.getItemType, BuiltInAtomicType.INTEGER)) {
      operator match {
        case Token.FEQ =>
          if (Literal.isConstantOne(comparand))
            FirstItemExpression.makeFirstItemExpression(start)
          else
            comparand match {
              case literal: Literal if literal
                .getValue
                .asInstanceOf[IntegerValue]
                .asBigInteger()
                .compareTo(BigInteger.ZERO) <=
                0 =>
                Literal.makeEmptySequence
              case _ =>
                new SubscriptExpression(start, comparand)
            }
        case Token.FLT =>
          SystemFunction.makeCall(
            "subsequence",
            start.getRetainedStaticContext,
            start,
            Literal.makeLiteral(Int64Value.makeIntegerValue(1), start),
            if (Literal.isAtomic(comparand)) {
              val n = comparand
                .asInstanceOf[Literal]
                .getValue
                .asInstanceOf[NumericValue]
                .longValue
              Literal.makeLiteral(Int64Value.makeIntegerValue(n - 1), start)
            } else {
              val decrement = new ArithmeticExpression(
                comparand,
                Token.MINUS,
                Literal.makeLiteral(Int64Value.makeIntegerValue(1), start)
              )
              decrement.setCalculator(
                Calculator.getCalculator(
                  StandardNames.XS_INTEGER,
                  StandardNames.XS_INTEGER,
                  Calculator.MINUS,
                  mustResolve = true
                )
              )
              decrement
            }
          )
        case Token.FLE =>
          SystemFunction.makeCall(
            "subsequence",
            start.getRetainedStaticContext,
            start,
            Literal.makeLiteral(Int64Value.makeIntegerValue(1), start),
            comparand
          )
        case Token.FNE =>
          SystemFunction.makeCall("remove", start.getRetainedStaticContext, start, comparand)
        case Token.FGT =>
          SystemFunction.makeCall(
            "subsequence",
            start.getRetainedStaticContext,
            start,
            if (Literal.isAtomic(comparand)) {
              val n = comparand
                .asInstanceOf[Literal]
                .getValue
                .asInstanceOf[NumericValue]
                .longValue
              Literal.makeLiteral(Int64Value.makeIntegerValue(n + 1), start)
            } else {
               new ArithmeticExpression(
                comparand,
                Token.PLUS,
                Literal.makeLiteral(Int64Value.makeIntegerValue(1), start)
               )
            }
          )
        case Token.FGE =>
          SystemFunction.makeCall(
            "subsequence",
            start.getRetainedStaticContext,
            start,
            comparand
          )
        case _ =>
          throw new IllegalArgumentException("operator")
      }
    } else {
      operator match {
        case Token.FEQ =>
          new SubscriptExpression(start, comparand)
        case Token.FLT =>
          val let = new LetExpression
          let.setRequiredType(SequenceType.makeSequenceType(comparand.getItemType, StaticProperty.ALLOWS_ONE))
          let.setVariableQName(new StructuredQName("pp", NamespaceConstant.SAXON, "pp" + let.hashCode))
          let.setSequence(comparand)
          val isWholeArg = new LocalVariableReference(let)
          val arithArg = new LocalVariableReference(let)
          val floorArg = new LocalVariableReference(let)
          val isWhole = VendorFunctionSetHE.getInstance
            .makeFunction("is-whole-number", 1)
            .makeFunctionCall(isWholeArg)
          val minusOne = new ArithmeticExpression(
            arithArg,
            Token.MINUS,
            Literal.makeLiteral(Int64Value.makeIntegerValue(1), start))
          val floor = SystemFunction.makeCall(
            "floor",
            start.getRetainedStaticContext,
            floorArg
          )
          val choice = Choose.makeConditional(isWhole, minusOne, floor)
          val subs = SystemFunction.makeCall(
            "subsequence",
            start.getRetainedStaticContext,
            start,
            Literal.makeLiteral(Int64Value.makeIntegerValue(1)),
            start,
            choice
          )
          let.setAction(subs)
          let
        case Token.FLE =>
          val floor = SystemFunction.makeCall(
            "floor",
            start.getRetainedStaticContext,
            comparand
          )
          SystemFunction.makeCall(
            "subsequence",
            start.getRetainedStaticContext,
            start,
            Literal.makeLiteral(Int64Value.makeIntegerValue(1), start),
            floor
          )
        case Token.FNE =>
          val let = new LetExpression()
          ExpressionTool.copyLocationInfo(start, let)
          let.setRequiredType(
            SequenceType.makeSequenceType(comparand.getItemType,
              StaticProperty.ALLOWS_ONE))
          let.setVariableQName(
            new StructuredQName("pp",
              NamespaceConstant.SAXON,
              "pp" + let.hashCode))
          let.setSequence(comparand)
          val isWholeArg = new LocalVariableReference(let)
          val castArg = new LocalVariableReference(let)
          val isWhole = VendorFunctionSetHE.getInstance
            .makeFunction("is-whole-number", 1)
            .makeFunctionCall(isWholeArg)
          ExpressionTool.copyLocationInfo(start, isWhole)
          val cast = new CastExpression(castArg, BuiltInAtomicType.INTEGER, false)
          ExpressionTool.copyLocationInfo(start, cast)
          val choice = Choose.makeConditional(
            isWhole,
            cast,
            Literal.makeLiteral(Int64Value.makeIntegerValue(0), start))
          val rem = SystemFunction.makeCall(
            "remove",
            start.getRetainedStaticContext,
            start,
            choice)
          let.setAction(rem)
          let
        case Token.FGT =>
          val let = new LetExpression
          let.setRequiredType(
            SequenceType.makeSequenceType(comparand.getItemType,
              StaticProperty.ALLOWS_ONE))
          let.setVariableQName(
            new StructuredQName("pp",
              NamespaceConstant.SAXON,
              "pp" + let.hashCode))
          let.setSequence(comparand)
          val isWholeArg = new LocalVariableReference(let)
          val arithArg = new LocalVariableReference(let)
          val ceilingArg = new LocalVariableReference(let)
          val isWhole = VendorFunctionSetHE.getInstance
            .makeFunction("is-whole-number", 1)
            .makeFunctionCall(isWholeArg)
          val plusOne = new ArithmeticExpression(
            arithArg,
            Token.PLUS,
            Literal.makeLiteral(Int64Value.makeIntegerValue(1), start))
          val ceiling = SystemFunction.makeCall(
            "ceiling",
            start.getRetainedStaticContext,
            ceilingArg)
          val choice =
            Choose.makeConditional(isWhole, plusOne, ceiling)
          val subs = SystemFunction.makeCall(
            "subsequence",
            start.getRetainedStaticContext,
            start,
            choice)
          let.setAction(subs)
          let
        case Token.FGE =>
          val ceiling = SystemFunction.makeCall(
            "ceiling",
            start.getRetainedStaticContext,
            comparand)
          SystemFunction.makeCall("subsequence",
            start.getRetainedStaticContext,
            start,
            ceiling)
        case _ =>
          throw new IllegalArgumentException("operator")
      }
    }

  def isPositionalFilter(exp: Expression, th: TypeHierarchy): Boolean = {
    val _type = exp.getItemType
    if (_type == BuiltInAtomicType.BOOLEAN) {
      isExplicitlyPositional(exp)
    } else {
      _type == BuiltInAtomicType.ANY_ATOMIC || (_type eq AnyItemType) ||
        _type == BuiltInAtomicType.INTEGER ||
        _type == NumericType.getInstance ||
        NumericType.isNumericType(_type) ||
        isExplicitlyPositional(exp)
    }
  }

  private def isExplicitlyPositional(exp: Expression): Boolean =
    (exp.getDependencies & (StaticProperty.DEPENDS_ON_POSITION | StaticProperty.DEPENDS_ON_LAST)) != 0
}

class FilterExpression(base: Expression, filter: Expression)
  extends BinaryExpression(base, Token.LSQB, filter)
    with ContextSwitchingExpression {

  @BooleanBeanProperty
  var filterIsPositional: Boolean = _
  private var filterIsSingletonBoolean: Boolean = _
  private var filterIsIndependent: Boolean = _
  var doneReorderingPredicates: Boolean = false

  base.setFiltered(true)

  override def getOperandRole(arg: Int): OperandRole =
    if (arg == 0) OperandRole.SAME_FOCUS_ACTION else FILTER_PREDICATE

  def getBase: Expression = getLhsExpression

  def setBase(baseExp: Expression): Unit =
    this.setLhsExpression(baseExp)

  def getFilter: Expression = getRhsExpression

  def setFilter(filter: Expression): Unit =
    this.setRhsExpression(filter)

  override def getExpressionName: String = "filter"

  def getItemType: ItemType =
    getFilter match {
      case instanceofExpression: InstanceOfExpression if instanceofExpression.getBaseExpression.isInstanceOf[ContextItemExpression] =>
        instanceofExpression.getRequiredItemType
      case _ =>
        getBase.getItemType
    }

  override def getStaticUType(contextItemType: UType): UType =
    getBase.getStaticUType(contextItemType)

  def getSelectExpression: Expression = getBase
  def getActionExpression: Expression = getFilter

  def isPositional(th: TypeHierarchy): Boolean =
    isPositionalFilter(getFilter, th)

  def isSimpleBooleanFilter: Boolean = filterIsSingletonBoolean

  def isIndependentFilter: Boolean = filterIsIndependent

  override def simplify(): Expression = {
    setBase(getBase.simplify())
    setFilter(getFilter.simplify())
    if (Literal.isEmptySequence(getBase))
      return getBase
    getFilter match {
      case literal: Literal if ! literal.getValue.isInstanceOf[NumericValue] =>
        try
          if (getFilter.effectiveBooleanValue(new EarlyEvaluationContext(getConfiguration)))
            return getBase
          else
            return Literal.makeEmptySequence
        catch {
          case e: XPathException =>
            e.maybeSetLocation(getLocation)
            throw e
        }
      case _ =>
         if (getFilter.isCallOn(classOf[PositionAndLast.Last])) {
           setFilter(new IsLastExpression(true))
           adoptChildExpression(getFilter)
         }
    }
    this
  }

  override def typeCheck(visitor: ExpressionVisitor,
                         contextInfo: ContextItemStaticInfo): Expression = {
    val config: Configuration = visitor.getConfiguration
    val th = config.getTypeHierarchy
    getLhs.typeCheck(visitor, contextInfo)
    getBase.setFiltered(true)
    if (Literal.isEmptySequence(getBase))
      return getBase
    val baseItemType = config.makeContextItemStaticInfo(getSelectExpression.getItemType, maybeUndefined = false)
    baseItemType.setContextSettingExpression(getBase)
    getRhs.typeCheck(visitor, baseItemType)
    val filter2 = ExpressionTool.unsortedIfHomogeneous(
      getFilter,
      visitor.isOptimizeForStreaming)
    if (filter2 ne getFilter)
      setFilter(filter2)
    if (Literal.isConstantOne(getFilter)) {
      val fie = FirstItemExpression.makeFirstItemExpression(getBase)
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
    val config = visitor.getConfiguration
    val opt = visitor.obtainOptimizer()
    val tracing = config.getBooleanProperty(Feature.TRACE_OPTIMIZER_DECISIONS)
    val th = config.getTypeHierarchy

    getLhs.optimize(visitor, contextItemType)
    getBase.setFiltered(true)

    val baseItemType = config.makeContextItemStaticInfo(getSelectExpression.getItemType, maybeUndefined = false)
    baseItemType.setContextSettingExpression(getBase)
    getRhs.optimize(visitor, baseItemType)

    val filter2 = ExpressionTool.unsortedIfHomogeneous(getFilter, visitor.isOptimizeForStreaming)
    if (filter2 != getFilter)
      setFilter(filter2)

    // Rewrite child::X[last()] as child::X[empty(following-sibling::X)] - especially useful for patterns

    getFilter match {
      case isLastExpression: IsLastExpression
        if isLastExpression.getCondition       &&
          getBase.isInstanceOf[AxisExpression] &&
          getBase.asInstanceOf[AxisExpression].getAxis == AxisInfo.CHILD =>

        val test = getBase.asInstanceOf[AxisExpression].getNodeTest
        val fs = new AxisExpression(AxisInfo.FOLLOWING_SIBLING, test)
        setFilter(SystemFunction.makeCall("empty", getRetainedStaticContext, fs))
        if (tracing)
          Optimizer.trace(
            config,
            "Replaced [last()] predicate by test for following-sibling",
            this)
      case _ =>
    }

    // rewrite axis::*[local-name() = 'literal'] as axis::*:local (people write this a lot in XSLT 1.0)

//    if (
//      getBase.isInstanceOf[AxisExpression] &&
//      (getBase.asInstanceOf[AxisExpression].getNodeTest eq NodeKindTest.ELEMENT) &&
//      getFilter.isInstanceOf[CompareToStringConstant] &&
//      (getFilter.asInstanceOf[CompareToStringConstant].getSingletonOperator == Token.FEQ) &&
//      getFilter.asInstanceOf[CompareToStringConstant].getLhsExpression.isCallOn(classOf[LocalName_1]) &&
//      getFilter.asInstanceOf[CompareToStringConstant].getLhsExpression.asInstanceOf[SystemFunctionCall].getArg(0).isInstanceOf[ContextItemExpression]) {
//      val ax2 =
//        new AxisExpression(
//          getBase.asInstanceOf[AxisExpression].getAxis,
//          new LocalNameTest(config.getNamePool, Type.ELEMENT, getFilter.asInstanceOf[CompareToStringConstant].getComparand)
//        )
//      ExpressionTool.copyLocationInfo(this, ax2)
//      return ax2
//    }

    if (
      {
        getBase match {
          case ae: AxisExpression => ae.getNodeTest eq NodeKindTest.ELEMENT
          case _                  => false
        }
      } && {
        getFilter match {
          case ctsc: CompareToStringConstant =>
            ctsc.getSingletonOperator == Token.FEQ &&
            ctsc.getLhsExpression.isCallOn(classOf[LocalName_1]) &&
            ctsc.getLhsExpression.asInstanceOf[SystemFunctionCall].getArg(0).isInstanceOf[ContextItemExpression]
          case _ => false
        }
      }
    ) {
      val ax2 =
        new AxisExpression(
          getBase.asInstanceOf[AxisExpression].getAxis,
          new LocalNameTest(config.getNamePool, Type.ELEMENT, getFilter.asInstanceOf[CompareToStringConstant].getComparand)
        )
      ExpressionTool.copyLocationInfo(this, ax2)
      return ax2
    }

    // if the result of evaluating the filter cannot include numeric values, then we can use
    // its effective boolean value

    val filterType = getFilter.getItemType
    if (! th.isSubType(filterType, BuiltInAtomicType.BOOLEAN) &&
      th.relationship(filterType, NumericType.getInstance) == Affinity.DISJOINT) {
      val f = SystemFunction.makeCall("boolean", getRetainedStaticContext, getFilter)
      setFilter(f.optimize(visitor, baseItemType))
    }

    // the filter expression may have been reduced to a constant boolean by previous optimizations
    getFilter match {
      case literal: Literal if literal.getValue.isInstanceOf[BooleanValue] =>
        if (literal
          .getValue
          .asInstanceOf[BooleanValue]
          .getBooleanValue) {
          if (tracing)
            opt.trace("Redundant filter removed", getBase)
          return getBase
        } else {
          val result = Literal.makeEmptySequence
          ExpressionTool.copyLocationInfo(this, result)
          if (tracing)
            opt.trace("Filter expression eliminated because predicate is always false", result)
          return result
        }
      case _ =>
    }

    // determine whether the filter might depend on position

    filterIsPositional = isPositionalFilter(getFilter, th)
    filterIsSingletonBoolean =
      getFilter.getCardinality == StaticProperty.EXACTLY_ONE &&
      getFilter.getItemType == BuiltInAtomicType.BOOLEAN

    if (! filterIsPositional && ! visitor.isOptimizeForStreaming) {
      val isIndexable = opt.isIndexableFilter(getFilter)
      if (isIndexable != 0) {
        val contextIsDoc = contextItemType != null &&
          contextItemType.getItemType != ErrorType &&
          th.isSubType(contextItemType.getItemType, NodeKindTest.DOCUMENT)
        val f = opt.tryIndexedFilter(this, visitor, isIndexable > 0, contextIsDoc)
        if (f ne this)
          return f.typeCheck(visitor, contextItemType).optimize(visitor, contextItemType)
      }
    }

    // if the filter is positional, try changing f[a and b] to f[a][b] to increase
    // the chances of finishing early.

    if (filterIsPositional && getFilter.isInstanceOf[BooleanExpression] &&
      getFilter.asInstanceOf[BooleanExpression].operator == Token.AND) {
      val bf = getFilter.asInstanceOf[BooleanExpression]
      if (isExplicitlyPositional(bf.getLhsExpression) && !isExplicitlyPositional(bf.getRhsExpression)) {
        val p0 = forceToBoolean(bf.getLhsExpression)
        val p1 = forceToBoolean(bf.getRhsExpression)
        val f1 = new FilterExpression(getBase, p0)
        ExpressionTool.copyLocationInfo(this, f1)
        val f2 = new FilterExpression(f1, p1)
        ExpressionTool.copyLocationInfo(this, f2)
        if (tracing)
          opt.trace("Composite filter replaced by nested filter expressions", f2)
        return f2.optimize(visitor, contextItemType)
      }
      if (isExplicitlyPositional(bf.getRhsExpression) && !isExplicitlyPositional(bf.getLhsExpression)) {
        val p0 = forceToBoolean(bf.getLhsExpression)
        val p1 = forceToBoolean(bf.getRhsExpression)
        val f1 = new FilterExpression(getBase, p1)
        ExpressionTool.copyLocationInfo(this, f1)
        val f2 = new FilterExpression(f1, p0)
        ExpressionTool.copyLocationInfo(this, f2)
        if (tracing)
          opt.trace("Composite filter replaced by nested filter expressions", f2)
        return f2.optimize(visitor, contextItemType)
      }
    }
    getFilter match {
      case isLastExpression: IsLastExpression if isLastExpression.getCondition =>
        getBase match {
          case literal: Literal =>
            setFilter(Literal.makeLiteral(new Int64Value(literal.getValue.getLength), this))
          case _ =>
            return new LastItemExpression(getBase)
        }
      case _ =>
    }

    val subsequence = tryToRewritePositionalFilter(visitor, tracing)
    if (subsequence != null) {
      if (tracing) {
        subsequence.setRetainedStaticContext(getRetainedStaticContext)
        opt.trace("Rewrote Filter Expression as:", subsequence)
      }
      ExpressionTool.copyLocationInfo(this, subsequence)
      return subsequence
        .simplify()
        .typeCheck(visitor, contextItemType)
        .optimize(visitor, contextItemType)
    }

    // If there are two non-positional filters, consider changing their order based on the estimated cost
    // of evaluation, so we evaluate the cheapest predicates first

    if (! filterIsPositional && ! doneReorderingPredicates &&
      ! getParentExpression.isInstanceOf[FilterExpression]) {
      val f2 = opt.reorderPredicates(this, visitor, contextItemType)
      if (f2 ne this) {
        f2.doneReorderingPredicates = true
        return f2
      }
    }
    val sequence = tryEarlyEvaluation(visitor)
    if (sequence != null) {
      val value = sequence.materialize
      return Literal.makeLiteral(value, this)
    }
    this
  }

  override def getCost: Double =
    Math.max(getLhsExpression.getCost + 5 * getRhsExpression.getCost, MAX_COST)

  override def getImplementationMethod: Int = ITERATE_METHOD

  override def getIntegerBounds: Array[IntegerValue] =
    getBase.getIntegerBounds

  private def tryEarlyEvaluation(visitor: ExpressionVisitor): Sequence = {
    try
      if (getBase.isInstanceOf[Literal] &&
        ! ExpressionTool.refersToVariableOrFunction(getFilter) &&
        (getFilter.getDependencies & ~StaticProperty.DEPENDS_ON_FOCUS) == 0) {
        val context = visitor.getStaticContext.makeEarlyEvaluationContext()
        return iterate(context).materialize
      }
    catch {
      case _: Exception =>
        return null
    }
    null
  }

  override def addToPathMap(
                             pathMap: PathMap,
                             pathMapNodeSet: PathMap.PathMapNodeSet): PathMap.PathMapNodeSet = {
    val target = getBase.addToPathMap(pathMap, pathMapNodeSet)
    getFilter.addToPathMap(pathMap, target)
    target
  }

  private def tryToRewritePositionalFilter(visitor: ExpressionVisitor,
                                           tracing: Boolean): Expression = {
    val config: Configuration = visitor.getConfiguration
    val th = config.getTypeHierarchy
    getFilter match {
      case literal: Literal =>
        val `val` = literal.getValue
        `val` match {
          case numericValue: NumericValue =>
            var result: Expression = null
            val lvalue = numericValue.asSubscript()
            result =
              if (lvalue != -1)
                if (lvalue == 1)
                  FirstItemExpression.makeFirstItemExpression(getBase)
                else
                  new SubscriptExpression(getBase, getFilter)
              else
                Literal.makeEmptySequence
            if (tracing)
              Optimizer.trace(config, "Rewriting numeric filter expression with constant subscript", result)
            return result
          case _ =>
            val result =
              if (ExpressionTool.effectiveBooleanValue(`val`.iterate()))
                getBase
              else
                Literal.makeEmptySequence
            if (tracing)
              Optimizer.trace(config, "Rewriting boolean filter expression with constant subscript", result)
            return result
        }
      case _ =>
    }
    if (NumericType.isNumericType(getFilter.getItemType) && ! Cardinality
      .allowsMany(getFilter.getCardinality) &&
      (getFilter.getDependencies & StaticProperty.DEPENDS_ON_FOCUS) == 0) {
      val result: Expression = new SubscriptExpression(getBase, getFilter)
      if (tracing)
        Optimizer.trace(config, "Rewriting numeric filter expression with focus-independent subscript", result)
      return result
    }
    getFilter match {
      case comparisonExpression: ComparisonExpression =>
        val lhs = comparisonExpression.getLhsExpression
        val rhs = comparisonExpression.getRhsExpression
        var operator = comparisonExpression.getSingletonOperator
        var comparand: Expression = null
        if (lhs.isCallOn(classOf[PositionAndLast.Position]) && NumericType.isNumericType(rhs.getItemType)) {
          comparand = rhs
        } else if (rhs.isCallOn(classOf[PositionAndLast.Position]) && NumericType.isNumericType(lhs.getItemType)) {
          comparand = lhs
          operator = Token.inverse(operator)
        } else {
          return null
        }
        if (ExpressionTool.dependsOnFocus(comparand))
          return null
        val card = comparand.getCardinality
        if (Cardinality.allowsMany(card))
          return null
        if (Cardinality.allowsZero(card)) {
          val let = new LetExpression()
          let.setRequiredType(SequenceType.makeSequenceType(comparand.getItemType, card))
          let.setVariableQName(new StructuredQName("pp", NamespaceConstant.SAXON, "pp" + let.hashCode))
          let.setSequence(comparand)
          comparand = new LocalVariableReference(let)
          val existsArg = new LocalVariableReference(let)
          val exists = SystemFunction.makeCall("exists", getRetainedStaticContext, existsArg)
          val rewrite = tryToRewritePositionalFilterSupport(getBase, comparand, operator, th)
          if (rewrite == null)
            return this
          val choice = Choose.makeConditional(exists, rewrite)
          let.setAction(choice)
          let
        } else {
          tryToRewritePositionalFilterSupport(getBase, comparand, operator, th)
        }
      case test: IntegerRangeTest =>
        val `val` = test.getValue
        if (! `val`.isCallOn(classOf[PositionAndLast]))
          return null
        var min = test.getMin
        val max = test.getMax
        if (ExpressionTool.dependsOnFocus(min))
          return null
        if (ExpressionTool.dependsOnFocus(max)) {
          if (max.isCallOn(classOf[PositionAndLast.Last])) {
            val result = SystemFunction.makeCall(
              "subsequence",
              getRetainedStaticContext,
              getBase,
              min)
            if (tracing)
              Optimizer.trace(config, "Rewriting numeric range filter expression using subsequence()", result)
            return result
          } else {
            return null
          }
        }
        val let = new LetExpression()
        let.setRequiredType(SequenceType.SINGLE_INTEGER)
        let.setVariableQName(new StructuredQName("nn", NamespaceConstant.SAXON, "nn" + let.hashCode))
        let.setSequence(min)
        min = new LocalVariableReference(let)
        val min2 = new LocalVariableReference(let)
        val minMinusOne = new ArithmeticExpression(
          min2,
          Token.MINUS,
          Literal.makeLiteral(Int64Value.makeIntegerValue(1), this))
        val length = new ArithmeticExpression(max, Token.MINUS, minMinusOne)
        val subs = SystemFunction.makeCall("subsequence",
          getRetainedStaticContext,
          getBase,
          min,
          length)
        let.setAction(subs)
        if (tracing)
          Optimizer.trace(config, "Rewriting numeric range filter expression using subsequence()", subs)
        let
      case _ =>
        null
    }
  }

  override def unordered(retainAllNodes: Boolean,
                         forStreaming: Boolean): Expression = {
    if (! filterIsPositional)
      setBase(getBase.unordered(retainAllNodes, forStreaming))
    this
  }

  private def promoteIndependentPredicates(
                                            bindings: Array[Binding],
                                            opt: Optimizer,
                                            th: TypeHierarchy): FilterExpression = {
    if (!ExpressionTool.dependsOnVariable(getBase, bindings))
      return this
    if (isPositional(th))
      return this
    getBase match {
      case fe: FilterExpression =>
        if (fe.isPositional(th))
          return this
        if (! ExpressionTool.dependsOnVariable(fe.getFilter, bindings))
          return this
        if (! ExpressionTool.dependsOnVariable(getFilter, bindings)) {
          val result = new FilterExpression(
            new FilterExpression(fe.getBase, getFilter)
              .promoteIndependentPredicates(bindings, opt, th),
            fe.getFilter)
          opt.trace("Reordered filter predicates:", result)
          return result
        }
      case _ =>
    }
    this
  }

  override def computeCardinality(): Int = {
    getFilter match {
      case literal: Literal if literal.getValue.isInstanceOf[NumericValue] =>
        if (literal
          .getValue
          .asInstanceOf[NumericValue]
          .compareTo(1) == 0 &&
          ! Cardinality.allowsZero(getBase.getCardinality)) {
          return StaticProperty.ALLOWS_ONE
        } else {
          return StaticProperty.ALLOWS_ZERO_OR_ONE
        }
      case _ =>
    }
    if (filterIsIndependent) {
      val filterType = getFilter.getItemType.getPrimitiveItemType
      if (filterType == BuiltInAtomicType.INTEGER ||
        filterType == BuiltInAtomicType.DOUBLE ||
        filterType == BuiltInAtomicType.DECIMAL ||
        filterType == BuiltInAtomicType.FLOAT) {
        return StaticProperty.ALLOWS_ZERO_OR_ONE
      }
      if (getFilter.isInstanceOf[ArithmeticExpression])
        return StaticProperty.ALLOWS_ZERO_OR_ONE
    }
    getFilter match {
      case isLastExpression: IsLastExpression if isLastExpression.getCondition =>
        return StaticProperty.ALLOWS_ZERO_OR_ONE
      case _ =>
    }
    if (! Cardinality.allowsMany(getBase.getCardinality))
      return StaticProperty.ALLOWS_ZERO_OR_ONE
    StaticProperty.ALLOWS_ZERO_OR_MORE
  }

  override def computeSpecialProperties(): Int = getBase.getSpecialProperties

  override def equals(other: Any): Boolean = {
    other match {
      case f: FilterExpression =>
        return getBase.isEqual(f.getBase) && getFilter.isEqual(f.getFilter)
      case _ =>
    }
    false
  }

  override def computeHashCode(): Int =
    "FilterExpression".hashCode + getBase.hashCode + getFilter.hashCode

  override def toPattern(config: Configuration): Pattern = {
    val base = getSelectExpression
    val filter = getFilter
    val th = config.getTypeHierarchy
    val basePattern = base.toPattern(config)
    if (! isPositional(th)) {
      return new BasePatternWithPredicate(basePattern, filter)
    } else if (basePattern
      .isInstanceOf[NodeTestPattern] && basePattern.getItemType
      .isInstanceOf[NodeTest] &&
      filterIsPositional &&
      base.isInstanceOf[AxisExpression] &&
      base.asInstanceOf[AxisExpression].getAxis == AxisInfo.CHILD &&
      (filter.getDependencies & StaticProperty.DEPENDS_ON_LAST) == 0) {
      filter match {
        case literal: Literal if literal.getValue.isInstanceOf[IntegerValue] =>
          return new SimplePositionalPattern(
            basePattern.getItemType.asInstanceOf[NodeTest],
            literal
              .getValue
              .asInstanceOf[IntegerValue]
              .longValue
              .toInt)
        case _ =>
          return new GeneralPositionalPattern(basePattern.getItemType.asInstanceOf[NodeTest], filter)
      }
    }
    base.getItemType match {
      case nodeTest: NodeTest =>
        new GeneralNodePattern(this, nodeTest)
      case _ =>
        throw new XPathException("The filtered expression in an XSLT 2.0 pattern must be a simple step")
    }
  }

  override def iterate(context: XPathContext): SequenceIterator = {
    if (filterIsIndependent) {
      try {
        val it = getFilter.iterate(context)
        val first = it.next()
        if (first == null)
          return EmptyIterator.emptyIterator
        first match {
          case numericValue: NumericValue =>
            if (it.next() != null) {
              ExpressionTool.ebvError("sequence of two or more items starting with a numeric value", getFilter)
            } else {
              val pos = numericValue.asSubscript()
              if (pos != -1) {
                getBase match {
                  case varRef: VariableReference =>
                    val baseVal = varRef.evaluateVariable(context)
                    baseVal match {
                      case memoClosure: MemoClosure =>
                        val m = memoClosure.itemAt(pos - 1)
                        return if (m == null) EmptyIterator.emptyIterator else m.iterate()
                      case _ =>
                        val m = baseVal.materialize.itemAt(pos - 1)
                        return if (m == null) EmptyIterator.emptyIterator else m.iterate()
                    }
                  case literal: Literal =>
                    val i = literal.getValue.itemAt(pos - 1)
                    return if (i == null) EmptyIterator.emptyIterator else i.iterate()
                  case _ =>
                    val baseIter = getBase.iterate(context)
                    return SubsequenceIterator.make(baseIter, pos, pos)
                }
              }
              return EmptyIterator.emptyIterator
            }
          case _ =>
            var ebv = false
            first match {
              case _: NodeInfo =>
                ebv = true
              case _ => first match {
                case booleanValue: BooleanValue =>
                  ebv = booleanValue.getBooleanValue
                  if (it.next() != null)
                    ExpressionTool.ebvError("sequence of two or more items starting with a boolean value", getFilter)
                case _ => first match {
                  case stringValue: StringValue =>
                    ebv = !stringValue.isZeroLength
                    if (it.next() != null)
                      ExpressionTool.ebvError("sequence of two or more items starting with a boolean value", getFilter)
                  case _ =>
                    ExpressionTool.ebvError("sequence starting with an atomic value other than a boolean, number, or string", getFilter)
                }
              }
            }
            if (ebv)
              return getBase.iterate(context)
            else
              return EmptyIterator.emptyIterator
        }
      } catch {
        case e: XPathException =>
          e.maybeSetLocation(getLocation)
          throw e
      }
    }

    val baseIter = getBase.iterate(context)
    if (baseIter.isInstanceOf[EmptyIterator])
      baseIter
    else if (filterIsPositional && ! filterIsSingletonBoolean)
      new FilterIterator(baseIter, getFilter, context)
    else
      new FilterIterator.NonNumeric(baseIter, getFilter, context)
  }

  def copy(rebindings: RebindingMap): Expression = {
    val fe = new FilterExpression(getBase.copy(rebindings), getFilter.copy(rebindings))
    ExpressionTool.copyLocationInfo(this, fe)
    fe.filterIsIndependent = filterIsIndependent
    fe.filterIsPositional = filterIsPositional
    fe.filterIsSingletonBoolean = filterIsSingletonBoolean
    fe
  }

  override def getStreamerName: String = "FilterExpression"

  override def toString: String =
    ExpressionTool.parenthesize(getBase) + "[" + getFilter + "]"

  override def toShortString: String =
    getBase.toShortString + "[" + getFilter.toShortString + "]"

  override def export(out: ExpressionPresenter): Unit = {
    out.startElement("filter", this)
    var flags: String = ""
    if (filterIsIndependent)
      flags += "i"
    if (filterIsPositional)
      flags += "p"
    if (filterIsSingletonBoolean)
      flags += "b"
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
