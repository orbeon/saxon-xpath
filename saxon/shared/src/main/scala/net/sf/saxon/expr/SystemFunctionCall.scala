package net.sf.saxon.expr

import java.util.Arrays

import net.sf.saxon.event.Outputter
import net.sf.saxon.expr.instruct.AnalyzeString
import net.sf.saxon.expr.oper.OperandArray
import net.sf.saxon.expr.parser._
import net.sf.saxon.functions.{Error, _}
import net.sf.saxon.functions.registry.BuiltInFunctionSet
import net.sf.saxon.lib.NamespaceConstant
import net.sf.saxon.ma.map.MapFunctionSet
import net.sf.saxon.model.{BuiltInAtomicType, ItemType, TypeHierarchy}
import net.sf.saxon.om.{Function, Sequence}
import net.sf.saxon.pattern.{NodeSetPattern, Pattern}
import net.sf.saxon.trace.ExpressionPresenter
import net.sf.saxon.trans.XPathException
import net.sf.saxon.utils.Configuration
import net.sf.saxon.value.IntegerValue

import scala.jdk.CollectionConverters._

object SystemFunctionCall {

  abstract class Optimized(target: SystemFunction,
                           arguments: Array[Expression])
    extends SystemFunctionCall(target, arguments) {

    override def optimize(visitor: ExpressionVisitor,
                          contextInfo: ContextItemStaticInfo): Expression =
      this

  }

}

class SystemFunctionCall(target: SystemFunction, arguments: Array[Expression])
  extends StaticFunctionCall(target, arguments)
    with Negatable {

  var argumentEvaluators: Array[Evaluator] =
    new Array[Evaluator](arguments.length)

  Arrays.fill(argumentEvaluators.asInstanceOf[Array[Object]], Evaluator.LAZY_SEQUENCE)

  override def setRetainedStaticContext(rsc: RetainedStaticContext): Unit = {
    super.setRetainedStaticContext(rsc)
    getTargetFunction.setRetainedStaticContext(rsc)
  }

  override def preEvaluate(visitor: ExpressionVisitor): Expression = {
    val target: SystemFunction = getTargetFunction
    if ((target.getDetails.properties & BuiltInFunctionSet.LATE) ==
      0) {
      super.preEvaluate(visitor)
    } else {
      this
    }
  }

  override def typeCheck(visitor: ExpressionVisitor,
                         contextInfo: ContextItemStaticInfo): Expression = {
    typeCheckChildren(visitor, contextInfo)
    checkFunctionCall(getTargetFunction, visitor)
    getTargetFunction.supplyTypeInformation(visitor, contextInfo, getArguments)
    if ((getTargetFunction.getDetails.properties & BuiltInFunctionSet.LATE) ==
      0) {
      preEvaluateIfConstant(visitor)
    }
    allocateArgumentEvaluators(getArguments)
    this
  }

  private def allocateArgumentEvaluators(arguments: Array[Expression]): Unit = {
    for (i <- 0 until arguments.length) {
      val arg: Expression = arguments(i)
      val cardinality: Int =
        if (isCallOn(classOf[Concat])) StaticProperty.ALLOWS_ZERO_OR_ONE
        else getTargetFunction().getDetails().argumentTypes(i).getCardinality
      argumentEvaluators(i) =
        if (arg.isInstanceOf[Literal]) Evaluator.LITERAL
        else if (arg.isInstanceOf[VariableReference]) Evaluator.VARIABLE
        else if (cardinality == StaticProperty.EXACTLY_ONE)
          Evaluator.SINGLE_ITEM
        else if (cardinality == StaticProperty.ALLOWS_ZERO_OR_ONE)
          Evaluator.OPTIONAL_ITEM
        else Evaluator.LAZY_SEQUENCE
    }
  }

  override def getTargetFunction(): SystemFunction =
    super.getTargetFunction.asInstanceOf[SystemFunction]

  override def getIntrinsicDependencies(): Int = {
    val properties: Int = getTargetFunction.getDetails.properties
    var dep: Int = 0
    if ((properties & BuiltInFunctionSet.LATE) != 0) {
      dep = StaticProperty.DEPENDS_ON_RUNTIME_ENVIRONMENT
    }
    if ((properties & BuiltInFunctionSet.FOCUS) != 0) {
      if ((properties & BuiltInFunctionSet.CDOC) != 0) {
        dep |= StaticProperty.DEPENDS_ON_CONTEXT_DOCUMENT
      }
      if ((properties & BuiltInFunctionSet.CITEM) != 0) {
        dep |= StaticProperty.DEPENDS_ON_CONTEXT_ITEM
      }
      if ((properties & BuiltInFunctionSet.POSN) != 0) {
        dep |= StaticProperty.DEPENDS_ON_POSITION
      }
      if ((properties & BuiltInFunctionSet.LAST) != 0) {
        dep |= StaticProperty.DEPENDS_ON_LAST
      }
    }
    if ((properties & BuiltInFunctionSet.BASE) != 0) {
      dep |= StaticProperty.DEPENDS_ON_STATIC_CONTEXT
    }
    if ((properties & BuiltInFunctionSet.DCOLL) != 0) {
      dep |= StaticProperty.DEPENDS_ON_STATIC_CONTEXT
    }
//    if (isCallOn(classOf[RegexGroup]) || isCallOn(classOf[CurrentMergeGroup]) || isCallOn(classOf[CurrentMergeKey])) {
    if (isCallOn(classOf[RegexGroup])) {
      dep |= StaticProperty.DEPENDS_ON_CURRENT_GROUP
    }
    dep
  }

   override def computeCardinality(): Int =
    getTargetFunction.getCardinality(getArguments)

   override def computeSpecialProperties(): Int =
    getTargetFunction.getSpecialProperties(getArguments)

  override def getNetCost(): Int = getTargetFunction.getNetCost

  override def getScopingExpression(): Expression =
    if (isCallOn(classOf[RegexGroup])) {
      var parent: Expression = getParentExpression
      while (parent != null) {
        if (parent.isInstanceOf[AnalyzeString]) {
          return parent
        }
        parent = parent.getParentExpression
      }
      null
    } else {
      super.getScopingExpression
    }

  override def isLiftable(forStreaming: Boolean): Boolean =
    super.isLiftable(forStreaming) &&
//      ! isCallOn(classOf[CurrentMergeGroup]) &&
//      ! isCallOn(classOf[CurrentMergeKey])   &&
      (! forStreaming || ! isCallOn(classOf[MapFunctionSet.MapEntry]))

  override def optimize(visitor: ExpressionVisitor,
                        contextInfo: ContextItemStaticInfo): Expression = {
    val opt: Optimizer = visitor.obtainOptimizer()
    val sf: Expression = super.optimize(visitor, contextInfo)
    if (sf == this) {
      val sfo: Expression = getTargetFunction.makeOptimizedFunctionCall(
        visitor,
        contextInfo,
        getArguments.toIndexedSeq: _*)
      if (sfo != null) {
        sfo.setParentExpression(getParentExpression)
        ExpressionTool.copyLocationInfo(this, sfo)
        if (sfo.isInstanceOf[SystemFunctionCall]) {
          sfo
            .asInstanceOf[SystemFunctionCall]
            .allocateArgumentEvaluators(
              sfo.asInstanceOf[SystemFunctionCall].getArguments)
        }
        return sfo
      }
    }
    if (sf.isInstanceOf[SystemFunctionCall] && opt.isOptionSet(
      OptimizerOptions.CONSTANT_FOLDING)) {
      val details: BuiltInFunctionSet.Entry =
        sf.asInstanceOf[SystemFunctionCall].getTargetFunction.getDetails
      if ((details.properties & BuiltInFunctionSet.UO) != 0) {
        setArg(0, getArg(0).unordered(retainAllNodes = true, forStreaming = visitor.isOptimizeForStreaming))
      }
      if (getArity <= details.resultIfEmpty.length) {
        for (i <- 0 until getArity
             if Literal.isEmptySequence(getArg(i)) && details.resultIfEmpty(i) != null) {
          Literal.makeLiteral(details.resultIfEmpty(i).materialize(), this)
        }
      }
      sf.asInstanceOf[SystemFunctionCall]
        .allocateArgumentEvaluators(
          sf.asInstanceOf[SystemFunctionCall].getArguments)
    }
    sf
  }

  override def isVacuousExpression(): Boolean = isCallOn(classOf[Error])

  override def getItemType(): ItemType =
    getTargetFunction.getResultItemType(getArguments)

  override def copy(rebindings: RebindingMap): Expression = {
    val args: Array[Expression] = Array.ofDim[Expression](getArity)
    for (i <- 0 until args.length) {
      args(i) = getArg(i).copy(rebindings)
    }
    var target: SystemFunction = getTargetFunction
    if (target.isInstanceOf[StatefulSystemFunction]) {
      target = target.asInstanceOf[StatefulSystemFunction].copy()
    }
    target.makeFunctionCall(args.toIndexedSeq: _*)
  }

  override def getIntegerBounds(): Array[IntegerValue] = {
    val fn: SystemFunction = getTargetFunction
    if ((fn.getDetails.properties & BuiltInFunctionSet.FILTER) !=
      0) {
      getArg(0).getIntegerBounds
    }
    fn.getIntegerBounds
  }

  def isNegatable(th: TypeHierarchy): Boolean =
    isCallOn(classOf[NotFn]) || isCallOn(classOf[BooleanFn]) ||
      isCallOn(classOf[Empty]) ||
      isCallOn(classOf[Exists])

  def negate(): Expression = {
    val fn: SystemFunction = getTargetFunction
    if (fn.isInstanceOf[NotFn]) {
      val arg: Expression = getArg(0)
      if (arg.getItemType == BuiltInAtomicType.BOOLEAN && arg.getCardinality == StaticProperty.EXACTLY_ONE) {
        return arg
      } else {
        SystemFunction.makeCall("boolean", getRetainedStaticContext, arg)
      }
    } else if (fn.isInstanceOf[BooleanFn]) {
      SystemFunction.makeCall("not", getRetainedStaticContext, getArg(0))
    } else if (fn.isInstanceOf[Exists]) {
      SystemFunction.makeCall("empty", getRetainedStaticContext, getArg(0))
    } else if (fn.isInstanceOf[Empty]) {
      SystemFunction.makeCall("exists", getRetainedStaticContext, getArg(0))
    }
    throw new UnsupportedOperationException()
  }

  override def unordered(retainAllNodes: Boolean,
                         forStreaming: Boolean): Expression = {
    val fn: SystemFunction = getTargetFunction
    if (fn.isInstanceOf[Reverse]) {
      getArg(0)
    }
    if (fn.isInstanceOf[TreatFn]) {
      setArg(0, getArg(0).unordered(retainAllNodes, forStreaming))
    }
    this
  }

  override def addToPathMap(
                             pathMap: PathMap,
                             pathMapNodeSet: PathMap.PathMapNodeSet): PathMap.PathMapNodeSet =
    if (isCallOn(classOf[Doc]) || isCallOn(classOf[DocumentFn]) ||
      isCallOn(classOf[CollectionFn])) {
      getArg(0).addToPathMap(pathMap, pathMapNodeSet)
      new PathMap.PathMapNodeSet(pathMap.makeNewRoot(this))
    } else if (isCallOn(classOf[KeyFn])) {
      getTargetFunction
        .asInstanceOf[KeyFn]
        .addToPathMap(pathMap, pathMapNodeSet)
    } else {
      super.addToPathMap(pathMap, pathMapNodeSet)
    }

  override def toPattern(config: Configuration): Pattern = {
    val fn: SystemFunction = getTargetFunction
    if (fn.isInstanceOf[Root_1]) {
      if (getArg(0).isInstanceOf[ContextItemExpression] ||
        (getArg(0).isInstanceOf[ItemChecker] &&
          getArg(0)
            .asInstanceOf[ItemChecker]
            .getBaseExpression
            .isInstanceOf[ContextItemExpression])) {
        new NodeSetPattern(this)
      }
    }
    super.toPattern(config)
  }

  override def evaluateArguments(context: XPathContext): Array[Sequence] = {
    val operanda: OperandArray = getOperanda
    val numArgs: Int = operanda.getNumberOfOperands
    val actualArgs: Array[Sequence] = Array.ofDim[Sequence](numArgs)
    for (i <- 0 until numArgs) {
      val exp: Expression = operanda.getOperandExpression(i)
      actualArgs(i) = argumentEvaluators(i).evaluate(exp, context)
    }
    actualArgs
  }

  override def resetLocalStaticProperties(): Unit = {
    super.resetLocalStaticProperties()
    if (argumentEvaluators != null) {
      allocateArgumentEvaluators(getArguments)
    }
  }

  override def process(output: Outputter, context: XPathContext): Unit = {
    val target: Function = getTargetFunction
    if (target.isInstanceOf[PushableFunction]) {
      val actualArgs: Array[Sequence] = evaluateArguments(context)
      try target
        .asInstanceOf[PushableFunction]
        .process(output, context, actualArgs)
      catch {
        case e: XPathException => {
          e.maybeSetLocation(getLocation)
          e.maybeSetContext(context)
          e.maybeSetFailingExpression(this)
          throw e
        }

      }
    } else {
      super.process(output, context)
    }
  }

  override def getExpressionName(): String = "sysFuncCall"

  override def export(out: ExpressionPresenter): Unit = {
    if (getFunctionName.hasURI(NamespaceConstant.FN)) {
      out.startElement("fn", this)
      out.emitAttribute("name", getFunctionName.getLocalPart)
      getTargetFunction.exportAttributes(out)
      for (o <- operands().asScala) {
        o.getChildExpression.export(out)
      }
      getTargetFunction.exportAdditionalArguments(this, out)
      out.endElement()
    } else {
      out.startElement("ifCall", this)
      out.emitAttribute("name", getFunctionName)
      out.emitAttribute(
        "type",
        getTargetFunction.getFunctionItemType.getResultType.toAlphaCode)
      getTargetFunction.exportAttributes(out)
      for (o <- operands().asScala) {
        o.getChildExpression.export(out)
      }
      getTargetFunction.exportAdditionalArguments(this, out)
      out.endElement()
    }
  }

}
