package net.sf.saxon.expr.parser

import net.sf.saxon.utils.Configuration

import net.sf.saxon.expr._

import net.sf.saxon.expr.instruct._

import net.sf.saxon.expr.sort.DocumentSorter

import net.sf.saxon.functions.PositionAndLast

import net.sf.saxon.lib.Feature

import net.sf.saxon.lib.Logger

import net.sf.saxon.model.TypeHierarchy

import net.sf.saxon.om.GroundedValue

import net.sf.saxon.om.Sequence

import net.sf.saxon.om.SequenceIterator

import net.sf.saxon.pattern.NodeSetPattern

import net.sf.saxon.pattern.Pattern

import net.sf.saxon.trans.GlobalVariableManager

import net.sf.saxon.trans.XPathException

import net.sf.saxon.trans.rules.RuleTarget

import net.sf.saxon.value.BooleanValue

import scala.jdk.CollectionConverters._

import scala.beans.{BeanProperty, BooleanBeanProperty}

object Optimizer {

  def trace(config: Configuration, message: String, exp: Expression): Unit = {
    if (config.getBooleanProperty(Feature.TRACE_OPTIMIZER_DECISIONS)) {
      val err: Logger = config.getLogger
      err.info(
        "OPT : At line " + exp.getLocation.getLineNumber + " of " +
          exp.getLocation.getSystemId)
      err.info("OPT : " + message)
      err.info("OPT : Expression after rewrite: " + exp.toString)
      exp.verifyParentPointers()
    }
  }

}

class Optimizer( var config: Configuration) {

  @BeanProperty
  var optimizerOptions: OptimizerOptions =
    OptimizerOptions.FULL_EE_OPTIMIZATION

   var tracing: Boolean =
    config.getBooleanProperty(Feature.TRACE_OPTIMIZER_DECISIONS)

  def getConfiguration(): Configuration = config

  def isOptionSet(option: Int): Boolean = optimizerOptions.isSet(option)

  def optimizeValueComparison(
                               vc: ValueComparison,
                               visitor: ExpressionVisitor,
                               contextInfo: ContextItemStaticInfo): Expression = {
    val lhs: Expression = vc.getLhsExpression
    val rhs: Expression = vc.getRhsExpression
    var e2: Expression = optimizePositionVsLast(lhs, rhs, vc.getOperator)
    if (e2 != null) {
      trace("Rewrote position() ~= last()", e2)
      return e2
    }
    e2 = optimizePositionVsLast(rhs, lhs, Token.inverse(vc.getOperator))
    if (e2 != null) {
      trace("Rewrote last() ~= position()", e2)
      return e2
    }
    vc
  }

  private def optimizePositionVsLast(lhs: Expression,
                                     rhs: Expression,
                                     operator: Int): Expression = {
    if (lhs.isCallOn(classOf[PositionAndLast.Position]) && rhs.isCallOn(
      classOf[PositionAndLast.Last])) {
      operator match {
        case Token.FEQ | Token.FGE =>
          var iletrue: IsLastExpression = new IsLastExpression(true)
          ExpressionTool.copyLocationInfo(lhs, iletrue)
          return iletrue
        case Token.FNE | Token.FLT =>
          var ilefalse: IsLastExpression = new IsLastExpression(false)
          ExpressionTool.copyLocationInfo(lhs, ilefalse)
          return ilefalse
        case Token.FGT => Literal.makeLiteral(BooleanValue.FALSE, lhs)
        case Token.FLE => Literal.makeLiteral(BooleanValue.TRUE, lhs)

      }
    }
    null
  }

  def optimizeGeneralComparison(
                                 visitor: ExpressionVisitor,
                                 gc: GeneralComparison,
                                 backwardsCompatible: Boolean,
                                 contextItemType: ContextItemStaticInfo): Expression = gc

  def optimizeSaxonStreamFunction(visitor: ExpressionVisitor,
                                  cisi: ContextItemStaticInfo,
                                  select: Expression): Expression = {
    if (select.getItemType.isPlainType) {
      return select
    }
    null
  }

  def convertPathExpressionToKey(pathExp: SlashExpression,
                                 visitor: ExpressionVisitor): Expression = null

  def tryIndexedFilter(f: FilterExpression,
                       visitor: ExpressionVisitor,
                       indexFirstOperand: Boolean,
                       contextIsDoc: Boolean): Expression = f

  def reorderPredicates(f: FilterExpression,
                        visitor: ExpressionVisitor,
                        cisi: ContextItemStaticInfo): FilterExpression = f

  def convertToFilterExpression(pathExp: SlashExpression,
                                th: TypeHierarchy): FilterExpression = null

  def isIndexableFilter(filter: Expression): Int = 0

  def makeIndexedValue(iter: SequenceIterator): GroundedValue =
    throw new UnsupportedOperationException("Indexing requires Saxon-EE")

  def optimizeNodeSetPattern(pattern: NodeSetPattern): Unit = {}

  def prepareForStreaming(exp: Expression): Unit = {}

  def evaluateStreamingArgument(expr: Expression,
                                context: XPathContext): Sequence =
    ExpressionTool.eagerEvaluate(expr, context)

  def isVariableReplaceableByDot(exp: Expression,
                                 binding: Array[Binding]): Boolean =
    if (exp.isInstanceOf[ContextSwitchingExpression]) {
      val start: Expression =
        exp.asInstanceOf[ContextSwitchingExpression].getSelectExpression
      val step: Expression =
        exp.asInstanceOf[ContextSwitchingExpression].getActionExpression
      isVariableReplaceableByDot(start, binding) && !ExpressionTool
        .dependsOnVariable(step, binding)
    } else {
      for (op <- exp.operands().asScala) if (!isVariableReplaceableByDot(op.getChildExpression, binding)) false
      true
    }

  def makeConditionalDocumentSorter(sorter: DocumentSorter,
                                    path: SlashExpression): Expression = sorter

  def tryInlineFunctionCall(
                             functionCall: UserFunctionCall,
                             visitor: ExpressionVisitor,
                             contextItemType: ContextItemStaticInfo): Expression = functionCall

  def promoteExpressionsToGlobal(body: Expression,
                                 gvManager: GlobalVariableManager,
                                 visitor: ExpressionVisitor): Expression = null

  def eliminateCommonSubexpressions(in: Expression): Expression = in

  def trySwitch(choose: Choose, visitor: ExpressionVisitor): Expression =
    choose

  def tryGeneralComparison(visitor: ExpressionVisitor,
                           contextItemType: ContextItemStaticInfo,
                           orExpr: OrExpression): Expression = orExpr

  def makeInversion(pattern: Pattern, template: NamedTemplate): RuleTarget =
    null

  def makeCopyOperationsExplicit(parent: Expression, child: Operand): Unit = {}

  def checkStreamability(/*sourceTemplate: XSLTemplate,*/ // no scala class found
                         compiledTemplate: TemplateRule): Unit = {}

  def optimizeQuantifiedExpressionForStreaming(
                                                expr: QuantifiedExpression): Expression = expr

  def generateMultithreadedInstruction(instruction: Expression): Expression =
    instruction

  def compileToByteCode(compilerService: ICompilerService,
                        expr: Expression,
                        objectName: String,
                        evaluationMethods: Int): Expression = null

  def makeByteCodeCandidate(owner: ExpressionOwner,
                            expr: Expression,
                            objectName: String,
                            requiredEvaluationModes: Int): Expression = expr

  def injectByteCodeCandidates(exp: Expression): Unit = {}

  def optimizeNumberInstruction(
                                 ni: NumberInstruction,
                                 contextInfo: ContextItemStaticInfo): Expression = null

  def assessFunctionStreamability(/*reporter: XSLFunction,*/ // no scala class found
                                  compiledFunction: UserFunction): Unit = {
    throw new XPathException(
      "Streamable stylesheet functions are not supported in Saxon-HE",
      "XTSE3430")
  }

  def trace(message: String, exp: Expression): Unit = {
    if (tracing) {
      val err: Logger = getConfiguration.getLogger
      err.info(
        "OPT : At line " + exp.getLocation.getLineNumber + " of " +
          exp.getLocation.getSystemId)
      err.info("OPT : " + message)
      err.info("OPT : Expression after rewrite: " + exp.toString)
      exp.verifyParentPointers()
    }
  }

}
