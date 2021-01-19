package org.orbeon.saxon.functions

import org.orbeon.saxon.expr._
import org.orbeon.saxon.expr.parser._
import org.orbeon.saxon.functions.IntegratedFunctionCall._
import org.orbeon.saxon.lib.{ExtensionFunctionCall, ExtensionFunctionDefinition}
import org.orbeon.saxon.model.{Affinity, AnyItemType, ItemType, TypeHierarchy}
import org.orbeon.saxon.om._
import org.orbeon.saxon.pattern.AnyNodeTest
import org.orbeon.saxon.trace.ExpressionPresenter
import org.orbeon.saxon.trans.{SaxonErrorCode, XPathException}
import org.orbeon.saxon.utils.Configuration
import org.orbeon.saxon.value.SequenceType

import scala.beans.BeanProperty
//import scala.collection.compat._
import scala.jdk.CollectionConverters._

object IntegratedFunctionCall {

  class ConfigurationCheckingFunction(private var config: Configuration)
    extends ItemMappingFunction {

    def mapItem(item: Item): Item = {
      item match {
        case info: NodeInfo if ! config.isCompatible(info.getConfiguration) =>
          throw new XPathException(
            "Node returned by extension function was created with an incompatible Configuration",
            SaxonErrorCode.SXXP0004)
        case _ =>
      }
      item
    }
  }
}

class IntegratedFunctionCall(private var name: StructuredQName,
                             @BeanProperty var function: ExtensionFunctionCall)
  extends FunctionCall
     with Callable {

  private var resultType: SequenceType = SequenceType.ANY_SEQUENCE

  private var state: Int = 0

  def setResultType(resultType: SequenceType): Unit =
    this.resultType = resultType

  def getFunctionName: StructuredQName = name

  def getTargetFunction(context: XPathContext): Function = null

  override def checkArguments(visitor: ExpressionVisitor): Unit = {

    val definition = function.getDefinition
    checkArgumentCount(definition.getMinimumNumberOfArguments, definition.getMaximumNumberOfArguments)

    val args                  = getArity
    val declaredArgumentTypes = definition.getArgumentTypes

    if (declaredArgumentTypes == null || (args != 0 && declaredArgumentTypes.length == 0))
      throw new XPathException("Integrated function " + getDisplayName + " failed to declare its argument types")

    val actualArgumentTypes = Array.ofDim[SequenceType](args)
    val tc                  = visitor.getConfiguration.getTypeChecker(false)
    for (i <- 0 until args) {
      setArg(
        i,
        tc.staticTypeCheck(
          getArg(i),
          if (i < declaredArgumentTypes.length) declaredArgumentTypes(i)
          else declaredArgumentTypes(declaredArgumentTypes.length - 1),
          new RoleDiagnostic(RoleDiagnostic.FUNCTION,
            getFunctionName.getDisplayName,
            i),
          visitor
        )
      )
      actualArgumentTypes(i) = SequenceType.makeSequenceType(
        getArg(i).getItemType,
        getArg(i).getCardinality)
    }
    resultType = definition.getResultType(actualArgumentTypes)
    if (state == 0)
      function.supplyStaticContext(visitor.getStaticContext, 0, getArguments)
    state += 1
  }

  override def typeCheck(
    visitor     : ExpressionVisitor,
    contextInfo : ContextItemStaticInfo
  ): Expression =
    super.typeCheck(visitor, contextInfo) match {
      case integratedFunctionCall: IntegratedFunctionCall =>
        val exp2 = integratedFunctionCall
          .function
          .rewrite(visitor.getStaticContext, getArguments)
        if (exp2 == null) {
          integratedFunctionCall
        } else {
          ExpressionTool.copyLocationInfo(this, exp2)
          exp2
            .simplify()
            .typeCheck(visitor, contextInfo)
            .optimize(visitor, contextInfo)
        }
      case exp =>
        exp
    }

  override def preEvaluate(visitor: ExpressionVisitor): Expression = this

  def getItemType: ItemType = resultType.getPrimaryType

  def computeCardinality(): Int = resultType.getCardinality

  override def getIntrinsicDependencies: Int = {
    val definition = function.getDefinition
    if (definition.dependsOnFocus())
      StaticProperty.DEPENDS_ON_FOCUS
    else
      0
  }

  override def computeSpecialProperties(): Int = {
    val definition = function.getDefinition
    if (definition.hasSideEffects)
      StaticProperty.HAS_SIDE_EFFECTS
    else
      StaticProperty.NO_NODES_NEWLY_CREATED
  }

  def copy(rebindings: RebindingMap): Expression = {
    val newCall = function.getDefinition.makeCallExpression()
    newCall.setDefinition(function.getDefinition)
    function.copyLocalData(newCall)
    val copy = new IntegratedFunctionCall(getFunctionName, newCall)
    val args = Array.ofDim[Expression](getArity)
    for (i <- args.indices)
      args(i) = getArg(i).copy(rebindings)
    copy.setArguments(args)
    copy.resultType = resultType
    copy.state = state
    ExpressionTool.copyLocationInfo(this, copy)
    copy
  }

  override def export(out: ExpressionPresenter): Unit = {
    out.startElement("ifCall", this)
    out.emitAttribute("name", getFunctionName)
    out.emitAttribute("type", resultType.toAlphaCode)
    for (o <- operands.asScala)
      o.getChildExpression.export(out)
    out.endElement()
  }

  override def iterate(context: XPathContext): SequenceIterator = {
    val definition = function.getDefinition
    val argValues = Array.ofDim[Sequence](getArity)
    for (i <- argValues.indices)
      argValues(i) = SequenceTool.toLazySequence(getArg(i).iterate(context))
    val role = new RoleDiagnostic(
      RoleDiagnostic.FUNCTION_RESULT,
      getFunctionName.getDisplayName,
      0)
    val config = context.getConfiguration
    val th = config.getTypeHierarchy
    var result: SequenceIterator = null
    try
      result = function.call(context, argValues).iterate()
    catch {
      case e: XPathException => {
        e.maybeSetLocation(getLocation)
        throw e
      }

    }
    if (! definition.trustResultType()) {
      val card = resultType.getCardinality
      if (card != StaticProperty.ALLOWS_ZERO_OR_MORE)
        result = new CardinalityCheckingIterator(result, card, role, getLocation)
      val `type` = resultType.getPrimaryType
      if (`type` != AnyItemType) {
        result = new ItemMappingIterator(
          result,
          item => {
            if (!`type`.matches(item, th)) {
              val msg: String = role.composeErrorMessage(`type`, item, th)
              val err: XPathException = new XPathException(msg, "XPTY0004")
              err.setLocation(getLocation)
              throw err
            }
            item
          },
          true
        )
      }
      if (th.relationship(`type`, AnyNodeTest) != Affinity.DISJOINT) {
        result = new ItemMappingIterator(
          result,
          new ConfigurationCheckingFunction(context.getConfiguration),
          true)
      }
    }
    result
  }

  override def effectiveBooleanValue(context: XPathContext): Boolean = {
    val argValues: Array[Sequence] = Array.ofDim[Sequence](getArity)
    for (i <- argValues.indices)
      argValues(i) = SequenceTool.toLazySequence(getArg(i).iterate(context))
    try
      function.effectiveBooleanValue(context, argValues)
    catch {
      case e: XPathException =>
        e.maybeSetLocation(getLocation)
        throw e
    }
  }

  def call(context: XPathContext, arguments: Array[Sequence]): Sequence =
    function.call(context, arguments)
}
