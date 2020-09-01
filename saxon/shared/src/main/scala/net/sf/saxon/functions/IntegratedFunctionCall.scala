package net.sf.saxon.functions

import net.sf.saxon.utils.Configuration

import net.sf.saxon.expr._

import net.sf.saxon.expr.parser._

import net.sf.saxon.lib.ExtensionFunctionCall

import net.sf.saxon.lib.ExtensionFunctionDefinition

import net.sf.saxon.model.Affinity

import net.sf.saxon.model.AnyItemType

import net.sf.saxon.model.ItemType

import net.sf.saxon.model.TypeHierarchy

import net.sf.saxon.om._

import net.sf.saxon.pattern.AnyNodeTest

import net.sf.saxon.trace.ExpressionPresenter

import net.sf.saxon.trans.SaxonErrorCode

import net.sf.saxon.trans.XPathException

import net.sf.saxon.value.SequenceType

import IntegratedFunctionCall._

import scala.beans.{BeanProperty, BooleanBeanProperty}

import scala.jdk.CollectionConverters._

object IntegratedFunctionCall {

  class ConfigurationCheckingFunction(private var config: Configuration)
    extends ItemMappingFunction {

    def mapItem(item: Item): Item = {
      if (item.isInstanceOf[NodeInfo] &&
        !config.isCompatible(item.asInstanceOf[NodeInfo].getConfiguration)) {
        throw new XPathException(
          "Node returned by extension function was created with an incompatible Configuration",
          SaxonErrorCode.SXXP0004)
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

  def setResultType(resultType: SequenceType): Unit = {
    this.resultType = resultType
  }

  override def getFunctionName(): StructuredQName = name

  override def getTargetFunction(context: XPathContext): Function = null

  override def checkArguments(visitor: ExpressionVisitor): Unit = {
    val definition: ExtensionFunctionDefinition = function.getDefinition
    checkArgumentCount(definition.getMinimumNumberOfArguments,
      definition.getMaximumNumberOfArguments)
    val args: Int = getArity
    val declaredArgumentTypes: Array[SequenceType] =
      definition.getArgumentTypes
    if (declaredArgumentTypes == null || (args != 0 && declaredArgumentTypes.length == 0)) {
      throw new XPathException(
        "Integrated function " + getDisplayName + " failed to declare its argument types")
    }
    val actualArgumentTypes: Array[SequenceType] =
      Array.ofDim[SequenceType](args)
    val tc: TypeChecker = visitor.getConfiguration.getTypeChecker(false)
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
    if (state == 0) {
      function.supplyStaticContext(visitor.getStaticContext, 0, getArguments)
    }
    state += 1
  }

  override def typeCheck(visitor: ExpressionVisitor,
                         contextInfo: ContextItemStaticInfo): Expression = {
    val exp: Expression = super.typeCheck(visitor, contextInfo)
    if (exp.isInstanceOf[IntegratedFunctionCall]) {
      val exp2: Expression = exp
        .asInstanceOf[IntegratedFunctionCall]
        .function
        .rewrite(visitor.getStaticContext, getArguments)
      if (exp2 == null) {
        return  exp
      } else {
        ExpressionTool.copyLocationInfo(this, exp2)
        exp2
          .simplify()
          .typeCheck(visitor, contextInfo)
          .optimize(visitor, contextInfo)
      }
    }
    exp
  }

  override def preEvaluate(visitor: ExpressionVisitor): Expression = this

  def getItemType(): ItemType = resultType.getPrimaryType

   def computeCardinality(): Int = resultType.getCardinality

  override def getIntrinsicDependencies(): Int = {
    val definition: ExtensionFunctionDefinition = function.getDefinition
    if (definition.dependsOnFocus()) StaticProperty.DEPENDS_ON_FOCUS else 0
  }

  override  def computeSpecialProperties(): Int = {
    val definition: ExtensionFunctionDefinition = function.getDefinition
    if (definition.hasSideEffects()) StaticProperty.HAS_SIDE_EFFECTS
    else StaticProperty.NO_NODES_NEWLY_CREATED
  }

  def copy(rebindings: RebindingMap): Expression = {
    val newCall: ExtensionFunctionCall =
      function.getDefinition.makeCallExpression()
    newCall.setDefinition(function.getDefinition)
    function.copyLocalData(newCall)
    val copy: IntegratedFunctionCall =
      new IntegratedFunctionCall(getFunctionName, newCall)
    val args: Array[Expression] = Array.ofDim[Expression](getArity)
    for (i <- 0 until args.length) {
      args(i) = getArg(i).copy(rebindings)
    }
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
    for (o <- operands().asScala) {
      o.getChildExpression.export(out)
    }
    out.endElement()
  }

  override def iterate(context: XPathContext): SequenceIterator = {
    val definition: ExtensionFunctionDefinition = function.getDefinition
    val argValues: Array[Sequence] = Array.ofDim[Sequence](getArity)
    for (i <- 0 until argValues.length) {
      argValues(i) = SequenceTool.toLazySequence(getArg(i).iterate(context))
    }
    val role: RoleDiagnostic = new RoleDiagnostic(
      RoleDiagnostic.FUNCTION_RESULT,
      getFunctionName.getDisplayName,
      0)
    val config: Configuration = context.getConfiguration
    val th: TypeHierarchy = config.getTypeHierarchy
    var result: SequenceIterator = null
    try result = function.call(context, argValues).iterate()
    catch {
      case e: XPathException => {
        e.maybeSetLocation(getLocation)
        throw e
      }

    }
    if (!definition.trustResultType()) {
      val card: Int = resultType.getCardinality
      if (card != StaticProperty.ALLOWS_ZERO_OR_MORE) {
        result =
          new CardinalityCheckingIterator(result, card, role, getLocation)
      }
      val `type`: ItemType = resultType.getPrimaryType
      if (`type` != AnyItemType) {
        result = new ItemMappingIterator(
          result,
          (item) => {
            if (!`type`.matches(item, th)) {
              var msg: String = role.composeErrorMessage(`type`, item, th)
              var err: XPathException = new XPathException(msg, "XPTY0004")
              err.setLocation(getLocation)
              throw err
            }
            item
          },
          true
        )
      }
      if (th.relationship(`type`, AnyNodeTest.getInstance) != Affinity.DISJOINT) {
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
    for (i <- 0 until argValues.length) {
      argValues(i) = SequenceTool.toLazySequence(getArg(i).iterate(context))
    }
    try function.effectiveBooleanValue(context, argValues)
    catch {
      case e: XPathException => {
        e.maybeSetLocation(getLocation)
        throw e
      }

    }
  }

  def call(context: XPathContext, arguments: Array[Sequence]): Sequence =
    function.call(context, arguments)

}
