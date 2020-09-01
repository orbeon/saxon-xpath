package net.sf.saxon.expr.instruct

import net.sf.saxon.event.Outputter

import net.sf.saxon.expr.Expression

import net.sf.saxon.expr.Operand

import net.sf.saxon.expr.StaticProperty

import net.sf.saxon.expr.XPathContext

import net.sf.saxon.expr.parser.ExpressionTool

import net.sf.saxon.model.ItemType

import net.sf.saxon.model.Type

import net.sf.saxon.om.Item

import net.sf.saxon.om.SequenceIterator

import net.sf.saxon.om.StandardNames

import net.sf.saxon.s9api.Location

import net.sf.saxon.trans.XPathException

import net.sf.saxon.tree.iter.EmptyIterator

import net.sf.saxon.tree.iter.SingletonIterator

import javax.xml.transform.SourceLocator

import scala.jdk.CollectionConverters._

import Expression._

object Instruction {

  def dynamicError(loc: Location,
                   error: XPathException,
                   context: XPathContext): XPathException = {
    if (error.isInstanceOf[TerminationException]) {
      return error
    }
    error.maybeSetLocation(loc)
    error.maybeSetContext(context)
    error
  }

  def assembleParams(context: XPathContext,
                     actualParams: Array[WithParam]): ParameterSet = {
    if (actualParams == null || actualParams.length == 0) {
      return null
    }
    val params: ParameterSet = new ParameterSet(actualParams.length)
    for (actualParam <- actualParams) {
      params.put(actualParam.getVariableQName,
        actualParam.getSelectValue(context),
        actualParam.isTypeChecked)
    }
    params
  }

  def assembleTunnelParams(context: XPathContext,
                           actualParams: Array[WithParam]): ParameterSet = {
    val existingParams: ParameterSet = context.getTunnelParameters
    if (existingParams == null) {
      assembleParams(context, actualParams)
    }
    if (actualParams == null || actualParams.length == 0) {
      return existingParams
    }
    val newParams: ParameterSet =
      new ParameterSet(existingParams, actualParams.length)
    for (actualParam <- actualParams) {
      newParams.put(actualParam.getVariableQName,
        actualParam.getSelectValue(context),
        checked = false)
    }
    newParams
  }

}

abstract class Instruction extends Expression with TailCallReturner {

  def getImplementationMethod(): Int = Expression.PROCESS_METHOD

  override def isInstruction(): Boolean = true

  def getInstructionNameCode(): Int = -1

  override def getExpressionName(): String = {
    val code: Int = getInstructionNameCode
    if (code >= 0 & code < 1024) {
      StandardNames.getDisplayName(code)
    } else {
      super.getExpressionName
    }
  }

  def getItemType(): ItemType = Type.ITEM_TYPE

  def computeCardinality(): Int = StaticProperty.ALLOWS_ZERO_OR_MORE

  override def operands(): java.lang.Iterable[Operand] = super.operands()

  def processLeavingTail(output: Outputter, context: XPathContext): TailCall

  override def process(output: Outputter, context: XPathContext): Unit = {
    try {
      var tc: TailCall = processLeavingTail(output, context)
      while (tc != null) tc = tc.processLeavingTail()
    } catch {
      case err: XPathException => {
        err.maybeSetFailingExpression(this)
        err.maybeSetContext(context)
        throw err
      }

    }
  }

  def getSourceLocator(): SourceLocator = getLocation

  override def computeSpecialProperties(): Int = {
    var p: Int = super.computeSpecialProperties()
    if (alwaysCreatesNewNodes()) {
      p |= StaticProperty.ALL_NODES_NEWLY_CREATED
    }
    if (mayCreateNewNodes()) {
      p
    } else {
      p | StaticProperty.NO_NODES_NEWLY_CREATED
    }
  }

  override def getNetCost(): Int = 20

  def mayCreateNewNodes(): Boolean = false

  def alwaysCreatesNewNodes(): Boolean = false

  def someOperandCreatesNewNodes(): Boolean = {
    for (o <- operands().asScala) {
      val child = o.getChildExpression
      val props: Int = child.getSpecialProperties
      if ((props & StaticProperty.NO_NODES_NEWLY_CREATED) == 0) {
        true
      }
    }
    false
  }

  override def evaluateItem(context: XPathContext): Item = {
    val m: Int = getImplementationMethod
    if ((m & EVALUATE_METHOD) != 0) {
      throw new AssertionError(
        "evaluateItem() is not implemented in the subclass " +
          getClass)
    } else if ((m & ITERATE_METHOD) != 0) {
      iterate(context).next()
    } else {
      ExpressionTool.getItemFromProcessMethod(this, context)
    }
  }

  override def iterate(context: XPathContext): SequenceIterator = {
    val m: Int = getImplementationMethod
    if ((m & EVALUATE_METHOD) != 0) {
      val item: Item = evaluateItem(context)
      if (item == null) {
        EmptyIterator.emptyIterator()
      } else {
        SingletonIterator.makeIterator(item)
      }
    } else if ((m & ITERATE_METHOD) != 0) {
      throw new AssertionError(
        "iterate() is not implemented in the subclass " + getClass)
    } else {
      ExpressionTool.getIteratorFromProcessMethod(this, context)
    }
  }

  override def evaluateAsString(context: XPathContext): CharSequence = {
    val item: Item = evaluateItem(context)
    if (item == null) {
      ""
    } else {
      item.getStringValue
    }
  }

  def isXSLT(): Boolean = getPackageData.isXSLT

}
