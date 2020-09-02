package net.sf.saxon.functions

import net.sf.saxon.expr.Callable

import net.sf.saxon.expr.Expression

import net.sf.saxon.expr.XPathContext

import net.sf.saxon.expr.parser.ContextItemStaticInfo

import net.sf.saxon.expr.parser.ExpressionVisitor

import net.sf.saxon.model.ErrorType

import net.sf.saxon.om.FocusIterator

import net.sf.saxon.om.Function

import net.sf.saxon.om.Sequence

import net.sf.saxon.trans.SymbolicName

import net.sf.saxon.trans.XPathException

import net.sf.saxon.value.Int64Value

import net.sf.saxon.value.IntegerValue

import PositionAndLast._

import scala.beans.{BeanProperty, BooleanBeanProperty}

object PositionAndLast {

  class Position extends PositionAndLast {

    def evaluateItem(c: XPathContext): Int64Value = {
      val currentIterator: FocusIterator = c.getCurrentIterator
      if (currentIterator == null) {
        val e: XPathException = new XPathException(
          "The context item is absent, so position is undefined")
        e.setXPathContext(c)
        e.setErrorCode("XPDY0002")
        throw e
      }
      Int64Value.makeIntegerValue(currentIterator.position)
    }

    override def getCompilerName(): String = "PositionCompiler"

  }

  class Last extends PositionAndLast {

    def evaluateItem(c: XPathContext): Int64Value =
      Int64Value.makeIntegerValue(c.getLast)

    override def getCompilerName(): String = "LastCompiler"

    override def getStreamerName: String = "Last"

  }

}

abstract class PositionAndLast extends ContextAccessorFunction {

  @BooleanBeanProperty
  var contextPossiblyUndefined: Boolean = true

  override def getNetCost(): Int = 0

  override def bindContext(context: XPathContext): Function = {
    var value: Int64Value = null
    try value = evaluateItem(context)
    catch {
      case e: XPathException => {
        val name: SymbolicName.F =
          new SymbolicName.F(getFunctionName, getArity)
        val callable: Callable = (context1, arguments) => throw e
        new CallableFunction(name, callable, getFunctionItemType)
      }

    }
    val fn: ConstantFunction = new ConstantFunction(value)
    fn.setDetails(getDetails)
    fn.setRetainedStaticContext(getRetainedStaticContext)
    fn
  }

  override def getIntegerBounds(): Array[IntegerValue] =
    Array(Int64Value.PLUS_ONE, Expression.MAX_SEQUENCE_LENGTH)

  override def supplyTypeInformation(visitor: ExpressionVisitor,
                                     contextInfo: ContextItemStaticInfo,
                                     arguments: Array[Expression]): Unit = {
    super.supplyTypeInformation(visitor, contextInfo, arguments)
    if (contextInfo.getItemType == ErrorType) {
      val err = new XPathException(
        "The context item is absent at this point")
      err.setErrorCode("XPDY0002")
      throw err
    } else {
      contextPossiblyUndefined = contextInfo.isPossiblyAbsent
    }
  }

  def evaluateItem(c: XPathContext): Int64Value

  override def call(context: XPathContext, arguments: Array[Sequence]): IntegerValue =
    evaluateItem(context)

}
