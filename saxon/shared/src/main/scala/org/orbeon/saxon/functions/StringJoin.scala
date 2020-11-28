package org.orbeon.saxon.functions

import org.orbeon.saxon.event.Outputter
import org.orbeon.saxon.expr.parser.{ContextItemStaticInfo, ExpressionVisitor}
import org.orbeon.saxon.expr.{Expression, StaticProperty, XPathContext}
import org.orbeon.saxon.model.BuiltInAtomicType
import org.orbeon.saxon.om._
import org.orbeon.saxon.tree.util.FastStringBuffer
import org.orbeon.saxon.value.Cardinality

import scala.beans.BooleanBeanProperty


class StringJoin extends FoldingFunction with PushableFunction {

  @BooleanBeanProperty
  var returnEmptyIfEmpty: Boolean = _

  override def getCardinality(arguments: Array[Expression]): Int =
    if (returnEmptyIfEmpty) {
      StaticProperty.ALLOWS_ZERO_OR_ONE
    } else {
      StaticProperty.EXACTLY_ONE
    }

  override def makeOptimizedFunctionCall(
                                          visitor: ExpressionVisitor,
                                          contextInfo: ContextItemStaticInfo,
                                          arguments: Expression*): Expression = {
    val e2 = super.makeOptimizedFunctionCall(visitor, contextInfo, arguments: _*)
    if (e2 != null)
      return e2
    val card = arguments(0).getCardinality
    if (! Cardinality.allowsMany(card)) {
      if (Cardinality.allowsZero(card) || arguments(0).getItemType.getPrimitiveItemType != BuiltInAtomicType.STRING)
        SystemFunction.makeCall("string", getRetainedStaticContext, arguments(0))
      else
        arguments(0)
    } else
      null
  }

  override def getFold(context: XPathContext,
                       additionalArguments: Sequence*): Fold = {
    var separator: CharSequence = ""
    if (additionalArguments.nonEmpty)
      separator = additionalArguments(0).head.getStringValueCS
    new StringJoinFold(separator)
  }

  override def process(destination: Outputter,
                       context: XPathContext,
                       arguments: Array[Sequence]): Unit = {
    val separator = if (arguments.length > 1) arguments(1).head.getStringValueCS else ""
    val output = destination.getStringReceiver(false)
    output.open()
    var first = true
    val iter = arguments(0).iterate()
    var it: Item = null
    while ({
      it = iter.next()
      it
    } != null) {
      if (first)
        first = false
      else
        output.cat(separator)
      output.cat(it.getStringValueCS)
    }
    output.close()
  }

  private class StringJoinFold(private var separator: CharSequence)
    extends Fold {

    private var position: Int = 0
    private val data = new FastStringBuffer(FastStringBuffer.C64)

    def processItem(item: Item): Unit = {
      if (position == 0) {
        data.cat(item.getStringValueCS)
        position = 1
      } else {
        data.cat(separator).append(item.getStringValueCS)
      }
    }

    def isFinished: Boolean = false

    def result(): ZeroOrOne[_ <: Item] =
      if (position == 0 && returnEmptyIfEmpty)
        ZeroOrOne.empty
      else
        One.string(data.toString)
  }

  override def equals(o: Any): Boolean =
    o.isInstanceOf[StringJoin] &&
      super.equals(o)          &&
      returnEmptyIfEmpty == o.asInstanceOf[StringJoin].returnEmptyIfEmpty

  override def getCompilerName: String = "StringJoinCompiler"
}
