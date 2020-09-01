package net.sf.saxon.functions

import net.sf.saxon.event.Outputter

import net.sf.saxon.expr.Expression

import net.sf.saxon.expr.StaticProperty

import net.sf.saxon.expr.XPathContext

import net.sf.saxon.expr.parser.ContextItemStaticInfo

import net.sf.saxon.expr.parser.ExpressionVisitor

import net.sf.saxon.model.BuiltInAtomicType

import net.sf.saxon.om._

import net.sf.saxon.tree.util.CharSequenceConsumer

import net.sf.saxon.tree.util.FastStringBuffer

import net.sf.saxon.value.Cardinality

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
    val e2: Expression =
      super.makeOptimizedFunctionCall(visitor, contextInfo, arguments: _*)
    if (e2 != null) return e2
    val card: Int = arguments(0).getCardinality
    if (!Cardinality.allowsMany(card)) {
      if (Cardinality.allowsZero(card) ||
        arguments(0).getItemType.getPrimitiveItemType != BuiltInAtomicType.STRING) {
        SystemFunction.makeCall("string",
          getRetainedStaticContext,
          arguments(0))
      } else {
        arguments(0)
      }
    }
    null
  }

  override def getFold(context: XPathContext,
                       additionalArguments: Sequence*): Fold = {
    var separator: CharSequence = ""
    if (additionalArguments.length > 0) {
      separator = additionalArguments(0).head.getStringValueCS
    }
    new StringJoinFold(separator)
  }

  override def process(destination: Outputter,
                       context: XPathContext,
                       arguments: Array[Sequence]): Unit = {
    val separator: CharSequence =
      if (arguments.length > 1) arguments(1).head.getStringValueCS else ""
    val output: CharSequenceConsumer = destination.getStringReceiver(false)
    output.open()
    var first: Boolean = true
    val iter: SequenceIterator = arguments(0).iterate()
    var it: Item = null
    while (({
      it = iter.next()
      it
    }) != null) {
      if (first) {
        first = false
      } else {
        output.cat(separator)
      }
      output.cat(it.getStringValueCS)
    }
    output.close()
  }

  private class StringJoinFold(private var separator: CharSequence)
    extends Fold {

    private var position: Int = 0

    private var data: FastStringBuffer = new FastStringBuffer(
      FastStringBuffer.C64)

    def processItem(item: Item): Unit = {
      if (position == 0) {
        data.cat(item.getStringValueCS)
        position = 1
      } else {
        data.cat(separator).append(item.getStringValueCS)
      }
    }

    def isFinished(): Boolean = false

    def result(): ZeroOrOne[_ <: Item] =
      if (position == 0 && returnEmptyIfEmpty) {
        ZeroOrOne.empty()
      } else {
        One.string(data.toString)
      }

  }

  override def equals(o: Any) = o.isInstanceOf[StringJoin] && super.equals(o) && returnEmptyIfEmpty == o.asInstanceOf[StringJoin].returnEmptyIfEmpty

  override def getCompilerName(): String = "StringJoinCompiler"

  override def call(context: XPathContext, args: Array[Sequence]): Sequence = call(context, args)
}
