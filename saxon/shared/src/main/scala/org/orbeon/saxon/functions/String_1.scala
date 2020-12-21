package org.orbeon.saxon.functions

import org.orbeon.saxon.expr.{Expression, StaticProperty, XPathContext}
import org.orbeon.saxon.expr.instruct.SimpleNodeConstructor
import org.orbeon.saxon.expr.parser.{ContextItemStaticInfo, ExpressionVisitor}
import org.orbeon.saxon.model.BuiltInAtomicType
import org.orbeon.saxon.om.{Item, One, ZeroOrOne}
import org.orbeon.saxon.value.{AtomicValue, StringValue}


class String_1 extends ScalarSystemFunction {

  override def evaluate(arg: Item, context: XPathContext): AtomicValue = {
    var result: CharSequence = null
    result = arg.getStringValueCS
    new StringValue(result)
  }

  override def resultWhenEmpty(): ZeroOrOne[_ <: Item] = new One(StringValue.EMPTY_STRING)

  override def makeOptimizedFunctionCall(
                                          visitor: ExpressionVisitor,
                                          contextInfo: ContextItemStaticInfo,
                                          arguments: Expression*): Expression = {
    val th = visitor.getConfiguration.getTypeHierarchy
    val arg = arguments(0)
    if (th.isSubType(arg.getItemType, BuiltInAtomicType.STRING) && arg.getCardinality == StaticProperty.EXACTLY_ONE)
      return arg
    arg match {
      case constructor: SimpleNodeConstructor =>
        return constructor.getSelect
      case _ =>
    }
    null
  }

  override def getCompilerName: String = "StringFnCompiler"

  override def getStreamerName: String = "StringFn"

}
