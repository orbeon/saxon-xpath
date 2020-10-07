package org.orbeon.saxon.functions

import org.orbeon.saxon.expr.Expression

import org.orbeon.saxon.expr.StaticProperty

import org.orbeon.saxon.expr.XPathContext

import org.orbeon.saxon.expr.instruct.SimpleNodeConstructor

import org.orbeon.saxon.expr.parser.ContextItemStaticInfo

import org.orbeon.saxon.expr.parser.ExpressionVisitor

import org.orbeon.saxon.model.BuiltInAtomicType

import org.orbeon.saxon.model.TypeHierarchy

import org.orbeon.saxon.om.Item

import org.orbeon.saxon.om.One

import org.orbeon.saxon.om.ZeroOrOne

import org.orbeon.saxon.value.AtomicValue

import org.orbeon.saxon.value.StringValue

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
    val th: TypeHierarchy = visitor.getConfiguration.getTypeHierarchy
    val arg: Expression = arguments(0)
    if (th.isSubType(arg.getItemType, BuiltInAtomicType.STRING) &&
      arg.getCardinality == StaticProperty.EXACTLY_ONE) {
      return arg
    }
    if (arg.isInstanceOf[SimpleNodeConstructor]) {
      return arg.asInstanceOf[SimpleNodeConstructor].getSelect
    }
    null
  }

  override def getCompilerName(): String = "StringFnCompiler"

  override def getStreamerName: String = "StringFn"

}
