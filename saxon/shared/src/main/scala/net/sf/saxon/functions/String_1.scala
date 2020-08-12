package net.sf.saxon.functions

import net.sf.saxon.expr.Expression

import net.sf.saxon.expr.StaticProperty

import net.sf.saxon.expr.XPathContext

import net.sf.saxon.expr.instruct.SimpleNodeConstructor

import net.sf.saxon.expr.parser.ContextItemStaticInfo

import net.sf.saxon.expr.parser.ExpressionVisitor

import net.sf.saxon.model.BuiltInAtomicType

import net.sf.saxon.model.TypeHierarchy

import net.sf.saxon.om.Item

import net.sf.saxon.om.One

import net.sf.saxon.om.ZeroOrOne

import net.sf.saxon.value.AtomicValue

import net.sf.saxon.value.StringValue

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

  override def getStreamerName(): String = "StringFn"

}
