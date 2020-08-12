package net.sf.saxon.functions

import net.sf.saxon.expr.Expression

import net.sf.saxon.expr.StaticProperty

import net.sf.saxon.expr.XPathContext

import net.sf.saxon.om.Item

import net.sf.saxon.om.NodeInfo

import net.sf.saxon.om.One

import net.sf.saxon.om.ZeroOrOne

import net.sf.saxon.tree.util.FastStringBuffer

import net.sf.saxon.value.AtomicValue

import net.sf.saxon.value.StringValue

import GenerateId_1._

object GenerateId_1 {

  def generateId(node: NodeInfo): StringValue = {
    val buffer: FastStringBuffer = new FastStringBuffer(FastStringBuffer.C16)
    node.generateId(buffer)
    buffer.condense()
    new StringValue(buffer)
  }

}

class GenerateId_1 extends ScalarSystemFunction {

  override def getSpecialProperties(arguments: Array[Expression]): Int = {
    val p: Int = super.getSpecialProperties(arguments)
    p & ~StaticProperty.NO_NODES_NEWLY_CREATED
  }

  override def resultWhenEmpty(): ZeroOrOne[StringValue] = One.string("")

  override def evaluate(arg: Item, context: XPathContext): AtomicValue =
    generateId(arg.asInstanceOf[NodeInfo])

  override def getCompilerName(): String = "GenerateIdCompiler"

}
