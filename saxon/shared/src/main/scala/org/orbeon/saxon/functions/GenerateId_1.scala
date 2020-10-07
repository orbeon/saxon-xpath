package org.orbeon.saxon.functions

import org.orbeon.saxon.expr.Expression

import org.orbeon.saxon.expr.StaticProperty

import org.orbeon.saxon.expr.XPathContext

import org.orbeon.saxon.om.Item

import org.orbeon.saxon.om.NodeInfo

import org.orbeon.saxon.om.One

import org.orbeon.saxon.om.ZeroOrOne

import org.orbeon.saxon.tree.util.FastStringBuffer

import org.orbeon.saxon.value.AtomicValue

import org.orbeon.saxon.value.StringValue

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
