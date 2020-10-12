package org.orbeon.saxon.functions

import org.orbeon.saxon.utils.Controller

import org.orbeon.saxon.expr.Expression

import org.orbeon.saxon.expr.XPathContext

import org.orbeon.saxon.expr.parser.Loc

import org.orbeon.saxon.lib.Logger

import org.orbeon.saxon.lib.TraceListener

import org.orbeon.saxon.ma.arrays.ArrayItem

import org.orbeon.saxon.ma.map.MapItem

import org.orbeon.saxon.model.Type

import org.orbeon.saxon.om._

import org.orbeon.saxon.s9api.Location

import org.orbeon.saxon.trace.Traceable

import org.orbeon.saxon.trans.Err

import org.orbeon.saxon.tree.util.Navigator

import org.orbeon.saxon.value.AtomicValue

import org.orbeon.saxon.value.ObjectValue

import java.util.HashMap

import java.util.Map

import Trace._

object Trace {

  def traceItem(`val`: Item, label: String, out: Logger): Unit = {
    if (`val` == null) {
      out.info(label)
    } else {
      if (`val`.isInstanceOf[NodeInfo]) {
        out.info(
          label + ": " + Type.displayTypeName(`val`) + ": " + Navigator
            .getPath(`val`.asInstanceOf[NodeInfo]))
      } else if (`val`.isInstanceOf[AtomicValue]) {
        out.info(
          label + ": " + Type
            .displayTypeName(`val`) + ": " + `val`.getStringValue)
      } else if (`val`.isInstanceOf[ArrayItem] || `val`
        .isInstanceOf[MapItem]) {
        out.info(label + ": " + `val`.toShortString)
      } else if (`val`.isInstanceOf[Function]) {
        val name: StructuredQName =
          `val`.asInstanceOf[Function].getFunctionName
        out.info(
          label + ": function " +
            (if (name == null) "(anon)" else name.getDisplayName) +
            "#" +
            `val`.asInstanceOf[Function].getArity)
      } else if (`val`.isInstanceOf[ObjectValue[_]]) {
        val obj: AnyRef = `val`.asInstanceOf[ObjectValue[_]].getObject.asInstanceOf[AnyRef]
        out.info(
          label + ": " + obj.getClass.getName + " = " + Err.truncate30(
            obj.toString))
      } else {
        out.info(label + ": " + `val`.toShortString)
      }
    }
  }

}

class Trace extends SystemFunction with Traceable {

  var location: Location = Loc.NONE

  override def getSpecialProperties(arguments: Array[Expression]): Int =
    arguments(0).getSpecialProperties

  override def getCardinality(arguments: Array[Expression]): Int =
    arguments(0).getCardinality

  def notifyListener(label: String,
                     `val`: Sequence,
                     context: XPathContext): Unit = {
    val info: Map[String, Any] = new HashMap[String, Any]()
    info.put("label", label)
    info.put("value", `val`)
    val listener: TraceListener = context.getController.getTraceListener
    listener.enter(this, info, context)
    listener.leave(this)
  }

  override def makeFunctionCall(arguments: Expression*): Expression = {
    val e: Expression = super.makeFunctionCall(arguments: _*)
    location = e.getLocation
    e
  }

  override def getLocation: Location = location

  def call(context: XPathContext, arguments: Array[Sequence]): Sequence = {
    val controller: Controller = context.getController
    val label: String =
      if (arguments.length == 1) "*" else arguments(1).head.getStringValue
    if (controller.isTracing) {
      val value: Sequence = arguments(0).iterate().materialize
      notifyListener(label, value, context)
      value
    } else {
      val out: Logger = controller.getTraceFunctionDestination
      if (out == null) {
        arguments(0)
      } else {
        SequenceTool.toLazySequence(
          new TracingIterator(arguments(0).iterate(), label, out))
      }
    }
  }

  override def getObjectName: StructuredQName = null

  private class TracingIterator(private var base: SequenceIterator,
                                private var label: String,
                                private var out: Logger)
    extends SequenceIterator {

    private var empty: Boolean = true

    private var position: Int = 0

    def next(): Item = {
      val n: Item = base.next()
      position += 1
      if (n == null) {
        if (empty) {
          traceItem(null, label + ": empty sequence", out)
        }
      } else {
        traceItem(n, label + " [" + position + ']', out)
        empty = false
      }
      n
    }

    override def close(): Unit = {
      base.close()
    }

  }

  override def getStreamerName: String = "Trace"

}
