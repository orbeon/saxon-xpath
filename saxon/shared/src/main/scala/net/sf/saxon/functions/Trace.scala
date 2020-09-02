package net.sf.saxon.functions

import net.sf.saxon.utils.Controller

import net.sf.saxon.expr.Expression

import net.sf.saxon.expr.XPathContext

import net.sf.saxon.expr.parser.Loc

import net.sf.saxon.lib.Logger

import net.sf.saxon.lib.TraceListener

import net.sf.saxon.ma.arrays.ArrayItem

import net.sf.saxon.ma.map.MapItem

import net.sf.saxon.model.Type

import net.sf.saxon.om._

import net.sf.saxon.s9api.Location

import net.sf.saxon.trace.Traceable

import net.sf.saxon.trans.Err

import net.sf.saxon.tree.util.Navigator

import net.sf.saxon.value.AtomicValue

import net.sf.saxon.value.ObjectValue

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
      val value: Sequence = arguments(0).iterate().materialize()
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
