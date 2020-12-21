package org.orbeon.saxon.expr.flwor

import org.orbeon.saxon.utils.Controller
import org.orbeon.saxon.expr.XPathContext
import org.orbeon.saxon.trans.XPathException
import java.util.Collections

import org.orbeon.saxon.lib.TraceListener

class TraceClausePull(private var base: TuplePull,
                      private var traceClause: TraceClause,
                      private var baseClause: Clause)
  extends TuplePull {

  override def nextTuple(context: XPathContext): Boolean = {
    val controller: Controller = context.getController
    if (controller.isTracing) {
      val baseInfo: ClauseInfo = new ClauseInfo(baseClause)
      baseInfo.setNamespaceResolver(traceClause.getNamespaceResolver)
      controller.getTraceListener.enter(baseInfo,
        Collections.emptyMap(),
        context)
      val b: Boolean = base.nextTuple(context)
      controller.getTraceListener.leave(baseInfo)
      b
    } else {
      base.nextTuple(context)
    }
  }

  override def close(): Unit = {
    base.close()
  }

}
