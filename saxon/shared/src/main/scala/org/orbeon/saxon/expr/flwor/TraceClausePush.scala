package org.orbeon.saxon.expr.flwor

import org.orbeon.saxon.utils.Controller
import org.orbeon.saxon.event.Outputter
import org.orbeon.saxon.expr.XPathContext
import org.orbeon.saxon.trans.XPathException
import java.util.Collections

import org.orbeon.saxon.lib.TraceListener

class TraceClausePush(outputter: Outputter,
                      private var destination: TuplePush,
                      var traceClause: TraceClause,
                      private var baseClause: Clause)
  extends TuplePush(outputter) {

  override def processTuple(context: XPathContext): Unit = {
    val controller: Controller = context.getController
    if (controller.isTracing) {
      val baseInfo: ClauseInfo = new ClauseInfo(baseClause)
      baseInfo.setNamespaceResolver(traceClause.getNamespaceResolver)
      controller.getTraceListener.enter(baseInfo,
        Collections.emptyMap(),
        context)
      destination.processTuple(context)
      controller.getTraceListener.leave(baseInfo)
    } else {
      destination.processTuple(context)
    }
  }

  override def close(): Unit = {
    destination.close()
  }

}
