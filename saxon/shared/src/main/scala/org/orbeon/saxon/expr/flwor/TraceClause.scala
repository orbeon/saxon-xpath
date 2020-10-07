package org.orbeon.saxon.expr.flwor

import org.orbeon.saxon.event.Outputter

import org.orbeon.saxon.expr.XPathContext

import org.orbeon.saxon.expr.parser.PathMap

import org.orbeon.saxon.expr.parser.RebindingMap

import org.orbeon.saxon.om.NamespaceResolver

import org.orbeon.saxon.trace.ExpressionPresenter

import org.orbeon.saxon.trans.XPathException

import org.orbeon.saxon.expr.flwor.Clause.ClauseName.TRACE

import  Clause.ClauseName.ClauseName

class TraceClause(expression: FLWORExpression, private var target: Clause)
  extends Clause {

  private var nsResolver: NamespaceResolver =
    expression.getRetainedStaticContext

  def getNamespaceResolver: NamespaceResolver = nsResolver

  def setNamespaceResolver(nsResolver: NamespaceResolver): Unit = {
    this.nsResolver = nsResolver
  }

  override def getClauseKey(): ClauseName = TRACE

  def copy(flwor: FLWORExpression, rebindings: RebindingMap): TraceClause =
    new TraceClause(flwor, target)

  override def getPullStream(base: TuplePull,
                             context: XPathContext): TuplePull =
    new TraceClausePull(base, this, target)

  override def getPushStream(destination: TuplePush,
                             output: Outputter,
                             context: XPathContext): TuplePush =
    new TraceClausePush(output, destination, this, target)

  override def processOperands(processor: OperandProcessor): Unit = ()

  override def addToPathMap(pathMap: PathMap,
                            pathMapNodeSet: PathMap.PathMapNodeSet): Unit = ()

  override def explain(out: ExpressionPresenter): Unit = {
    out.startElement("trace")
    out.endElement()
  }

  override def toString: String = "trace"

}
