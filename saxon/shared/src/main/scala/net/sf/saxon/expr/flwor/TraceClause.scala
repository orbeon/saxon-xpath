package net.sf.saxon.expr.flwor

import net.sf.saxon.event.Outputter

import net.sf.saxon.expr.XPathContext

import net.sf.saxon.expr.parser.PathMap

import net.sf.saxon.expr.parser.RebindingMap

import net.sf.saxon.om.NamespaceResolver

import net.sf.saxon.trace.ExpressionPresenter

import net.sf.saxon.trans.XPathException

import net.sf.saxon.expr.flwor.Clause.ClauseName.TRACE

import  Clause.ClauseName.ClauseName

class TraceClause(expression: FLWORExpression, private var target: Clause)
  extends Clause {

  private var nsResolver: NamespaceResolver =
    expression.getRetainedStaticContext

  def getNamespaceResolver(): NamespaceResolver = nsResolver

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

  override def toString(): String = "trace"

}
