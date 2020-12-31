package org.orbeon.saxon.expr

import org.orbeon.saxon.event.EventMonitor

import org.orbeon.saxon.event.Outputter

import org.orbeon.saxon.event.OutputterEventBuffer

import org.orbeon.saxon.event.PipelineConfiguration

import org.orbeon.saxon.expr.instruct.BreakInstr

import org.orbeon.saxon.expr.parser.ContextItemStaticInfo

import org.orbeon.saxon.expr.parser.ExpressionTool

import org.orbeon.saxon.expr.parser.ExpressionVisitor

import org.orbeon.saxon.expr.parser.RebindingMap

import org.orbeon.saxon.lib.ErrorReporter

import org.orbeon.saxon.lib.NamespaceConstant

import org.orbeon.saxon.model.ItemType

import org.orbeon.saxon.model.Type

import org.orbeon.saxon.om._

import org.orbeon.saxon.pattern.QNameTest

import org.orbeon.saxon.s9api.XmlProcessingError

import org.orbeon.saxon.trace.ExpressionPresenter

import org.orbeon.saxon.trans.XPathException

import org.orbeon.saxon.value.Cardinality

import java.util.ArrayList

import java.util.List

import TryCatch._

import scala.beans.{BeanProperty, BooleanBeanProperty}

//import scala.collection.compat._
import scala.jdk.CollectionConverters._

object TryCatch {
  class CatchClause {
    var slotNumber: Int = -1
    var catchOp: Operand = _
    var nameTest: QNameTest = _
  }
}

class TryCatch(tryExpr: Expression) extends Expression {

  private var tryOp: Operand =
    new Operand(this, tryExpr, OperandRole.SAME_FOCUS_ACTION)

  @BeanProperty
  var catchClauses: List[CatchClause] = new ArrayList()

  @BooleanBeanProperty
  var rollbackOutput: Boolean = _

  def addCatchExpression(test: QNameTest, catchExpr: Expression): Unit = {
    val clause: CatchClause = new CatchClause()
    clause.catchOp =
      new Operand(this, catchExpr, OperandRole.SAME_FOCUS_ACTION)
    clause.nameTest = test
    catchClauses.add(clause)
  }

  def getTryOperand: Operand = tryOp

  def getTryExpr: Expression = tryOp.getChildExpression

  override def isInstruction(): Boolean = true

  override def allowExtractingCommonSubexpressions(): Boolean = false

  def computeCardinality(): Int = {
    var card: Int = getTryExpr.getCardinality
    for (catchClause <- catchClauses.asScala) {
      card = Cardinality.union(
        card,
        catchClause.catchOp.getChildExpression.getCardinality)
    }
    card
  }

  def getItemType: ItemType = {
    var `type`: ItemType = getTryExpr.getItemType
    for (catchClause <- catchClauses.asScala) {
      `type` = Type.getCommonSuperType(
        `type`,
        catchClause.catchOp.getChildExpression.getItemType)
    }
    `type`
  }

  override def operands: java.lang.Iterable[Operand] = {
    val list: List[Operand] = new ArrayList[Operand]()
    list.add(tryOp)
    for (cc <- catchClauses.asScala) {
      list.add(cc.catchOp)
    }
    list
  }

  override def optimize(visitor: ExpressionVisitor,
                        contextInfo: ContextItemStaticInfo): Expression = {
    optimizeChildren(visitor, contextInfo)
    var e: Expression = getParentExpression
    while (e != null) {
      if (e.isInstanceOf[LetExpression] &&
        ExpressionTool.dependsOnVariable(
          getTryExpr,
          Array(e.asInstanceOf[LetExpression]))) {
        e.asInstanceOf[LetExpression].setNeedsEagerEvaluation(true)
      }
      e = e.getParentExpression
    }
    this
  }

  override def getImplementationMethod: Int = Expression.ITERATE_METHOD

  override def equals(other: Any): Boolean =
    other.isInstanceOf[TryCatch] &&
      other
        .asInstanceOf[TryCatch]
        .tryOp
        .getChildExpression
        .isEqual(tryOp.getChildExpression) &&
      other.asInstanceOf[TryCatch].catchClauses == catchClauses

override  def computeHashCode(): Int = {
    var h: Int = 0x836b12a0
    for (i <- 0 until catchClauses.size) {
      h ^= catchClauses.get(i).hashCode << i
    }
    h + tryOp.getChildExpression.hashCode
  }

  def copy(rebindings: RebindingMap): Expression = {
    val t2: TryCatch = new TryCatch(tryOp.getChildExpression.copy(rebindings))
    for (clause <- catchClauses.asScala) {
      t2.addCatchExpression(clause.nameTest,
        clause.catchOp.getChildExpression.copy(rebindings))
    }
    t2.setRollbackOutput(rollbackOutput)
    ExpressionTool.copyLocationInfo(this, t2)
    t2
  }

override  def evaluateItem(c: XPathContext): Item = {
    val c1: XPathContext = c.newMinorContext()
    try ExpressionTool.eagerEvaluate(tryOp.getChildExpression, c1).head
    catch {
      case err: XPathException => {
        if (err.isGlobalError) {
          err.setIsGlobalError(false)
        } else {
          var code: StructuredQName = err.getErrorCodeQName
          if (code == null) {
            code =
              new StructuredQName("saxon", NamespaceConstant.SAXON, "XXXX9999")
          }
          for (clause <- catchClauses.asScala if clause.nameTest.matches(code)) {
            val caught: Expression = clause.catchOp.getChildExpression
            val c2: XPathContextMajor = c.newContext()
            c2.setCurrentException(err)
            return caught.evaluateItem(c2)
          }
        }
        err.setHasBeenReported(false)
        throw err
      }

    }
  }

override  def iterate(c: XPathContext): SequenceIterator = {
    val c1: XPathContextMajor = c.newContext()
    c1.createThreadManager()
    c1.setErrorReporter(new FilteringErrorReporter(c.getErrorReporter))
    try {
      val v: Sequence =
        ExpressionTool.eagerEvaluate(tryOp.getChildExpression, c1)
      c1.waitForChildThreads()
      val tci: TailCallLoop.TailCallInfo = c1.getTailCallInfo
      if (tci.isInstanceOf[BreakInstr]) {
        tci.asInstanceOf[BreakInstr].markContext(c)
      }
      v.iterate()
    } catch {
      case err: XPathException => {
        if (err.isGlobalError) {
          err.setIsGlobalError(false)
        } else {
          var code: StructuredQName = err.getErrorCodeQName
          if (code == null) {
            code =
              new StructuredQName("saxon", NamespaceConstant.SAXON, "XXXX9999")
          }
          for (clause <- catchClauses.asScala if clause.nameTest.matches(code)) {
            val caught: Expression = clause.catchOp.getChildExpression
            val c2: XPathContextMajor = c.newContext()
            c2.setCurrentException(err)
            val v: Sequence = ExpressionTool.eagerEvaluate(caught, c2)
            val tci: TailCallLoop.TailCallInfo = c2.getTailCallInfo
            if (tci.isInstanceOf[BreakInstr]) {
              tci.asInstanceOf[BreakInstr].markContext(c)
            }
            return v.iterate()
          }
        }
        err.setHasBeenReported(false)
        throw err
      }

    }
  }

  override def process(output: Outputter, context: XPathContext): Unit = {
    val pipe: PipelineConfiguration = output.getPipelineConfiguration
    val c1: XPathContextMajor = context.newContext()
    c1.createThreadManager()
    c1.setErrorReporter(new FilteringErrorReporter(context.getErrorReporter))
    var o2: Outputter = null
    if (rollbackOutput) {
      o2 = new OutputterEventBuffer()
      o2.setPipelineConfiguration(pipe)
    } else {
      o2 = new EventMonitor(output)
      o2.setPipelineConfiguration(pipe)
    }
    try {
      tryOp.getChildExpression.process(o2, c1)
      c1.waitForChildThreads()
      val tci: TailCallLoop.TailCallInfo = c1.getTailCallInfo
      if (tci.isInstanceOf[BreakInstr]) {
        tci.asInstanceOf[BreakInstr].markContext(context)
      }
      if (rollbackOutput) {
        o2.asInstanceOf[OutputterEventBuffer].replay(output)
      }
    } catch {
      case err: XPathException => {
        if (err.isGlobalError) {
          err.setIsGlobalError(false)
        } else {
          var code: StructuredQName = err.getErrorCodeQName
          if (code == null) {
            code =
              new StructuredQName("saxon", NamespaceConstant.SAXON, "XXXX9999")
          }
          for (clause <- catchClauses.asScala if clause.nameTest.matches(code)) {
            if (o2.isInstanceOf[EventMonitor] && o2
              .asInstanceOf[EventMonitor]
              .hasBeenWrittenTo) {
              val message: String = err.getMessage +
                ". The error could not be caught, because rollback-output=no was specified, and output was already written to the result tree"
              val xe: XPathException = new XPathException(message, "XTDE3530")
              xe.setLocation(err.getLocator)
              xe.setXPathContext(context)
              throw xe
            }
            val caught: Expression = clause.catchOp.getChildExpression
            val c2: XPathContextMajor = context.newContext()
            c2.setCurrentException(err)
            val v: Sequence = ExpressionTool.eagerEvaluate(caught, c2)
            val tci: TailCallLoop.TailCallInfo = c2.getTailCallInfo
            if (tci.isInstanceOf[BreakInstr]) {
              tci.asInstanceOf[BreakInstr].markContext(context)
            }
            v.asInstanceOf[GroundedValue]
              .iterate()
              .forEachOrFail((item) => output.append(item))
            return
          }
        }
        err.setHasBeenReported(false)
        throw err
      }

    }
  }

  override def getExpressionName: String = "tryCatch"

  def export(out: ExpressionPresenter): Unit = {
    out.startElement("try", this)
    if (rollbackOutput) {
      out.emitAttribute("flags", "r")
    }
    tryOp.getChildExpression.export(out)
    for (clause <- catchClauses.asScala) {
      out.startElement("catch")
      out.emitAttribute("errors", clause.nameTest.exportQNameTest)
      clause.catchOp.getChildExpression.export(out)
      out.endElement()
    }
    out.endElement()
  }

  private class FilteringErrorReporter(private var base: ErrorReporter)
    extends ErrorReporter {

    private def isCaught(err: XmlProcessingError): Boolean = {
      val code: StructuredQName = err.getErrorCode.getStructuredQName
      for (clause <- catchClauses.asScala if clause.nameTest.matches(code)) {
        return true
      }
      false
    }

    override def report(error: XmlProcessingError): Unit = {
      if (error.isWarning || !isCaught(error)) {
        base.report(error)
      }
    }

  }

  override def getStreamerName: String = "TryCatch"

}
