////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
package org.orbeon.saxon.expr.flwor

import java.util.List

import org.orbeon.saxon.event.Outputter
import org.orbeon.saxon.expr._
import org.orbeon.saxon.expr.flwor.Clause._
import org.orbeon.saxon.expr.parser.{ContextItemStaticInfo, ExpressionVisitor, PathMap, RebindingMap}
import org.orbeon.saxon.s9api.Location
import org.orbeon.saxon.trace.ExpressionPresenter
import org.orbeon.saxon.trans.XPathException
import org.orbeon.saxon.utils.Configuration

import scala.beans.BeanProperty


object Clause {

  object ClauseName extends Enumeration {

    val FOR: ClauseName = new ClauseName()
    val LET: ClauseName = new ClauseName()
    val WINDOW: ClauseName = new ClauseName()
    val GROUP_BY: ClauseName = new ClauseName()
    val COUNT: ClauseName = new ClauseName()
    val ORDER_BY: ClauseName = new ClauseName()
    val WHERE: ClauseName = new ClauseName()
    val TRACE: ClauseName = new ClauseName()
    val FOR_MEMBER: ClauseName = new ClauseName()

    class ClauseName extends Val

    implicit def convertValue(v: Value): ClauseName =
      v.asInstanceOf[ClauseName]
  }
}

/**
 * A "Clause" refers specifically to one of the clauses of a FLWOR expression, for example the "for"
 * clause, the "let" clause, the "where" or "order by" clause. (The "return" clause, however, is not
 * modelled as a Clause).
 */
abstract class Clause {

  @BeanProperty
  var location: Location = _

  @BeanProperty
  var packageData: PackageData = _

  var repeated: Boolean = _

  def setRepeated(repeated: Boolean): Unit = this.repeated = repeated

  def isRepeated: Boolean = repeated
  def getConfiguration: Configuration = packageData.getConfiguration
  def copy(flwor: FLWORExpression, rebindings: RebindingMap): Clause

  /**
   * Optimize any expressions contained within this clause
   *
   * @param visitor         the ExpressionVisitor, providing access to static context information
   * @param contextItemType the type of the context item
   * @throws XPathException if any error is detected
   */
  def optimize(visitor: ExpressionVisitor,
               contextItemType: ContextItemStaticInfo): Unit = ()

  def typeCheck(visitor: ExpressionVisitor,
                contextInfo: ContextItemStaticInfo): Unit = ()

  def getPullStream(base: TuplePull, context: XPathContext): TuplePull

  def getPushStream(destination: TuplePush,
                    output: Outputter,
                    context: XPathContext): TuplePush

  def processOperands(processor: OperandProcessor): Unit

  def explain(out: ExpressionPresenter): Unit

  def getRangeVariables: Array[LocalVariableBinding] =
    Array.ofDim[LocalVariableBinding](0)

  def gatherVariableReferences(visitor: ExpressionVisitor,
                               binding: Binding,
                               refs: List[VariableReference]): Unit = ()

  def containsNonInlineableVariableReference(binding: Binding): Boolean = false

  def refineVariableType(visitor: ExpressionVisitor,
                         references: List[VariableReference],
                         returnExpr: Expression): Unit = ()

  def addToPathMap(pathMap: PathMap,
                   pathMapNodeSet: PathMap.PathMapNodeSet): Unit

  def getClauseKey: ClauseName.ClauseName

  def toShortString: String = toString
}