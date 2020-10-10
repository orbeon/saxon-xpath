////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
package org.orbeon.saxon.expr.flwor

import org.orbeon.saxon.event.Outputter

import org.orbeon.saxon.expr._

import org.orbeon.saxon.expr.parser._

import org.orbeon.saxon.model.ItemType

import org.orbeon.saxon.trace.ExpressionPresenter

import org.orbeon.saxon.trans.XPathException

import org.orbeon.saxon.tree.util.FastStringBuffer

import java.util.List

import org.orbeon.saxon.expr.flwor.Clause.ClauseName.LET

import scala.beans.{BeanProperty, BooleanBeanProperty}
import Clause.ClauseName.ClauseName

//import scala.collection.compat._
import scala.jdk.CollectionConverters._

/**
 * A "let" clause in a FLWOR expression
 */
class LetClause extends Clause {

  @BeanProperty
  var rangeVariable: LocalVariableBinding = _

  private var sequenceOp: Operand = _

  @BeanProperty
  var evaluator: Evaluator =
    ExpressionTool.lazyEvaluator(getSequence, repeatable = true)

  override def getClauseKey(): ClauseName = LET

  def copy(flwor: FLWORExpression, rebindings: RebindingMap): LetClause = {
    val let2: LetClause = new LetClause()
    let2.setLocation(getLocation)
    let2.setPackageData(getPackageData)
    let2.rangeVariable = rangeVariable.copy()
    let2.initSequence(flwor, getSequence.copy(rebindings))
    let2
  }

  def initSequence(flwor: FLWORExpression, sequence: Expression): Unit = {
    sequenceOp = new Operand(
      flwor,
      sequence,
      if (isRepeated) OperandRole.REPEAT_NAVIGATE else OperandRole.NAVIGATE)
  }

  def setSequence(sequence: Expression): Unit = {
    sequenceOp.setChildExpression(sequence)
  }

  def getSequence: Expression = sequenceOp.getChildExpression

  /**
   * Get the number of variables bound by this clause
   *
   * @return the number of variable bindings
   */
  override def getRangeVariables(): Array[LocalVariableBinding] =
    Array(rangeVariable)

  override def getPullStream(base: TuplePull,
                             context: XPathContext): TuplePull =
    new LetClausePull(base, this)

  /**
   * Get a push-mode tuple stream that implements the functionality of this clause, supplying its
   * output to another tuple stream
   *
   * @param destination the output tuple stream
   * @param output      the destination for the result
   * @param context
   * @return the push tuple stream that implements the functionality of this clause of the FLWOR
   *         expression
   */
  override def getPushStream(destination: TuplePush,
                             output: Outputter,
                             context: XPathContext): TuplePush =
    new LetClausePush(output, destination, this)

  /**
   * Process the subexpressions of this clause
   *
   * @param processor the expression processor used to process the subexpressions
   */
  override def processOperands(processor: OperandProcessor): Unit = {
    processor.processOperand(sequenceOp)
  }

  override def typeCheck(visitor: ExpressionVisitor,
                         contextInfo: ContextItemStaticInfo): Unit = {
    val role: RoleDiagnostic = new RoleDiagnostic(
      RoleDiagnostic.VARIABLE,
      rangeVariable.getVariableQName.getDisplayName,
      0)
    this.setSequence(TypeChecker.strictTypeCheck(getSequence,
      rangeVariable.getRequiredType,
      role,
      visitor.getStaticContext))
    evaluator = ExpressionTool.lazyEvaluator(getSequence, repeatable = true)
  }

  override def gatherVariableReferences(
                                         visitor: ExpressionVisitor,
                                         binding: Binding,
                                         references: List[VariableReference]): Unit = {
    ExpressionTool.gatherVariableReferences(getSequence, binding, references)
  }

  override def refineVariableType(visitor: ExpressionVisitor,
                                  references: List[VariableReference],
                                  returnExpr: Expression): Unit = {
    val seq: Expression = getSequence
    val actualItemType: ItemType = seq.getItemType
    for (ref <- references.asScala) {
      ref.refineVariableType(actualItemType,
        getSequence.getCardinality,
        if (seq.isInstanceOf[Literal])
          seq.asInstanceOf[Literal].getValue
        else null,
        seq.getSpecialProperties)
      ExpressionTool.resetStaticProperties(returnExpr)
    }
  }

  //    }
  override def addToPathMap(pathMap: PathMap,
                            pathMapNodeSet: PathMap.PathMapNodeSet): Unit = {
    val varPath: PathMap.PathMapNodeSet =
      getSequence.addToPathMap(pathMap, pathMapNodeSet)
    pathMap.registerPathForVariable(rangeVariable, varPath)
  }

  /**
   * Diagnostic print of expression structure. The abstract expression tree
   * is written to the supplied output destination.
   *
   * @param out the expression presenter used to display the structure
   */
  override def explain(out: ExpressionPresenter): Unit = {
    out.startElement("let")
    out.emitAttribute("var", getRangeVariable.getVariableQName)
    out.emitAttribute("slot", getRangeVariable.getLocalSlotNumber.toString)
    getSequence.export(out)
    out.endElement()
  }

  override def toShortString: String = {
    val fsb = new FastStringBuffer(FastStringBuffer.C64)
    fsb.append("let $")
    fsb.append(rangeVariable.getVariableQName.getDisplayName)
    fsb.append(" := ")
    fsb.append(getSequence.toShortString)
    fsb.toString
  }

  override def toString: String = {
    val fsb = new FastStringBuffer(FastStringBuffer.C64)
    fsb.append("let $")
    fsb.append(rangeVariable.getVariableQName.getDisplayName)
    fsb.append(" := ")
    fsb.append(getSequence.toString)
    fsb.toString
  }

}

// Copyright (c) 2018-2020 Saxonica Limited
// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is "Incompatible With Secondary Licenses", as defined by the Mozilla Public License, v. 2.0.
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
