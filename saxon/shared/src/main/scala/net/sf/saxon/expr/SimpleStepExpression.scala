////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
package net.sf.saxon.expr

import net.sf.saxon.expr.parser.ContextItemStaticInfo

import net.sf.saxon.expr.parser.ExpressionTool

import net.sf.saxon.expr.parser.ExpressionVisitor

import net.sf.saxon.expr.parser.RebindingMap

import net.sf.saxon.model.ErrorType

import net.sf.saxon.model.ItemType

import net.sf.saxon.om.AxisInfo

import net.sf.saxon.om.NodeInfo

import net.sf.saxon.om.SequenceIterator

import net.sf.saxon.trans.XPathException

import net.sf.saxon.tree.iter.EmptyIterator

import net.sf.saxon.value.SequenceType

import SimpleStepExpression._




object SimpleStepExpression {

  private var STEP_ROLE: OperandRole = new OperandRole(
    OperandRole.USES_NEW_FOCUS | OperandRole.HIGHER_ORDER,
    OperandUsage.TRANSMISSION,
    SequenceType.ANY_SEQUENCE)

}

class SimpleStepExpression(start: Expression, step: Expression)
    extends SlashExpression(start, step) {

  if (!(step.isInstanceOf[AxisExpression])) {
    throw new IllegalArgumentException()
  }

   override def getOperandRole(arg: Int): OperandRole =
    if (arg == 0) OperandRole.FOCUS_CONTROLLING_SELECT else STEP_ROLE

  /*@NotNull*/

  override def typeCheck(visitor: ExpressionVisitor,
                         contextInfo: ContextItemStaticInfo): Expression = {
    getLhs.typeCheck(visitor, contextInfo)
    val selectType: ItemType = getStart.getItemType
    if (selectType == ErrorType.getInstance) {
      Literal.makeEmptySequence
    }
    val cit: ContextItemStaticInfo =
      visitor.getConfiguration.makeContextItemStaticInfo(selectType, false)
    cit.setContextSettingExpression(getStart)
    getRhs.typeCheck(visitor, cit)
    if (!(getStep.isInstanceOf[AxisExpression])) {
      if (Literal.isEmptySequence(getStep)) {
        getStep
      }
      val se: SlashExpression = new SlashExpression(getStart, getStep)
      ExpressionTool.copyLocationInfo(this, se)
      se
    }
    if (getStart.isInstanceOf[ContextItemExpression] &&
        AxisInfo.isForwards(getStep.asInstanceOf[AxisExpression].getAxis)) {
      getStep
    }
    this
  }

  /*@NotNull*/

  override def optimize(visitor: ExpressionVisitor,
                        contextItemType: ContextItemStaticInfo): Expression =
    this

  /*@NotNull*/

  override def copy(rebindings: RebindingMap): SimpleStepExpression = {
    val exp: SimpleStepExpression = new SimpleStepExpression(
      getStart.copy(rebindings),
      getStep.copy(rebindings))
    ExpressionTool.copyLocationInfo(this, exp)
    exp
  }

  /**
    * An implementation of Expression must provide at least one of the methods evaluateItem(), iterate(), or process().
    * This method indicates which of these methods is provided directly. The other methods will always be available
    * indirectly, using an implementation that relies on one of the other methods.
    *
    * @return the implementation method, for example {@link #ITERATE_METHOD} or {@link #EVALUATE_METHOD} or
    * {@link #PROCESS_METHOD}
    */
  override def getImplementationMethod(): Int = Expression.ITERATE_METHOD

  /*@NotNull*/

  override def iterate(context: XPathContext): SequenceIterator = {
    var origin: NodeInfo = null
    try origin = getStart.evaluateItem(context).asInstanceOf[NodeInfo]
    catch {
      case e: XPathException =>
        if ("XPDY0002" == e.getErrorCodeLocalPart && !e.hasBeenReported) {
          throw new XPathException(
            "The context item for axis step " + toShortString() +
              " is absent",
            "XPDY0002",
            getLocation)
        } else {
          throw e
        }

    }
    if (origin == null) {
      EmptyIterator.getInstance
    }
    getStep.asInstanceOf[AxisExpression].iterate(origin)
  }

  override def getExpressionName(): String = "simpleStep"

}

// Copyright (c) 2018-2020 Saxonica Limited
// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is "Incompatible With Secondary Licenses", as defined by the Mozilla Public License, v. 2.0.
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
/**
  * A SimpleStepExpression is a special case of a SlashExpression in which the
  * start expression selects a single item (or nothing), and the step expression is
  * a simple AxisExpression. This is designed to avoid the costs of creating a new
  * dynamic context for expressions (common in XQuery) such as
  * for $b in EXPR return $b/title
  */
