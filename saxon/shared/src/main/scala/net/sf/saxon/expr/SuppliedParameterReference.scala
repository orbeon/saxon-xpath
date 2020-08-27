////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
package net.sf.saxon.expr

import net.sf.saxon.expr.parser.ContextItemStaticInfo

import net.sf.saxon.expr.parser.ExpressionTool

import net.sf.saxon.expr.parser.ExpressionVisitor

import net.sf.saxon.expr.parser.RebindingMap

import net.sf.saxon.lib.StandardDiagnostics

import net.sf.saxon.model.AnyItemType

import net.sf.saxon.model.ItemType

import net.sf.saxon.om.Item

import net.sf.saxon.om.Sequence

import net.sf.saxon.om.SequenceIterator

import net.sf.saxon.trace.ExpressionPresenter

import net.sf.saxon.trans.XPathException

import net.sf.saxon.value.SequenceType




class SuppliedParameterReference(var slotNumber: Int) extends Expression {

  var `type`: SequenceType = _

  def getSlotNumber(): Int = slotNumber

  def setSuppliedType(`type`: SequenceType): Unit = {
    this.`type` = `type`
  }

  /*@NotNull*/

  override def typeCheck(visitor: ExpressionVisitor,
                contextInfo: ContextItemStaticInfo): Expression = this

  /*@NotNull*/

  override def optimize(visitor: ExpressionVisitor,
               contextItemType: ContextItemStaticInfo): Expression = this

  /*@NotNull*/

  def getItemType(): ItemType =
    if (`type` != null) {
      `type`.getPrimaryType
    } else {
      AnyItemType.getInstance
    }

  override def getIntrinsicDependencies(): Int =
    StaticProperty.DEPENDS_ON_LOCAL_VARIABLES

  def computeCardinality(): Int =
    if (`type` != null) {
      `type`.getCardinality
    } else {
      StaticProperty.ALLOWS_ZERO_OR_MORE
    }

  /*@NotNull*/

  def copy(rebindings: RebindingMap): Expression = {
    val exp: SuppliedParameterReference = new SuppliedParameterReference(
      slotNumber)
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
  override def getImplementationMethod(): Int =
    Expression.EVALUATE_METHOD | Expression.ITERATE_METHOD

  def evaluateVariable(c: XPathContext): Sequence = {
    if (slotNumber == -1) {
      c.getStackFrame.popDynamicValue()
    }
    try c.evaluateLocalVariable(slotNumber)
    catch {
      case e: AssertionError => {
        new StandardDiagnostics()
          .printStackTrace(c, c.getConfiguration.getLogger, 2)
        throw new AssertionError(
          e.getMessage + ". No value has been set for parameter " +
            slotNumber)
      }

    }
  }

  /*@NotNull*/

  override def iterate(context: XPathContext): SequenceIterator =
    evaluateVariable(context).iterate()

  override def evaluateItem(context: XPathContext): Item =
    evaluateVariable(context).head()

  /**
    * Get a name identifying the kind of expression, in terms meaningful to a user.
    *
    * @return a name identifying the kind of expression, in terms meaningful to a user.
    * The name will always be in the form of a lexical XML QName, and should match the name used
    * in export() output displaying the expression.
    */
  override def getExpressionName(): String = "supplied"

  def export(destination: ExpressionPresenter): Unit = {
    destination.startElement("supplied", this)
    destination.emitAttribute("slot", slotNumber.toString)
    destination.endElement()
  }

  override def toString(): String = "suppliedParam(" + slotNumber + ")"

}

// Copyright (c) 2018-2020 Saxonica Limited
// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is "Incompatible With Secondary Licenses", as defined by the Mozilla Public License, v. 2.0.
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
/**
  * Supplied parameter reference: this is an internal expression used to refer to
  * the value of the n'th parameter supplied on a template call or a call to an inline function.
  * It is used within a type-checking expression designed to check the consistency
  * of the supplied value with the required type. This type checking is all done
  * at run-time, because the binding of apply-templates to actual template rules
  * is entirely dynamic.
  */
