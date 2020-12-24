////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// Copyright (c) 2018-2020 Saxonica Limited
// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is "Incompatible With Secondary Licenses", as defined by the Mozilla Public License, v. 2.0.
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
package org.orbeon.saxon.expr

import org.orbeon.saxon.expr.instruct.GlobalVariable
import org.orbeon.saxon.expr.parser.RebindingMap
import org.orbeon.saxon.om.{GroundedValue, StandardNames, StructuredQName}
import org.orbeon.saxon.trace.ExpressionPresenter
import org.orbeon.saxon.trans.{SymbolicName, Visibility, XPathException}

import java.util.{HashSet, Set}


/**
 * A reference to a global variable
 */
class GlobalVariableReference private (qnameOrBinding: StructuredQName Either GlobalVariable)
  extends VariableReference(qnameOrBinding)
    with ComponentInvocation {

  var bindingSlot: Int = -1

  def this(name: StructuredQName)    = this(Left(name))
  def this(variable: GlobalVariable) = this(Right(variable))

  def copy(rebindings: RebindingMap): Expression = {
    if (binding == null)
      //System.err.println("copy unbound variable " + this);
      throw new UnsupportedOperationException("Cannot copy a variable reference whose binding is unknown")

    val ref = new GlobalVariableReference(Left(getVariableName))
    ref.copyFrom(this)
    ref
  }

  /**
   * Set the binding slot to be used. This is the offset within the binding vector of the containing
   * component where the actual target component is to be found. The target template is not held directly
   * in the invocation instruction/expression itself because it can be overridden in a using package.
   *
   * @param slot the offset in the binding vector of the containing package where the target component
   *             can be found.
   */
  def setBindingSlot(slot: Int): Unit = {
    if (bindingSlot != -1)
      throw new AssertionError("Duplicate binding slot assignment")
    bindingSlot = slot
  }

  /**
   * Get the binding slot to be used. This is the offset within the binding vector of the containing
   * component where the actual target component is to be found.
   *
   * @return the offset in the binding vector of the containing package where the target component
   *         can be found.
   */
  def getBindingSlot: Int = bindingSlot

  /**
   * Get the symbolic name of the component that this invocation references
   *
   * @return the symbolic name of the target component
   */
  def getSymbolicName: SymbolicName =
    new SymbolicName(StandardNames.XSL_VARIABLE, getVariableName)

  def setTarget(target: Component): Unit =
    binding = target.getActor.asInstanceOf[GlobalVariable]

  def getTarget: Component =
    binding.asInstanceOf[GlobalVariable].getDeclaringComponent

  def getFixedTarget: Component = {
    val c = getTarget
    val v = c.getVisibility
    if (v == Visibility.PRIVATE || v == Visibility.FINAL)
      c
    else
      null
  }

  /**
   * Evaluate this variable
   *
   * @param c the XPath dynamic context
   * @return the value of the variable
   * @throws XPathException if any error occurs
   */
  override def evaluateVariable(c: XPathContext): GroundedValue =
    if (bindingSlot >= 0) {
      if (c.getCurrentComponent == null)
        throw new AssertionError("No current component")
      val target = c.getTargetComponent(bindingSlot)
      if (target.isHiddenAbstractComponent) {
        val err = new XPathException(
          "Cannot evaluate an abstract variable (" + getVariableName.getDisplayName +
            ") with no overriding declaration",
          "XTDE3052")
        err.setLocation(getLocation)
        throw err
      }
      val p = target.getActor.asInstanceOf[GlobalVariable]
      p.evaluateVariable(c, target)
    } else {
      // code for references to final/private variables, also used in XQuery
      val b = binding.asInstanceOf[GlobalVariable]
      b.evaluateVariable(c, b.getDeclaringComponent)
    }

  override def export(out: ExpressionPresenter): Unit = {
    out.startElement("gVarRef", this)
    out.emitAttribute("name", getVariableName)
    out.emitAttribute("bSlot", "" + getBindingSlot)
    out.endElement()
  }

  def getPreconditions: Set[Expression] = {
    val pre = new HashSet[Expression]
    //pre.add(this.copy());
    pre
  }
}
