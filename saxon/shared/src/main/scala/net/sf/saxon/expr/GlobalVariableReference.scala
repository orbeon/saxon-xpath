////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
package net.sf.saxon.expr

import net.sf.saxon.expr.instruct.GlobalVariable

import net.sf.saxon.expr.parser.RebindingMap

import net.sf.saxon.om.GroundedValue

import net.sf.saxon.om.StandardNames

import net.sf.saxon.om.StructuredQName

import net.sf.saxon.trace.ExpressionPresenter

import net.sf.saxon.trans.SymbolicName

import net.sf.saxon.trans.Visibility

import net.sf.saxon.trans.XPathException

import java.util.HashSet

import java.util.Set


/**
 * A reference to a global variable
 */
class GlobalVariableReference(var name: StructuredQName)
  extends VariableReference(name)
    with ComponentInvocation {

  var bindingSlot: Int = -1

  def this(globalVariable: GlobalVariable) = {
    this(globalVariable.getVariableQName)
    name = globalVariable.getVariableQName
    fixup(globalVariable)
  }

  def copy(rebindings: RebindingMap): Expression = {
    if (binding == null) {
      //System.err.println("copy unbound variable " + this);
      throw new UnsupportedOperationException(
        "Cannot copy a variable reference whose binding is unknown")
    }
    val ref: GlobalVariableReference = new GlobalVariableReference(
      getVariableName)
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
    if (bindingSlot != -1) {
      throw new AssertionError("Duplicate binding slot assignment")
    }
    bindingSlot = slot
  }

  /**
   * Get the binding slot to be used. This is the offset within the binding vector of the containing
   * component where the actual target component is to be found.
   *
   * @return the offset in the binding vector of the containing package where the target component
   *         can be found.
   */
  def getBindingSlot(): Int = bindingSlot

  /**
   * Get the symbolic name of the component that this invocation references
   *
   * @return the symbolic name of the target component
   */
  def getSymbolicName(): SymbolicName =
    new SymbolicName(StandardNames.XSL_VARIABLE, getVariableName)

  def setTarget(target: Component): Unit = {
    binding = target.getActor.asInstanceOf[GlobalVariable]
  }

  def getTarget(): Component =
    binding.asInstanceOf[GlobalVariable].getDeclaringComponent

  def getFixedTarget(): Component = {
    val c: Component = getTarget
    val v: Visibility.Visibility = c.getVisibility.asInstanceOf[Visibility.Visibility]
    if (v == Visibility.PRIVATE || v == Visibility.FINAL) {
      c
    } else {
      null
    }
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
      if (c.getCurrentComponent == null) {
        throw new AssertionError("No current component")
      }
      val target: Component = c.getTargetComponent(bindingSlot)
      if (target.isHiddenAbstractComponent) {
        val err: XPathException = new XPathException(
          "Cannot evaluate an abstract variable (" + getVariableName.getDisplayName +
            ") with no overriding declaration",
          "XTDE3052")
        err.setLocation(getLocation)
        throw err
      }
      val p: GlobalVariable = target.getActor.asInstanceOf[GlobalVariable]
      p.evaluateVariable(c, target)
    } else {
      // code for references to final/private variables, also used in XQuery
      val b: GlobalVariable = binding.asInstanceOf[GlobalVariable]
      b.evaluateVariable(c, b.getDeclaringComponent)
    }

  override def export(out: ExpressionPresenter): Unit = {
    out.startElement("gVarRef", this)
    out.emitAttribute("name", getVariableName)
    out.emitAttribute("bSlot", "" + getBindingSlot)
    out.endElement()
  }

  def getPreconditions(): Set[Expression] = {
    val pre: Set[Expression] = new HashSet[Expression]()
    //pre.add(this.copy());
    pre
  }

}

// Copyright (c) 2018-2020 Saxonica Limited
// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is "Incompatible With Secondary Licenses", as defined by the Mozilla Public License, v. 2.0.
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
