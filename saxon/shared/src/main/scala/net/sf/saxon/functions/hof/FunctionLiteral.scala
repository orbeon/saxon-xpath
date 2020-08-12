////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
package net.sf.saxon.functions.hof

import net.sf.saxon.expr.Expression

import net.sf.saxon.expr.Literal

import net.sf.saxon.expr.StaticProperty

import net.sf.saxon.expr.instruct.UserFunction

import net.sf.saxon.expr.parser._

import net.sf.saxon.functions.AbstractFunction

import net.sf.saxon.model.FunctionItemType

import net.sf.saxon.om.Function

import net.sf.saxon.trace.ExpressionPresenter

import net.sf.saxon.trans.XPathException




class FunctionLiteral(value: Function) extends Literal(value) {

  override def getValue(): Function = super.getValue.asInstanceOf[Function]

  /*@NotNull*/

  override def simplify(): Expression = {
    if (getValue.isInstanceOf[AbstractFunction]) {
      getValue.asInstanceOf[AbstractFunction].simplify()
    }
    this
  }

  /*@NotNull*/

  override def typeCheck(visitor: ExpressionVisitor,
                         contextInfo: ContextItemStaticInfo): Expression = {
    if (getValue.isInstanceOf[AbstractFunction]) {
      getValue.asInstanceOf[AbstractFunction].typeCheck(visitor, contextInfo)
    }
    this
  }

  /*@NotNull*/

  override def getItemType(): FunctionItemType = getValue.getFunctionItemType

  override def computeCardinality(): Int = StaticProperty.EXACTLY_ONE

  override def computeSpecialProperties(): Int = StaticProperty.NO_NODES_NEWLY_CREATED

  override def isVacuousExpression(): Boolean = false

  /*@NotNull*/

  override def copy(rebindings: RebindingMap): Expression = {
    val fl2: FunctionLiteral = new FunctionLiteral(getValue)
    ExpressionTool.copyLocationInfo(this, fl2)
    fl2
  }

  /**
    * Set the retained static context
    *
    * @param rsc the static context to be retained
    */
  override def setRetainedStaticContext(rsc: RetainedStaticContext): Unit = {
    super.setRetainedStaticContext(rsc)
  }

  override def equals(obj: Any): Boolean = obj match {
    case obj: FunctionLiteral => obj.getValue == getValue
    case _ => false

  }

  override def computeHashCode(): Int = getValue.hashCode

  /**
    * Get a name identifying the kind of expression, in terms meaningful to a user.
    *
    * @return a name identifying the kind of expression, in terms meaningful to a user.
    * The name will always be in the form of a lexical XML QName, and should match the name used
    * in export() output displaying the expression.
    */
  override def getExpressionName(): String = "namedFunctionRef"

  override def export(out: ExpressionPresenter): Unit = {
    val f: Function = getValue
    if (f.isInstanceOf[UserFunction]) {
      new UserFunctionReference(f.asInstanceOf[UserFunction]).export(out)
    } else {
      f.export(out)
    }
  }

}

// Copyright (c) 2020 Saxonica Limited
// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is "Incompatible With Secondary Licenses", as defined by the Mozilla Public License, v. 2.0.
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
/**
  * A FunctionLiteral is a wrapper around a FunctionItem; it is an expression, whose value is the function
  * that it wraps. Note that a FunctionLiteral can be used only where the binding to a specific function is
  * statically known. This works for constructor functions, for system functions that have no context
  * dependency, and for references to user function (my:f#2) in XQuery, but not in XSLT where the reference
  * cannot be fully resolved until separately-compiled packages are linked. In other cases a
  * {@link UserFunctionReference} is used.
  */
// Copyright (c) 2009-2020 Saxonica Limited
