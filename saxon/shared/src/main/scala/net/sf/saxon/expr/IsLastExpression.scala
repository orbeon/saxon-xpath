////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
package net.sf.saxon.expr

import net.sf.saxon.expr.parser.ContextItemStaticInfo

import net.sf.saxon.expr.parser.ExpressionTool

import net.sf.saxon.expr.parser.ExpressionVisitor

import net.sf.saxon.expr.parser.RebindingMap

import net.sf.saxon.model.BuiltInAtomicType

import net.sf.saxon.model.ItemType

import net.sf.saxon.trace.ExpressionPresenter

import net.sf.saxon.trans.XPathException

import net.sf.saxon.value.BooleanValue

import scala.beans.{BeanProperty, BooleanBeanProperty}




class IsLastExpression(@BeanProperty var condition: Boolean)
    extends Expression {

  /*@NotNull*/

  override def typeCheck(visitor: ExpressionVisitor,
                contextInfo: ContextItemStaticInfo): Expression = this

  /*@NotNull*/

  override def optimize(visitor: ExpressionVisitor,
               contextItemType: ContextItemStaticInfo): Expression = this

  override def computeSpecialProperties(): Int = {
    val p: Int = super.computeSpecialProperties()
    p | StaticProperty.NO_NODES_NEWLY_CREATED
  }

  override def evaluateItem(c: XPathContext): BooleanValue =
    BooleanValue.get(condition == c.isAtLast)

  /*@NotNull*/

  def getItemType: ItemType = BuiltInAtomicType.BOOLEAN

  def computeCardinality(): Int = StaticProperty.EXACTLY_ONE

  override def getIntrinsicDependencies(): Int =
    StaticProperty.DEPENDS_ON_POSITION | StaticProperty.DEPENDS_ON_LAST

  /*@NotNull*/

  def copy(rebindings: RebindingMap): Expression = {
    val exp: IsLastExpression = new IsLastExpression(condition)
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
  override def getImplementationMethod(): Int = Expression.EVALUATE_METHOD

  /**
    * Is this expression the same as another expression?
    *
    * @param other the expression to be compared with this one
    * @return true if the two expressions are statically equivalent
    */
  override def equals(other: Any): Boolean = other match {
    case other: IsLastExpression => other.condition == condition
    case _ => false

  }

  override def computeHashCode(): Int = if (condition) 0x236b91a0 else 0x896b92a0

  /**
    * Get a name identifying the kind of expression, in terms meaningful to a user.
    *
    * @return a name identifying the kind of expression, in terms meaningful to a user.
    * The name will always be in the form of a lexical XML QName, and should match the name used
    * in export() output displaying the expression.
    */
  override def getExpressionName(): String = "isLast"

  def export(destination: ExpressionPresenter): Unit = {
    destination.startElement("isLast", this)
    destination.emitAttribute("test", if (condition) "1" else "0")
    destination.endElement()
  }

  /**
    * <p>The toString() method for an expression attempts to give a representation of the expression
    * in an XPath-like form.</p>
    * <p>For subclasses of Expression that represent XPath expressions, the result should always be a string that
    * parses as an XPath 3.0 expression.</p>
    *
    * @return a representation of the expression as a string
    */
  override def toString: String =
    if (condition) {
      "position() eq last()"
    } else {
      "position() ne last()"
    }

  /**
    * Get the (partial) name of a class that supports streaming of this kind of expression
    *
    * @return the partial name of a class that can be instantiated to provide streaming support in Saxon-EE,
    * or null if there is no such class
    */
  override def getStreamerName(): String = "IsLastExpr"

}

// Copyright (c) 2018-2020 Saxonica Limited
// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is "Incompatible With Secondary Licenses", as defined by the Mozilla Public License, v. 2.0.
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
/**
  * A position() eq last() expression, generated by the optimizer.
  */
