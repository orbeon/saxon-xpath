package org.orbeon.saxon.expr

import org.orbeon.saxon.expr.parser.XPathParser

import org.orbeon.saxon.model.AtomicType

import org.orbeon.saxon.model.BuiltInAtomicType

import org.orbeon.saxon.model.Converter

import org.orbeon.saxon.om.NamespaceResolver

import org.orbeon.saxon.trace.ExpressionPresenter

import org.orbeon.saxon.trans.XPathException

import org.orbeon.saxon.value.SequenceType

import scala.beans.{BeanProperty, BooleanBeanProperty}


abstract class CastingExpression(source: Expression,
                                 @BeanProperty var targetType: AtomicType,
                                 private var allowEmpty: Boolean)
  extends UnaryExpression(source) {

  @BeanProperty
  var targetPrimitiveType: AtomicType = targetType.getPrimitiveItemType

   var converter: Converter = _

  @BooleanBeanProperty
  var operandIsStringLiteral: Boolean = false

   def getOperandRole(): OperandRole = OperandRole.SINGLE_ATOMIC

  def setAllowEmpty(allow: Boolean): Unit = {
    allowEmpty = allow
  }

  def allowsEmpty(): Boolean = allowEmpty

  def getConverter: Converter = converter

  def getNamespaceResolver: NamespaceResolver = getRetainedStaticContext

  /**
   * Get the expression's dependencies. If the target type is namespace-sensitive, then the expression
   * has a dependency on the namespace bindings in the static context
   *
   * @return the expression's dependencies.
   */
  override def getIntrinsicDependencies: Int =
    if (getTargetType.isNamespaceSensitive)
      StaticProperty.DEPENDS_ON_STATIC_CONTEXT
    else 0

  /*@NotNull*/

  override def simplify(): Expression = {
    if (targetType.isInstanceOf[BuiltInAtomicType]) {
      val s: String = XPathParser.whyDisallowedType(
        getPackageData,
        targetType.asInstanceOf[BuiltInAtomicType])
      if (s != null) {
        val err =
          new XPathException(s, "XPST0080", this.getLocation)
        err.setIsStaticError(true)
        throw err
      }
    }
    this.setBaseExpression(getBaseExpression.simplify())
    this
  }

  override def computeSpecialProperties(): Int = {
    val p: Int = super.computeSpecialProperties()
    p | StaticProperty.NO_NODES_NEWLY_CREATED
  }

   def export(out: ExpressionPresenter, elemName: String): Unit = {
    out.startElement(elemName, this)
    val card: Int =
      if (allowsEmpty()) StaticProperty.ALLOWS_ZERO_OR_ONE
      else StaticProperty.EXACTLY_ONE
    val st: SequenceType = SequenceType.makeSequenceType(getTargetType, card)
    out.emitAttribute("flags", "a" + (if (allowsEmpty()) "e" else ""))
    out.emitAttribute("as", st.toAlphaCode)
    getBaseExpression.export(out)
    out.endElement()
  }

}

// Copyright (c) 2018-2020 Saxonica Limited
// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is "Incompatible With Secondary Licenses", as defined by the Mozilla Public License, v. 2.0.
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
/**
 * Casting Expression: abstract superclass for "cast as X" and "castable as X", which share a good deal of logic
 */
