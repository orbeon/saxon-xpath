////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
package org.orbeon.saxon.xpath

import org.orbeon.saxon.utils.Configuration

import org.orbeon.saxon.expr._

import org.orbeon.saxon.expr.parser.RebindingMap

import org.orbeon.saxon.model.AnyItemType

import org.orbeon.saxon.model.ItemType

import org.orbeon.saxon.om.Sequence

import org.orbeon.saxon.om.SequenceIterator

import org.orbeon.saxon.om.StructuredQName

import org.orbeon.saxon.trace.ExpressionPresenter

import org.orbeon.saxon.trans.XPathException

import org.orbeon.saxon.value.EmptySequence

import javax.xml.xpath.XPathVariableResolver




class JAXPVariableReference(private var name: StructuredQName,
                            private var resolver: XPathVariableResolver)
  extends Expression
    with Callable {

  override def getExpressionName: String = "$" + name.getDisplayName

  /*@NotNull*/

  def copy(rebindings: RebindingMap): Expression =
    new JAXPVariableReference(name, resolver)

  /*@NotNull*/

  def getItemType: ItemType = AnyItemType

  def computeCardinality(): Int = StaticProperty.ALLOWS_ZERO_OR_MORE

  override def computeSpecialProperties(): Int = StaticProperty.NO_NODES_NEWLY_CREATED

  override def getImplementationMethod: Int = Expression.ITERATE_METHOD

  override def equals(other: Any): Boolean =
    other.isInstanceOf[JAXPVariableReference] &&
      other.asInstanceOf[JAXPVariableReference].name == name &&
      other.asInstanceOf[JAXPVariableReference].resolver == resolver

  override def computeHashCode(): Int = name.hashCode

  override def call(context: XPathContext,
                    arguments: Array[Sequence]): Sequence = {
    val config: Configuration = context.getConfiguration
    val value: AnyRef = resolver.resolveVariable(name.toJaxpQName)
    if (value == null) {
      EmptySequence.getInstance
    }
    val converter: JPConverter =
      JPConverter.allocate(value.getClass, null, config)
    converter.convert(value, context)
  }

  /*@NotNull*/

  override def iterate(context: XPathContext): SequenceIterator =
    call(context, null).iterate()

  override def toString: String = getExpressionName

  def export(destination: ExpressionPresenter): Unit = {
    destination.startElement("jaxpVar", this)
    destination.emitAttribute("name", name)
    destination.endElement()
  }

}

// Copyright (c) 2018-2020 Saxonica Limited
// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is "Incompatible With Secondary Licenses", as defined by the Mozilla Public License, v. 2.0.
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
/**
 * This class represents a variable in an XPath expression compiled using the JAXP XPath API.
 * Although the class name suggests otherwise, the expression is not in fact a VariableReference;
 * it's a custom expression which, on evaluation, calls the JAXP XPathVariableResolver to get the
 * value of the variable.
 */
// See bug 2554 which motivated a redesign for Saxon 9.7.0.2
