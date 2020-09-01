////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
package net.sf.saxon.expr

import net.sf.saxon.lib.ConversionRules

import net.sf.saxon.model._

import net.sf.saxon.om.NamespaceResolver

import net.sf.saxon.om.Sequence

import net.sf.saxon.om.SequenceIterator

import net.sf.saxon.om.StructuredQName

import net.sf.saxon.trans.XPathException

import net.sf.saxon.value._




class ListCastableFunction(targetType: ListType,
                           resolver: NamespaceResolver,
                           allowEmpty: Boolean)
    extends ListConstructorFunction(targetType, resolver, allowEmpty) {

  /**
    * Get the item type of the function item
    *
    * @return the function item's type
    */
  override def getFunctionItemType: FunctionItemType =
    new SpecificFunctionType(Array(SequenceType.ANY_SEQUENCE),
                             SequenceType.SINGLE_BOOLEAN)

  /**
    * Get the name of the function, or null if it is anonymous
    *
    * @return the function name, or null for an anonymous inline function
    */
  override def getFunctionName: StructuredQName = null

  override def call(context: XPathContext, args: Array[Sequence]): BooleanValue = {
    val iter: SequenceIterator = args(0).iterate()
    val `val`: AtomicValue = iter.next().asInstanceOf[AtomicValue]
    if (`val` == null) {
      BooleanValue.get(allowEmpty)
    }
    if (iter.next() != null) {
      BooleanValue.FALSE
    }
    if (!(`val`.isInstanceOf[StringValue]) || `val`
          .isInstanceOf[AnyURIValue]) {
      BooleanValue.FALSE
    }
    val rules: ConversionRules = context.getConfiguration.getConversionRules
    val cs: CharSequence = `val`.getStringValueCS
    val failure: ValidationFailure =
      targetType.validateContent(cs, nsResolver, rules)
    BooleanValue.get(failure == null)
  }

}

// Copyright (c) 2018-2020 Saxonica Limited
// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is "Incompatible With Secondary Licenses", as defined by the Mozilla Public License, v. 2.0.
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
/**
  * A function item representing a castability test for a list type
  */
