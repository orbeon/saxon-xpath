////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
package net.sf.saxon.expr

import net.sf.saxon.functions.AbstractFunction

import net.sf.saxon.lib.ConversionRules

import net.sf.saxon.model._

import net.sf.saxon.om._

import net.sf.saxon.trans.XPathException

import net.sf.saxon.value.AnyURIValue

import net.sf.saxon.value.AtomicValue

import net.sf.saxon.value.SequenceType

import net.sf.saxon.value.StringValue




class ListConstructorFunction( var targetType: ListType,
                              resolver: NamespaceResolver,
                               var allowEmpty: Boolean)
    extends AbstractFunction {

   var nsResolver: NamespaceResolver = resolver

   var memberType: SimpleType = targetType.getItemType

  def getTargetType(): ListType = targetType

  def getMemberType(): SimpleType = memberType

  def isAllowEmpty(): Boolean = allowEmpty

  /**
    * Get the item type of the function item
    *
    * @return the function item's type
    */
  def getFunctionItemType(): FunctionItemType = {
    var resultType: AtomicType = BuiltInAtomicType.ANY_ATOMIC
    if (memberType.isAtomicType) {
      resultType = memberType.asInstanceOf[AtomicType]
    }
    val argType: SequenceType =
      if (allowEmpty) SequenceType.OPTIONAL_ATOMIC
      else SequenceType.SINGLE_ATOMIC
    new SpecificFunctionType(
      Array(argType),
      SequenceType.makeSequenceType(resultType,
                                    StaticProperty.ALLOWS_ZERO_OR_MORE))
  }

  /**
    * Get the name of the function, or null if it is anonymous
    *
    * @return the function name, or null for an anonymous inline function
    */
  def getFunctionName(): StructuredQName = targetType.getStructuredQName

  /**
    * Get a description of this function for use in error messages. For named functions, the description
    * is the function name (as a lexical QName). For others, it might be, for example, "inline function",
    * or "partially-applied ends-with function".
    *
    * @return a description of the function for use in error messages
    */
  def getDescription(): String = getFunctionName.getDisplayName

  /**
    * Get the arity of the function
    *
    * @return the number of arguments in the function signature
    */
  def getArity(): Int = 1

  /**
    * Invoke the function
    *
    * @param context the XPath dynamic evaluation context
    * @param args    the actual arguments to be supplied
    * @return the result of invoking the function
    * @throws net.sf.saxon.trans.XPathException
    *          if a dynamic error occurs within the function
    */
  def call(context: XPathContext, args: Array[Sequence]): AtomicSequence = {
    val `val`: AtomicValue = args(0).head().asInstanceOf[AtomicValue]
    if (`val` == null) {
      if (allowEmpty) {
        EmptyAtomicSequence.getInstance
      } else {
        val e: XPathException = new XPathException(
          "Cast expression does not allow an empty sequence to be supplied",
          "XPTY0004")
        e.setIsTypeError(true)
        throw e
      }
    }
    if (!(`val`.isInstanceOf[StringValue]) || `val`
          .isInstanceOf[AnyURIValue]) {
      val e: XPathException = new XPathException(
        "Only xs:string and xs:untypedAtomic can be cast to a list type",
        "XPTY0004")
      e.setIsTypeError(true)
      throw e
    }
    val rules: ConversionRules = context.getConfiguration.getConversionRules
    val cs: CharSequence = `val`.getStringValueCS
    val failure: ValidationFailure =
      targetType.validateContent(cs, nsResolver, rules)
    if (failure != null) {
      throw failure.makeException()
    }
    targetType.getTypedValue(cs, nsResolver, rules)
  }

}

// Copyright (c) 2018-2020 Saxonica Limited
// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is "Incompatible With Secondary Licenses", as defined by the Mozilla Public License, v. 2.0.
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
/**
  * A function item representing a constructor function for a list type
  */
