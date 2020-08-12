////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
package net.sf.saxon.functions.registry

import net.sf.saxon.utils.Configuration

import net.sf.saxon.expr._

import net.sf.saxon.functions.CallableFunction

import net.sf.saxon.functions.FunctionLibrary

import net.sf.saxon.functions.hof.AtomicConstructorFunction

import net.sf.saxon.lib.NamespaceConstant

import net.sf.saxon.model._

import net.sf.saxon.om.Function

import net.sf.saxon.om.NamespaceResolver

import net.sf.saxon.om.StructuredQName

import net.sf.saxon.om.Sequence

import net.sf.saxon.trans.SymbolicName

import net.sf.saxon.trans.XPathException

import net.sf.saxon.value.AtomicValue

import net.sf.saxon.value.EmptySequence

import net.sf.saxon.value.SequenceType

import java.util.List




class ConstructorFunctionLibrary(private var config: Configuration)
    extends FunctionLibrary {

  /**
    * Test whether a function with a given name and arity is available; if so, return a function
    * item that can be dynamically called.
    * <p>This supports the function-lookup() function in XPath 3.0.</p>
    *
    * @param functionName  the qualified name of the function being called
    * @param staticContext the static context to be used by the function, in the event that
    *                      it is a system function with dependencies on the static context
    * @return if a function of this name and arity is available for calling, then a corresponding
    *         function item; or null if the function does not exist
    * @throws net.sf.saxon.trans.XPathException
    *          in the event of certain errors, for example attempting to get a function
    *          that is private
    */
  def getFunctionItem(functionName: SymbolicName.F,
                      staticContext: StaticContext): Function = {
    if (functionName.getArity != 1) {
      null
    }
    val uri: String = functionName.getComponentName.getURI
    val localName: String = functionName.getComponentName.getLocalPart
    val `type`: SchemaType =
      config.getSchemaType(new StructuredQName("", uri, localName))
    if (`type` == null || `type`.isComplexType) {
      null
    }
    val resolver: NamespaceResolver =
      if (`type`.asInstanceOf[SimpleType].isNamespaceSensitive)
        staticContext.getNamespaceResolver
      else null
    if (`type`.isInstanceOf[AtomicType]) {
      new AtomicConstructorFunction(`type`.asInstanceOf[AtomicType], resolver)
    } else if (`type`.isInstanceOf[ListType]) {
      new ListConstructorFunction(`type`.asInstanceOf[ListType],
                                  resolver,
                                  true)
    } else {
      val callable: Callable = (context:XPathContext, arguments:Array[Sequence]) => {
        var value: AtomicValue = arguments(0).head().asInstanceOf[AtomicValue]
        if (value == null) {
          EmptySequence.getInstance
        }
        UnionConstructorFunction.cast(
          value,
          `type`.asInstanceOf[UnionType],
          resolver,
          context.getConfiguration.getConversionRules)
      }
      val returnType: SequenceType =
        `type`.asInstanceOf[UnionType].getResultTypeOfCast
      new CallableFunction(
        1,
        callable,
        new SpecificFunctionType(Array(SequenceType.OPTIONAL_ATOMIC),
                                 returnType))
    }
  }

  def isAvailable(functionName: SymbolicName.F): Boolean = {
    if (functionName.getArity != 1) {
      false
    }
    val uri: String = functionName.getComponentName.getURI
    val localName: String = functionName.getComponentName.getLocalPart
    val `type`: SchemaType =
      config.getSchemaType(new StructuredQName("", uri, localName))
    if (`type` == null || `type`.isComplexType) {
      false
    }
    if (`type`.isAtomicType && `type`.asInstanceOf[AtomicType].isAbstract) {
      false
    }
    `type` != AnySimpleType.getInstance
  }

  def bind(functionName: SymbolicName.F,
           arguments: Array[Expression],
           env: StaticContext,
           reasons: List[String]): Expression = {
    val uri: String = functionName.getComponentName.getURI
    val localName: String = functionName.getComponentName.getLocalPart
    val builtInNamespace: Boolean = uri == NamespaceConstant.SCHEMA
    if (builtInNamespace) {
// it's a constructor function: treat it as shorthand for a cast expression
      if (functionName.getArity != 1) {
        reasons.add("A constructor function must have exactly one argument")
        null
      }
      val `type`: SimpleType = Type.getBuiltInSimpleType(uri, localName)
      if (`type` != null) {
        if (`type`.isAtomicType) {
          if (`type`.asInstanceOf[AtomicType].isAbstract) {
            reasons.add(
              "Abstract type used in constructor function: {" + uri +
                '}' +
                localName)
            null
          } else {
            val cast: CastExpression = new CastExpression(
              arguments(0),
              `type`.asInstanceOf[AtomicType],
              true)
            if (arguments(0).isInstanceOf[StringLiteral]) {
              cast.setOperandIsStringLiteral(true)
            }
            cast
          }
        } else if (`type`.isUnionType) {
          val resolver: NamespaceResolver = env.getNamespaceResolver
          val ucf: UnionConstructorFunction = new UnionConstructorFunction(
            `type`.asInstanceOf[UnionType],
            resolver,
            true)
          new StaticFunctionCall(ucf, arguments)
        } else {
          val resolver: NamespaceResolver = env.getNamespaceResolver
          try {
            val lcf: ListConstructorFunction = new ListConstructorFunction(
              `type`.asInstanceOf[ListType],
              resolver,
              true)
            new StaticFunctionCall(lcf, arguments)
          } catch {
            case e: MissingComponentException => {
              reasons.add("Missing schema component: " + e.getMessage)
              null
            }

          }
        }
      } else {
        reasons.add("Unknown constructor function: {" + uri + '}' + localName)
        null
      }
    }
    if (arguments.length == 1) {
      val st: SchemaType =
        config.getSchemaType(new StructuredQName("", uri, localName))
      if (st.isInstanceOf[SimpleType]) {
        if (st.isInstanceOf[AtomicType]) {
          new CastExpression(arguments(0), st.asInstanceOf[AtomicType], true)
        } else if (st.isInstanceOf[ListType] && env.getXPathVersion >= 30) {
          val resolver: NamespaceResolver = env.getNamespaceResolver
          try {
            val lcf: ListConstructorFunction = new ListConstructorFunction(
              st.asInstanceOf[ListType],
              resolver,
              true)
            new StaticFunctionCall(lcf, arguments)
          } catch {
            case e: MissingComponentException => {
              reasons.add("Missing schema component: " + e.getMessage)
              null
            }

          }
        } else if (st.asInstanceOf[SimpleType]
                     .isUnionType && env.getXPathVersion >= 30) {
          val resolver: NamespaceResolver = env.getNamespaceResolver
          val ucf: UnionConstructorFunction = new UnionConstructorFunction(
            st.asInstanceOf[UnionType],
            resolver,
            true)
          new StaticFunctionCall(ucf, arguments)
        }
      }
    }
    null
  }
// Now see if it's a constructor function for a user-defined type
// Now see if it's a constructor function for a user-defined type

  def copy(): FunctionLibrary = this

}

// Copyright (c) 2018-2020 Saxonica Limited
// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is "Incompatible With Secondary Licenses", as defined by the Mozilla Public License, v. 2.0.
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
/**
  * The ConstructorFunctionLibrary represents the collection of constructor functions for atomic types. These
  * are provided for the built-in types such as xs:integer and xs:date, and also for user-defined atomic types.
  */
