////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// Copyright (c) 2018-2020 Saxonica Limited
// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is "Incompatible With Secondary Licenses", as defined by the Mozilla Public License, v. 2.0.
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
package org.orbeon.saxon.functions.registry

import org.orbeon.saxon.utils.Configuration

import org.orbeon.saxon.expr._

import org.orbeon.saxon.functions.CallableFunction

import org.orbeon.saxon.functions.FunctionLibrary

import org.orbeon.saxon.functions.hof.AtomicConstructorFunction

import org.orbeon.saxon.lib.NamespaceConstant

import org.orbeon.saxon.model._

import org.orbeon.saxon.om.Function

import org.orbeon.saxon.om.NamespaceResolver

import org.orbeon.saxon.om.StructuredQName

import org.orbeon.saxon.om.Sequence

import org.orbeon.saxon.trans.SymbolicName

import org.orbeon.saxon.trans.XPathException

import org.orbeon.saxon.value.AtomicValue

import org.orbeon.saxon.value.EmptySequence

import org.orbeon.saxon.value.SequenceType

import java.util.List


/**
 * The ConstructorFunctionLibrary represents the collection of constructor functions for atomic types. These
 * are provided for the built-in types such as xs:integer and xs:date, and also for user-defined atomic types.
 */
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
   * @throws org.orbeon.saxon.trans.XPathException
   * in the event of certain errors, for example attempting to get a function
   * that is private
   */
  def getFunctionItem(functionName: SymbolicName.F,
                      staticContext: StaticContext): Function = {
    if (functionName.getArity != 1) {
      return null
    }
    val uri: String = functionName.getComponentName.getURI
    val localName: String = functionName.getComponentName.getLocalPart
    val `type`: SchemaType =
      config.getSchemaType(new StructuredQName("", uri, localName))
    if (`type` == null || `type`.isComplexType) {
      return null
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
      val callable: Callable = (context: XPathContext, arguments: Array[Sequence]) => {
        var value: AtomicValue = arguments(0).head.asInstanceOf[AtomicValue]
        if (value == null) {
          EmptySequence.getInstance
        } else
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
      return false
    }
    val uri: String = functionName.getComponentName.getURI
    val localName: String = functionName.getComponentName.getLocalPart
    val _type = config.getSchemaType(new StructuredQName("", uri, localName))
    if (_type == null || _type.isComplexType)
      false
    else if (_type.isAtomicType && _type.asInstanceOf[AtomicType].isAbstract)
      false
    else
      _type ne AnySimpleType
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
        return null
      }
      val `type`: SimpleType = Type.getBuiltInSimpleType(uri, localName)
      if (`type` != null) {
        if (`type`.isAtomicType) {
          if (`type`.asInstanceOf[AtomicType].isAbstract) {
            reasons.add(
              "Abstract type used in constructor function: {" + uri +
                '}' +
                localName)
            return null
          } else {
            val cast: CastExpression = new CastExpression(
              arguments(0),
              `type`.asInstanceOf[AtomicType],
              true)
            if (arguments(0).isInstanceOf[StringLiteral]) {
              cast.setOperandIsStringLiteral(true)
            }
            return cast
          }
        } else if (`type`.isUnionType) {
          val resolver: NamespaceResolver = env.getNamespaceResolver
          val ucf: UnionConstructorFunction = new UnionConstructorFunction(
            `type`.asInstanceOf[UnionType],
            resolver,
            true)
          return new StaticFunctionCall(ucf, arguments)
        } else {
          val resolver: NamespaceResolver = env.getNamespaceResolver
          try {
            val lcf: ListConstructorFunction = new ListConstructorFunction(
              `type`.asInstanceOf[ListType],
              resolver,
              true)
            return new StaticFunctionCall(lcf, arguments)
          } catch {
            case e: MissingComponentException =>
              reasons.add("Missing schema component: " + e.getMessage)
              return null
          }
        }
      } else {
        reasons.add("Unknown constructor function: {" + uri + '}' + localName)
        return null
      }
    }
    if (arguments.length == 1) {
      val st: SchemaType =
        config.getSchemaType(new StructuredQName("", uri, localName))
      st match {
        case simpleType: SimpleType =>
          st match {
            case atomicType: AtomicType =>
              new CastExpression(arguments(0), atomicType, true)
            case listType: ListType if env.getXPathVersion >= 30 =>
              val resolver: NamespaceResolver = env.getNamespaceResolver
              try {
                val lcf: ListConstructorFunction = new ListConstructorFunction(
                  listType,
                  resolver,
                  true)
                return new StaticFunctionCall(lcf, arguments)
              } catch {
                case e: MissingComponentException =>
                  reasons.add("Missing schema component: " + e.getMessage)
                  return null
              }
            case _ => if (simpleType.isUnionType && env.getXPathVersion >= 30) {
              val resolver: NamespaceResolver = env.getNamespaceResolver
              val ucf: UnionConstructorFunction = new UnionConstructorFunction(
                st.asInstanceOf[UnionType],
                resolver,
                true)
              return new StaticFunctionCall(ucf, arguments)
            }
          }
        case _ =>
      }
    }
    null
  }

  def copy(): FunctionLibrary = this
}