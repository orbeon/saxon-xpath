////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
package org.orbeon.saxon.functions

import org.orbeon.saxon.expr.{Callable, Expression, StringLiteral, XPathContext}
import org.orbeon.saxon.functions.Translate._
import org.orbeon.saxon.om.Sequence
import org.orbeon.saxon.regex.UnicodeString
import org.orbeon.saxon.tree.util.FastStringBuffer
import org.orbeon.saxon.value.StringValue
import org.orbeon.saxon.z.{IntToIntHashMap, IntToIntMap}

import scala.beans.BeanProperty




object Translate {

  def translate(sv0: StringValue,
                sv1: StringValue,
                sv2: StringValue): CharSequence = {
// if any string contains surrogate pairs, expand everything to 32-bit characters
    if (sv0.containsSurrogatePairs() || sv1.containsSurrogatePairs() ||
        sv2.containsSurrogatePairs()) {
      translateUsingMap(sv0, buildMap(sv1, sv2))
    }
// if the size of the strings is above some threshold, use a hash map to avoid O(n*m) performance
    if (sv0.getStringLength * sv1.getStringLength > 1000) {
// Cut-off point for building the map based on some simple measurements
      translateUsingMap(sv0, buildMap(sv1, sv2))
    }
    val cs0: CharSequence = sv0.getStringValueCS
    val cs1: CharSequence = sv1.getStringValueCS
    val cs2: CharSequence = sv2.getStringValueCS
    val st1: String = cs1.toString
    val sb: FastStringBuffer = new FastStringBuffer(cs0.length)
    val s2len: Int = cs2.length
    val s0len: Int = cs0.length
    for (i <- 0 until s0len) {
      val c: Char = cs0.charAt(i)
      val j: Int = st1.indexOf(c)
      if (j < s2len) {
        sb.cat(if (j < 0) c else cs2.charAt(j))
      }
    }
    sb
  }

  private def buildMap(arg1: StringValue, arg2: StringValue): IntToIntMap = {
    val a1: UnicodeString = arg1.getUnicodeString
    val a2: UnicodeString = arg2.getUnicodeString
    val map: IntToIntMap = new IntToIntHashMap(a1.uLength, 0.5)
    for (i <- 0 until a1.uLength if !map.find(a1.uCharAt(i))) {
      map.put(a1.uCharAt(i), if (i > a2.uLength - 1) -1 else a2.uCharAt(i))
    }
    map
  }

  def translateUsingMap(in: StringValue, map: IntToIntMap): CharSequence = {
    val us: UnicodeString = in.getUnicodeString
    val len = us.uLength
    val sb: FastStringBuffer = new FastStringBuffer(len)
    for (i <- 0 until len) {
      val c: Int = us.uCharAt(i)
      var newchar: Int = map.get(c)
      if (newchar == java.lang.Integer.MAX_VALUE) {
// character not in map, so is not to be translated
        newchar = c
      }
      if (newchar != -1) {
        sb.appendWideChar(newchar)
      }
    }
// else no action, delete the character
// else no action, delete the character
    sb
  }

}

class Translate
    extends SystemFunction
    with Callable
    with StatefulSystemFunction {

  @BeanProperty
  var staticMap: IntToIntMap = null

  /**
    * Allow the function to create an optimized call based on the values of the actual arguments
    *
    * @param arguments the supplied arguments to the function call
    * @return either a function call on this function, or an expression that delivers
    * the same result, or null indicating that no optimization has taken place
    */
  override def fixArguments(arguments: Expression*): Expression = {
    if (arguments(1).isInstanceOf[StringLiteral] && arguments(2)
          .isInstanceOf[StringLiteral]) {
      staticMap = buildMap(arguments(1).asInstanceOf[StringLiteral].getValue,
                           arguments(2).asInstanceOf[StringLiteral].getValue)
    }
    null
  }

  /**
    * Evaluate the expression
    *
    * @param context   the dynamic evaluation context
    * @param arguments the values of the arguments, supplied as Sequences
    * @return the result of the evaluation, in the form of a Sequence
    * @throws org.orbeon.saxon.trans.XPathException if a dynamic error occurs during the evaluation of the expression
    */
  def call(context: XPathContext, arguments: Array[Sequence]): StringValue = {
    val sv0: StringValue = arguments(0).head.asInstanceOf[StringValue]
    if (sv0 == null) {
      StringValue.EMPTY_STRING
    }
    if (staticMap != null) {
      new StringValue(translateUsingMap(sv0, staticMap))
    } else {
      val sv1: StringValue = arguments(1).head.asInstanceOf[StringValue]
      val sv2: StringValue = arguments(2).head.asInstanceOf[StringValue]
      new StringValue(translate(sv0, sv1, sv2))
    }
  }

  override def getCompilerName(): String = "TranslateCompiler"

  /**
    * Make a copy of this SystemFunction. This is required only for system functions such as regex
    * functions that maintain state on behalf of a particular caller.
    *
    * @return a copy of the system function able to contain its own copy of the state on behalf of
    * the caller.
    */
  override def copy(): Translate = {
    val copy: Translate = SystemFunction
      .makeFunction(getFunctionName.getLocalPart,
                    getRetainedStaticContext,
                    getArity)
      .asInstanceOf[Translate]
    copy.staticMap = staticMap
    copy
  }

}

// Copyright (c) 2018-2020 Saxonica Limited
// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is "Incompatible With Secondary Licenses", as defined by the Mozilla Public License, v. 2.0.
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
/**
  * Implement the XPath translate() function
  */
