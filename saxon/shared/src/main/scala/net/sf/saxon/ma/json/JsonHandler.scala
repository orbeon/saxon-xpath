////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
package net.sf.saxon.ma.json

import net.sf.saxon.expr.XPathContext

import net.sf.saxon.functions.SystemFunction

import net.sf.saxon.model.SpecificFunctionType

import net.sf.saxon.om.Function

import net.sf.saxon.om.Item

import net.sf.saxon.om.Sequence

import net.sf.saxon.serialize.charcode.UTF16CharacterSet

import net.sf.saxon.trans.XPathException

import net.sf.saxon.tree.util.FastStringBuffer

import net.sf.saxon.value.SequenceType

import net.sf.saxon.value.StringValue

import java.util.Map

import java.util.function.IntPredicate

import JsonHandler._

import scala.beans.{BeanProperty, BooleanBeanProperty}




object JsonHandler {

  private val REPLACEMENT: String = "\ufffd"

}

/**
  * Default handler class for accepting the result from parsing JSON strings
  */
class JsonHandler {

  var escape: Boolean = _

   var charChecker: IntPredicate = _

  @BeanProperty
  var context: XPathContext = _

  private var fallbackFunction: Function = null

  def getResult(): Sequence = null

  /**
    * Set the key to be written for the next entry in an object/map
    *
    * @param unEscaped the key for the entry (null implies no key) in unescaped form (backslashes,
    *                  if present, do not signal an escape sequence)
    * @param reEscaped the key for the entry (null implies no key) in reescaped form. In this form
    *                  special characters are represented as backslash-escaped sequences if the escape
    *                  option is yes; if escape=no, the reEscaped form is the same as the unEscaped form.
    * @return true if the key is already present in the map, false if it is not
    */
  def setKey(unEscaped: String, reEscaped: String): Boolean = false

  /**
    * Open a new array
    *
    * @throws XPathException if any error occurs
    */
  def startArray(): Unit = {}

  /**
    * Close the current array
    *
    * @throws XPathException if any error occurs
    */
  def endArray(): Unit = {}

  /**
    * Start a new object/map
    *
    * @throws XPathException if any error occurs
    */
  def startMap(): Unit = {}

  /**
    * Close the current object/map
    *
    * @throws XPathException if any error occurs
    */
  def endMap(): Unit = {}

  /**
    * Write a numeric value
    *
    * @param asString the string representation of the value
    * @param asDouble the double representation of the value
    * @throws XPathException if any error occurs
    */
  def writeNumeric(asString: String, asDouble: Double): Unit = {}

  /**
    * Write a string value
    *
    * @param val The string to be written (which may or may not contain JSON escape sequences, according to the
    * options that were set)
    * @throws XPathException if any error occurs
    */
  def writeString(`val`: String): Unit = {}

  def reEscape(`val`: String): String = {
    var escaped: CharSequence = null
    if (escape) {
      escaped = JsonReceiver.escape(`val`, forXml = true, new IntPredicate() {
        def test(value: Int): Boolean =
          (value >= 0 && value <= 0x1F) || (value >= 0x7F && value <= 0x9F) ||
            !charChecker.test(value) ||
            (value == 0x5C)
      })
    } else {
      val buffer: FastStringBuffer = new FastStringBuffer(`val`)
      handleInvalidCharacters(buffer)
      escaped = buffer
    }
    escaped.toString
  }

  /**
    * Write a boolean value
    * @param value the boolean value to be written
    * @throws XPathException if any error occurs
    */
  def writeBoolean(value: Boolean): Unit = {}

  /**
    * Write a null value
    *
    * @throws XPathException if any error occurs
    */
  def writeNull(): Unit = {}

  /**
    * Deal with invalid characters in the JSON string
    * @param buffer the JSON string
    * @throws XPathException if any error occurs
    */
   def handleInvalidCharacters(buffer: FastStringBuffer): Unit = {
//if (checkSurrogates && !liberal) {
    val charChecker: IntPredicate =
      context.getConfiguration.getValidCharacterChecker
    for (i <- 0 until buffer.length) {
      val ch: Char = buffer.charAt(i)
      if (UTF16CharacterSet.isHighSurrogate(ch)) {
        if (i + 1 >= buffer.length ||
            !UTF16CharacterSet.isLowSurrogate(buffer.charAt(i + 1))) {
          substitute(buffer, i, 1, context)
        }
      } else if (UTF16CharacterSet.isLowSurrogate(ch)) {
        if (i == 0 ||
            !UTF16CharacterSet.isHighSurrogate(buffer.charAt(i - 1))) {
          substitute(buffer, i, 1, context)
        } else {
          val pair: Int =
            UTF16CharacterSet.combinePair(buffer.charAt(i - 1), ch)
          if (!charChecker.test(pair)) {
            substitute(buffer, i - 1, 2, context)
          }
        }
      } else {
        if (!charChecker.test(ch)) {
          substitute(buffer, i, 1, context)
        }
      }
    }
  }
//}
//}

   def markAsEscaped(escaped: CharSequence, isKey: Boolean): Unit = {}
// do nothing in this class
// do nothing in this class

  /**
    * Replace an character or two characters within a string buffer, either by executing the replacement function,
    * or using the default Unicode replacement character
    *
    * @param buffer the string buffer, which is modified by this call
    * @param offset the position of the characters to be replaced
    * @param count the number of characters to be replaced
    * @param context the XPath context
    * @throws XPathException if the callback function throws an exception
    */
  private def substitute(buffer: FastStringBuffer,
                         offset: Int,
                         count: Int,
                         context: XPathContext): Unit = {
    val escaped: FastStringBuffer = new FastStringBuffer(count * 6)
    for (j <- 0 until count) {
      escaped.append("\\u")
      var hex: String =
        java.lang.Integer.toHexString(buffer.charAt(offset + j))
      while (hex.length < 4) hex = "0" + hex
// cheat to get through test json-to-xml-039
      hex = hex.toUpperCase()
      escaped.append(hex)
    }
    val replacement: String = replace(escaped.toString, context)
    if (replacement.length == count) {
      for (j <- 0 until count) {
        buffer.setCharAt(offset + j, replacement.charAt(j))
      }
    } else {
      for (j <- 0 until count) {
        buffer.removeCharAt(offset + j)
      }
      for (j <- 0 until replacement.length) {
        buffer.insert(offset + j, replacement.charAt(j))
      }
    }
  }

  /**
    * Replace an illegal XML character, either by executing the replacement function,
    * or using the default Unicode replacement character
    *
    * @param s       the string representation of the illegal character
    * @param context the XPath context
    * @return the replacement string
    * @throws XPathException if the callback function throws an exception
    */
  private def replace(s: String, context: XPathContext): String =
    if (fallbackFunction != null) {
      val args: Array[Sequence] = Array.ofDim[Sequence](1)
      args(0) = new StringValue(s)
      val result: Sequence =
        SystemFunction.dynamicCall(fallbackFunction, context, args).head()
      val first: Item = result.head()
      if (first == null) "" else first.getStringValue
    } else {
      REPLACEMENT
    }

  def setFallbackFunction(options: Map[String, Sequence],
                          context: XPathContext): Unit = {
    val `val`: Sequence = options.get("fallback")
    if (`val` != null) {
      val fn: Item = `val`.head()
      if (fn.isInstanceOf[Function]) {
        fallbackFunction = fn.asInstanceOf[Function]
        if (fallbackFunction.getArity != 1) {
          throw new XPathException("Fallback function must have arity=1",
                                   "FOJS0005")
        }
        val required: SpecificFunctionType = new SpecificFunctionType(
          Array(SequenceType.SINGLE_STRING),
          SequenceType.ANY_SEQUENCE)
        if (!required.matches(fallbackFunction,
                              context.getConfiguration.getTypeHierarchy)) {
          throw new XPathException(
            "Fallback function does not match the required type",
            "FOJS0005")
        }
      } else {
        throw new XPathException(
          "Value of option 'fallback' is not a function",
          "FOJS0005")
      }
    }
  }

}

// Copyright (c) 2018-2020 Saxonica Limited
// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is "Incompatible With Secondary Licenses", as defined by the Mozilla Public License, v. 2.0.
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// Copyright (c) 2018-2020 Saxonica Limited
