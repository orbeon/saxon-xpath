////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// Copyright (c) 2018-2020 Saxonica Limited
// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is "Incompatible With Secondary Licenses", as defined by the Mozilla Public License, v. 2.0.
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
package org.orbeon.saxon.ma.json

import java.{util => ju}

import org.orbeon.saxon.expr.XPathContext
import org.orbeon.saxon.ma.arrays.{ArrayItem, SimpleArrayItem}
import org.orbeon.saxon.ma.map.{DictionaryMap, MapItem}
import org.orbeon.saxon.om.{GroundedValue, Sequence}
import org.orbeon.saxon.trans.XPathException
import org.orbeon.saxon.value.{BooleanValue, DoubleValue, EmptySequence, StringValue}


/**
 * Event handler for the JSON parser which constructs a structure of maps and arrays
 * representing the content of the JSON text.
 */
class JsonHandlerMap(context: XPathContext, flags: Int) extends JsonHandler {

  var stack: List[Sequence] = Nil
  var keys: List[String] = Nil

  escape = (flags & JsonParser.ESCAPE) != 0

  charChecker = context.getConfiguration.getValidCharacterChecker

  override def getResult: Sequence = stack.head

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
  override def setKey(unEscaped: String, reEscaped: String): Boolean = {
    this.keys ::= reEscaped
    val map: MapItem = stack.head.asInstanceOf[MapItem]
    map.get(new StringValue(reEscaped)) != null
  }

  /**
   * Open a new array
   *
   */
  override def startArray(): Unit = {
    val map: ArrayItem = new SimpleArrayItem(new ju.ArrayList[GroundedValue])
    stack ::= map
  }

  /**
   * Close the current array
   */
  override def endArray(): Unit = {
    val map = { val r = stack.head; stack = stack.tail; r }.asInstanceOf[ArrayItem]
    if (stack.isEmpty) {
      // the end
      stack ::= map
    } else {
      writeItem(map)
    }
  }

  /**
   * Start a new object/map
   */
  override def startMap(): Unit = {
    val map: DictionaryMap = new DictionaryMap()
    stack ::= map
  }

  /**
   * Close the current object/map
   */
  override def endMap(): Unit = {
    val map = { val r = stack.head; stack = stack.tail; r }.asInstanceOf[DictionaryMap]
    if (stack.isEmpty) {
      // the end
      stack ::= map
    } else {
      writeItem(map)
    }
  }

  /**
   * Write an item into the current map, with the preselected key
   *
   * @param val the value/map to be written
   */
  private def writeItem(`val`: GroundedValue): Unit = {
    if (stack.isEmpty) {
      stack ::= `val`
    } else if (stack.head.isInstanceOf[ArrayItem]) {
      val array: SimpleArrayItem = stack.head.asInstanceOf[SimpleArrayItem]
      array.getMembersList.add(`val`.materialize)
    } else {
      val map: DictionaryMap = stack.head.asInstanceOf[DictionaryMap]
      map.initialPut({ val r = keys.head; keys = keys.tail; r }, `val`)
    }
  }

  /**
   * Write a numeric value
   *
   * @param asString the string representation of the value
   * @param asDouble the double representation of the value
   */
  override def writeNumeric(asString: String, asDouble: Double): Unit = {
    writeItem(new DoubleValue(asDouble))
  }

  /**
   * Write a string value
   *
   * @param val The string to be written (which may or may not contain JSON escape sequences, according to the
   *            options that were set)
   * @throws XPathException if a dynamic error occurs
   */
  override def writeString(`val`: String): Unit = {
    writeItem(new StringValue(reEscape(`val`)))
  }

  /**
   * Write a boolean value
   *
   * @param value the boolean value to be written
   */
  override def writeBoolean(value: Boolean): Unit = {
    writeItem(BooleanValue.get(value))
  }

  /**
   * Write a null value
   */
  override def writeNull(): Unit = {
    writeItem(EmptySequence.getInstance)
  }
}
