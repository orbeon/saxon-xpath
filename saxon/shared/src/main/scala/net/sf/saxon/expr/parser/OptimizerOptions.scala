////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
package net.sf.saxon.expr.parser

import OptimizerOptions._

import scala.beans.{BeanProperty, BooleanBeanProperty}


object OptimizerOptions {

  val LOOP_LIFTING: Int = 1

  val EXTRACT_GLOBALS: Int = 2

  val INLINE_VARIABLES: Int = 4

  val INLINE_FUNCTIONS: Int = 8

  val INDEX_VARIABLES: Int = 16

  val CREATE_KEYS: Int = 32

  val BYTE_CODE: Int = 64

  val COMMON_SUBEXPRESSIONS: Int = 128

  val MISCELLANEOUS: Int = 256

  val SWITCH: Int = 512

  val JIT: Int = 1024

  val RULE_SET: Int = 2048

  val REGEX_CACHE: Int = 4096

  val VOID_EXPRESSIONS: Int = 8192

  val TAIL_CALLS: Int = 16384

  val CONSTANT_FOLDING: Int = 32768

  val FULL_HE_OPTIMIZATION: OptimizerOptions = new OptimizerOptions("lvmt")

  val FULL_EE_OPTIMIZATION: OptimizerOptions = new OptimizerOptions(-1)

}

/**
 * Defines switches that can be used to control which optimizations take place.
 * The object is immutable.
 */
class OptimizerOptions(@BeanProperty var options: Int) {

  def this(flags: String) = {
    this(0)
    var opt: Int = 0
    if (flags.startsWith("-")) {
      opt = -1
      for (i <- 0 until flags.length) {
        val c: Char = flags.charAt(i)
        opt &= ~decodeFlag(c)
      }
    } else {
      for (i <- 0 until flags.length) {
        val c: Char = flags.charAt(i)
        opt |= decodeFlag(c)
      }
    }
    this.options = opt
  }

  private def decodeFlag(flag: Char): Int = flag match {
    case 'c' => BYTE_CODE
    case 'd' => VOID_EXPRESSIONS
    case 'e' => REGEX_CACHE
    case 'f' => INLINE_FUNCTIONS
    case 'g' => EXTRACT_GLOBALS
    case 'j' => JIT
    case 'k' => CREATE_KEYS
    case 'l' => LOOP_LIFTING
    case 'm' => MISCELLANEOUS
    case 'n' => CONSTANT_FOLDING
    case 'r' => RULE_SET
    case 's' => COMMON_SUBEXPRESSIONS
    case 't' => TAIL_CALLS
    case 'v' => INLINE_VARIABLES
    case 'w' => SWITCH
    case 'x' => INDEX_VARIABLES
    case _ => 0

  }

  def intersect(other: OptimizerOptions): OptimizerOptions =
    new OptimizerOptions(options & other.options)

  def union(other: OptimizerOptions): OptimizerOptions =
    new OptimizerOptions(options | other.options)

  def except(other: OptimizerOptions): OptimizerOptions =
    new OptimizerOptions(options & ~other.options)

  override def toString: String = {
    var result: String = ""
    if (isSet(BYTE_CODE)) {
      result += "c"
    }
    if (isSet(VOID_EXPRESSIONS)) {
      result += "d"
    }
    if (isSet(REGEX_CACHE)) {
      result += "e"
    }
    if (isSet(INLINE_FUNCTIONS)) {
      result += "f"
    }
    if (isSet(EXTRACT_GLOBALS)) {
      result += "g"
    }
    if (isSet(JIT)) {
      result += "j"
    }
    if (isSet(CREATE_KEYS)) {
      result += "k"
    }
    if (isSet(LOOP_LIFTING)) {
      result += "l"
    }
    if (isSet(MISCELLANEOUS)) {
      result += "m"
    }
    if (isSet(CONSTANT_FOLDING)) {
      result += "n"
    }
    if (isSet(RULE_SET)) {
      result += "r"
    }
    if (isSet(COMMON_SUBEXPRESSIONS)) {
      result += "s"
    }
    if (isSet(TAIL_CALLS)) {
      result += "t"
    }
    if (isSet(INLINE_VARIABLES)) {
      result += "v"
    }
    if (isSet(SWITCH)) {
      result += "w"
    }
    if (isSet(INDEX_VARIABLES)) {
      result += "x"
    }
    result
  }

  def isSet(option: Int): Boolean = (options & option) != 0

}

// Copyright (c) 2018-2020 Saxonica Limited
// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is "Incompatible With Secondary Licenses", as defined by the Mozilla Public License, v. 2.0.
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
