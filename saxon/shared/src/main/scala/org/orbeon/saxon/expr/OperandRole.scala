////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
package org.orbeon.saxon.expr

import org.orbeon.saxon.model.FunctionItemType
import org.orbeon.saxon.model.ItemType
import org.orbeon.saxon.model.PlainType
import org.orbeon.saxon.value.SequenceType
import OperandRole._
import org.orbeon.saxon.expr.OperandUsage.OperandUsage

import scala.beans.{BeanProperty, BooleanBeanProperty}




object OperandRole {

  val SETS_NEW_FOCUS: Int = 1

  val USES_NEW_FOCUS: Int = 2

  val HIGHER_ORDER: Int = 4

  val IN_CHOICE_GROUP: Int = 8

  val CONSTRAINED_CLASS: Int = 16

// set only where HIGHER_ORDER would otherwise imply repeated evaluation
  val SINGLETON: Int = 32

  val HAS_SPECIAL_FOCUS_RULES: Int = 64

  val SAME_FOCUS_ACTION: OperandRole =
    new OperandRole(0, OperandUsage.TRANSMISSION, SequenceType.ANY_SEQUENCE)

  val FOCUS_CONTROLLING_SELECT: OperandRole = new OperandRole(
    OperandRole.SETS_NEW_FOCUS,
    OperandUsage.INSPECTION,
    SequenceType.ANY_SEQUENCE)

  val FOCUS_CONTROLLED_ACTION: OperandRole = new OperandRole(
    OperandRole.USES_NEW_FOCUS | OperandRole.HIGHER_ORDER,
    OperandUsage.TRANSMISSION,
    SequenceType.ANY_SEQUENCE)

  val INSPECT: OperandRole =
    new OperandRole(0, OperandUsage.INSPECTION, SequenceType.ANY_SEQUENCE)

  val ABSORB: OperandRole =
    new OperandRole(0, OperandUsage.ABSORPTION, SequenceType.ANY_SEQUENCE)

  val REPEAT_INSPECT: OperandRole = new OperandRole(OperandRole.HIGHER_ORDER,
                                                    OperandUsage.INSPECTION,
                                                    SequenceType.ANY_SEQUENCE)

  val NAVIGATE: OperandRole =
    new OperandRole(0, OperandUsage.NAVIGATION, SequenceType.ANY_SEQUENCE)

  val REPEAT_NAVIGATE: OperandRole = new OperandRole(OperandRole.HIGHER_ORDER,
                                                     OperandUsage.NAVIGATION,
                                                     SequenceType.ANY_SEQUENCE)

  val REPEAT_NAVIGATE_CONSTRAINED: OperandRole = new OperandRole(
    OperandRole.HIGHER_ORDER | OperandRole.CONSTRAINED_CLASS,
    OperandUsage.NAVIGATION,
    SequenceType.ANY_SEQUENCE)

  val SINGLE_ATOMIC: OperandRole =
    new OperandRole(0, OperandUsage.ABSORPTION, SequenceType.SINGLE_ATOMIC)

  val ATOMIC_SEQUENCE: OperandRole =
    new OperandRole(0, OperandUsage.ABSORPTION, SequenceType.ATOMIC_SEQUENCE)

  val NEW_FOCUS_ATOMIC: OperandRole = new OperandRole(
    OperandRole.USES_NEW_FOCUS | OperandRole.HIGHER_ORDER,
    OperandUsage.ABSORPTION,
    SequenceType.ATOMIC_SEQUENCE)

  val PATTERN: OperandRole = new OperandRole(
    OperandRole.USES_NEW_FOCUS | OperandRole.HIGHER_ORDER |
      OperandRole.CONSTRAINED_CLASS,
    OperandUsage.ABSORPTION,
    SequenceType.ATOMIC_SEQUENCE)

  def getTypeDeterminedUsage(`type`: ItemType): OperandUsage =
    if (`type`.isInstanceOf[FunctionItemType]) {
      OperandUsage.INSPECTION
    } else if (`type`.isInstanceOf[PlainType]) {
      OperandUsage.ABSORPTION
    } else {
      OperandUsage.NAVIGATION
    }

}

/**
  * Defines the role of a child expression relative to its parent expression. The properties
  * of an operand role depend only on the kind of expression and not on the actual arguments
  * supplied to a specific instance of the kind of operand.
  */
class OperandRole(var properties: Int, @BeanProperty var usage: OperandUsage) {

  @BeanProperty
  var requiredType: SequenceType = SequenceType.ANY_SEQUENCE

  def this(properties: Int, usage: OperandUsage, requiredType: SequenceType) = {
    this(properties,usage)
    this.requiredType = requiredType
  }

  def setsNewFocus(): Boolean = (properties & SETS_NEW_FOCUS) != 0

  def hasSameFocus: Boolean =
    (properties & (USES_NEW_FOCUS | HAS_SPECIAL_FOCUS_RULES)) == 0

  def hasSpecialFocusRules: Boolean =
    (properties & HAS_SPECIAL_FOCUS_RULES) != 0

  def isHigherOrder: Boolean = (properties & HIGHER_ORDER) != 0

  def isEvaluatedRepeatedly: Boolean =
    ((properties & HIGHER_ORDER) != 0) && ((properties & SINGLETON) == 0)

  def isConstrainedClass: Boolean = (properties & CONSTRAINED_CLASS) != 0

  def isInChoiceGroup: Boolean = (properties & IN_CHOICE_GROUP) != 0

  def modifyProperty(property: Int, on: Boolean): OperandRole = {
    val newProp: Int =
      if (on) (properties | property) else (properties & ~property)
    new OperandRole(newProp, usage, requiredType)
  }

  def getProperties: Int = properties

}

// Copyright (c) 2018-2020 Saxonica Limited
// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is "Incompatible With Secondary Licenses", as defined by the Mozilla Public License, v. 2.0.
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
