////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// Copyright (c) 2018-2020 Saxonica Limited
// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is "Incompatible With Secondary Licenses", as defined by the Mozilla Public License, v. 2.0.
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
package org.orbeon.saxon.expr.parser

import org.orbeon.saxon.expr.Expression

import org.orbeon.saxon.model.AnyItemType

import org.orbeon.saxon.model.ErrorType

import org.orbeon.saxon.model.ItemType

import org.orbeon.saxon.model.UType

import ContextItemStaticInfo._

import scala.beans.{BeanProperty, BooleanBeanProperty}


/**
  * A data structure that represents the required type of the context item, together
  * with information about whether it is known to be present or absent or whether it
  * is not known statically whether it is present or absent.
  */
object ContextItemStaticInfo {
  val DEFAULT: ContextItemStaticInfo = new ContextItemStaticInfo(AnyItemType, true)
  val ABSENT : ContextItemStaticInfo = new ContextItemStaticInfo(ErrorType,   true)
}

class ContextItemStaticInfo(@BeanProperty var itemType: ItemType,
                            maybeUndefined: Boolean) {

  private val contextMaybeUndefined: Boolean = maybeUndefined

  @BeanProperty
  var contextSettingExpression: Expression = _

  @BooleanBeanProperty
  var parentless: Boolean = _

  def getContextItemUType: UType = itemType.getUType
  def isPossiblyAbsent: Boolean = contextMaybeUndefined
  def setContextPostureStriding(): Unit = ()
  def setContextPostureGrounded(): Unit = ()
  def isStrictStreamabilityRules: Boolean = false
}

