////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
package org.orbeon.saxon.expr

import org.orbeon.saxon.expr.parser.ExpressionTool

import org.orbeon.saxon.expr.parser.RebindingMap

import org.orbeon.saxon.om.GroundedValue

import org.orbeon.saxon.value.StringValue




/**
  * Subclass of Literal used specifically for string literals, as this is a common case
  */
class StringLiteral(value: StringValue)
    extends Literal(value.asInstanceOf[GroundedValue]) {

  def this(value: CharSequence) = this(StringValue.makeStringValue(value))

  /**
    * Get the value represented by this Literal
    *
    * @return the constant value
    */
  override def getValue(): StringValue =
    super.getValue.asInstanceOf[StringValue]

  def getStringValue: String = getValue.getStringValue

  /*@NotNull*/

  override def copy(rebindings: RebindingMap): Expression = {
    val stringLiteral: StringLiteral = new StringLiteral(getValue)
    ExpressionTool.copyLocationInfo(this, stringLiteral)
    stringLiteral
  }

}

// Copyright (c) 2018-2020 Saxonica Limited
// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is "Incompatible With Secondary Licenses", as defined by the Mozilla Public License, v. 2.0.
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
