////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
package net.sf.saxon.expr.instruct

import net.sf.saxon.expr.Expression

import net.sf.saxon.model.AnyItemType

import net.sf.saxon.model.ItemType

import net.sf.saxon.trace.ExpressionPresenter

import net.sf.saxon.trans.XPathException

import java.util.ArrayList

import java.util.List

import scala.beans.{BeanProperty, BooleanBeanProperty}




/**
  * Information about the requirements placed by a query or stylesheet on the global
  * context item: whether it is mandatory or optional, what its type must be, and
  * whether it has a default value.
  *
  * In XSLT, if more than one module specifies a global context item type, they must be the same.
  * In XQuery, several modules can specify different required types, and the actual context item
  * must satisfy them all.
  */
class GlobalContextRequirement {

  @BooleanBeanProperty
  var mayBeOmitted: Boolean = true

  @BooleanBeanProperty
  var absentFocus: Boolean = _

// XQuery only
  @BooleanBeanProperty
  var external: Boolean = _

  @BeanProperty
  var requiredItemTypes: List[ItemType] = new ArrayList()

// Used in XQuery only
  @BeanProperty
  var defaultValue: Expression = null

  def getRequiredItemType: ItemType =
    if (requiredItemTypes.isEmpty) {
      AnyItemType
    } else {
      requiredItemTypes.get(0)
    }

  def addRequiredItemType(requiredItemType: ItemType): Unit = {
    requiredItemTypes.add(requiredItemType)
  }

  def export(out: ExpressionPresenter): Unit = {
    out.startElement("glob")
    var use: String = null
    use = if (isMayBeOmitted) if (isAbsentFocus) "pro" else "opt" else "req"
    out.emitAttribute("use", use)
    if (getRequiredItemType != AnyItemType) {
      out.emitAttribute("type", getRequiredItemType.toExportString)
    }
    out.endElement()
  }

}

// Copyright (c) 2013-2020 Saxonica Limited
// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is "Incompatible With Secondary Licenses", as defined by the Mozilla Public License, v. 2.0.
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// Copyright (c) 2018-2020 Saxonica Limited
