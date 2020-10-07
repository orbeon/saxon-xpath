////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
package org.orbeon.saxon.expr.sort

import org.orbeon.saxon.om.Item

import org.orbeon.saxon.om.NodeInfo

import LocalOrderComparer._

import scala.beans.{BeanProperty, BooleanBeanProperty}




object LocalOrderComparer {

  @BeanProperty
  var instance: LocalOrderComparer = new LocalOrderComparer()

}

class LocalOrderComparer extends ItemOrderComparer {

  def compare(a: Item, b: Item): Int = {
    val n1: NodeInfo = a.asInstanceOf[NodeInfo]
    val n2: NodeInfo = b.asInstanceOf[NodeInfo]
    n1.compareOrder(n2)
  }

}

// Copyright (c) 2018-2020 Saxonica Limited
// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is "Incompatible With Secondary Licenses", as defined by the Mozilla Public License, v. 2.0.
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
/**
  * A Comparer used for comparing nodes in document order. This
  * comparer assumes that the nodes being compared come from the same document
  *
  * @author Michael H. Kay
  */
