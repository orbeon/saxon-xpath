////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
package net.sf.saxon.expr.sort

import net.sf.saxon.om.Item

import net.sf.saxon.om.NodeInfo

import GlobalOrderComparer._

import scala.beans.{BeanProperty, BooleanBeanProperty}




object GlobalOrderComparer {

  @BeanProperty
  var instance: GlobalOrderComparer = new GlobalOrderComparer()

}

class GlobalOrderComparer extends ItemOrderComparer {

  def compare(a: Item, b: Item): Int = {
    if (a == b) {
      return 0
    }
    val d1: Long = a.asInstanceOf[NodeInfo].getTreeInfo.getDocumentNumber
    val d2: Long = b.asInstanceOf[NodeInfo].getTreeInfo.getDocumentNumber
    if (d1 == d2) {
      a.asInstanceOf[NodeInfo].compareOrder(b.asInstanceOf[NodeInfo])
    }
    java.lang.Long.signum(d1 - d2)
  }

}

// Copyright (c) 2018-2020 Saxonica Limited
// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is "Incompatible With Secondary Licenses", as defined by the Mozilla Public License, v. 2.0.
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
/**
  * A Comparer used for comparing nodes in document order. This
  * comparer is used when there is no guarantee that the nodes being compared
  * come from the same document
  */
