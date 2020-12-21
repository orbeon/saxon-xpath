////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
package org.orbeon.saxon.tree.tiny

import org.orbeon.saxon.event.ReceiverOption

import org.orbeon.saxon.expr.parser.Loc

import org.orbeon.saxon.om.AttributeInfo

import org.orbeon.saxon.om.CodedName

import org.orbeon.saxon.om.NamePool

import java.util.Iterator




class AttributeInfoIterator(private var tree: TinyTree,
                            private var element: Int)
    extends Iterator[AttributeInfo] {

  private var index: Int = tree.alpha(element)

  override def hasNext: Boolean =
    index < tree.numberOfAttributes && tree.attParent(index) == element

  def next(): AttributeInfo = {
    val nc: Int = tree.attCode(index)
    val nodeName: CodedName = new CodedName(
      nc & NamePool.FP_MASK,
      tree.prefixPool.getPrefix(nc >> 20),
      tree.getNamePool)
    val info: AttributeInfo = new AttributeInfo(nodeName,
                                                tree.getAttributeType(index),
                                                tree.attValue(index).toString,
                                                Loc.NONE,
                                                ReceiverOption.NONE) {
      index += 1; index - 1
    }
    info
  }

}

// Copyright (c) 2018-2020 Saxonica Limited
// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is "Incompatible With Secondary Licenses", as defined by the Mozilla Public License, v. 2.0.
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
/**
  * AttributeIterator is an iterator over all the attribute nodes of an Element in the TinyTree.
  */
