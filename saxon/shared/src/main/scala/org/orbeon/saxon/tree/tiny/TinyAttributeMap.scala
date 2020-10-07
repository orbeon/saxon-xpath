////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// Copyright (c) 2018-2020 Saxonica Limited
// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is "Incompatible With Secondary Licenses", as defined by the Mozilla Public License, v. 2.0.
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
/**
  * An implementation of the AttributeMap interface based directly on the
  * TinyTree data structure.
  */

package org.orbeon.saxon.tree.tiny

import org.orbeon.saxon.event.ReceiverOption
import org.orbeon.saxon.expr.parser.Loc
import org.orbeon.saxon.om._
import org.jetbrains.annotations.NotNull

//import scala.collection.compat._
import scala.jdk.CollectionConverters._


class TinyAttributeMap(private var tree: TinyTree, private var element: Int)
    extends AttributeMap {

  private var firstAttribute: Int = tree.alpha(element)

  override def size: Int = {
    var i: Int = firstAttribute
    while (i < tree.numberOfAttributes && tree.attParent(i) == element)
      i += 1
    i - firstAttribute
  }

  override def get(name: NodeName): AttributeInfo = null
  override def get(uri: String, local: String): AttributeInfo = null
  override def getByFingerprint(fingerprint: Int, namePool: NamePool): AttributeInfo = null

  @NotNull
  override def iterator: collection.Iterator[AttributeInfo] =
    new AttributeInfoIterator(tree, element).asScala

  override def itemAt(index: Int): AttributeInfo = {
    val attNr: Int = firstAttribute + index
    val nc: Int = tree.attCode(attNr)
    val nodeName: CodedName = new CodedName(
      nc & NamePool.FP_MASK,
      tree.prefixPool.getPrefix(nc >> 20),
      tree.getNamePool)
    new AttributeInfo(nodeName,
                      tree.getAttributeType(attNr),
                      tree.attValue(attNr).toString,
                      Loc.NONE,
                      ReceiverOption.NONE)
  }
}
