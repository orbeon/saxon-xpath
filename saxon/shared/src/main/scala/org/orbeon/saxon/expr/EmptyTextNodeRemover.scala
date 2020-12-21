////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
package org.orbeon.saxon.expr

import org.orbeon.saxon.expr.parser.ExpressionTool

import org.orbeon.saxon.expr.parser.RebindingMap

import org.orbeon.saxon.model.ItemType

import org.orbeon.saxon.model.Type

import org.orbeon.saxon.om.Item

import org.orbeon.saxon.om.NodeInfo

import org.orbeon.saxon.om.SequenceIterator

import org.orbeon.saxon.trans.XPathException

import Expression._


/**
  * This class performs part of the processing in "constructing simple content":
  * it takes an input sequence and eliminates empty text nodes
  * into one.
  *
  * @since 9.3
  */
class EmptyTextNodeRemover(p0: Expression)
    extends UnaryExpression(p0)
    with ItemMappingFunction {

  /*@NotNull*/

  override def getItemType: ItemType = getBaseExpression.getItemType

  override def computeCardinality(): Int =
    getBaseExpression.getCardinality | StaticProperty.ALLOWS_ZERO

   override def getOperandRole(): OperandRole =
    OperandRole.SAME_FOCUS_ACTION

  /*@NotNull*/

  def copy(rebindings: RebindingMap): Expression = {
    val e2: EmptyTextNodeRemover = new EmptyTextNodeRemover(
      getBaseExpression.copy(rebindings))
    ExpressionTool.copyLocationInfo(this, e2)
    e2
  }

  def getImplementationMethod: Int =
    ITERATE_METHOD | ITEM_FEED_METHOD | WATCH_METHOD

  /*@NotNull*/

  override def iterate(context: XPathContext): SequenceIterator =
    new ItemMappingIterator(getBaseExpression.iterate(context), this)

  /*@Nullable*/

  def mapItem(item: Item): Item =
    if (item.isInstanceOf[NodeInfo] &&
        item.asInstanceOf[NodeInfo].getNodeKind == Type.TEXT &&
        item.getStringValueCS.length == 0) {
      null
    } else {
      item
    }

  /**
    * Get the (partial) name of a class that supports streaming of this kind of expression
    *
    * @return the partial name of a class that can be instantiated to provide streaming support in Saxon-EE,
    * or null if there is no such class
    */
  override def getStreamerName: String = "EmptyTextNodeRemover"

  override def getExpressionName: String = "emptyTextNodeRemover"

}

// Copyright (c) 2018-2020 Saxonica Limited
// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is "Incompatible With Secondary Licenses", as defined by the Mozilla Public License, v. 2.0.
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
