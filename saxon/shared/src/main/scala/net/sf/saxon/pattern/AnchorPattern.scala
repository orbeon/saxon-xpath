////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
package net.sf.saxon.pattern

import net.sf.saxon.expr.XPathContext

import net.sf.saxon.expr.parser.ContextItemStaticInfo

import net.sf.saxon.expr.parser.ExpressionVisitor

import net.sf.saxon.expr.parser.RebindingMap

import net.sf.saxon.model.ItemType

import net.sf.saxon.model.UType

import net.sf.saxon.om.Item

import net.sf.saxon.om.NodeInfo

import net.sf.saxon.trace.ExpressionPresenter

import net.sf.saxon.trans.XPathException

import AnchorPattern._




object AnchorPattern {

  private var THE_INSTANCE: AnchorPattern = new AnchorPattern()

  def getInstance: AnchorPattern = THE_INSTANCE

}

/**
  * This is a special pattern that matches the "anchor node". It is used for the selectors
  * that arise when evaluating XPath expressions in streaming mode; the anchor
  * node is the context node for the streamed XPath evaluation.
  *
  * Given a streamed evaluation of an expression such as ./BOOKS/BOOK/PRICE, the way we evaluate
  * this is to turn it into a pattern, which is then tested against all descendant nodes.
  * Conceptually the pattern is $A/BOOKS/BOOK/PRICE, where $A is referred to as the anchor
  * node. When we evaluate the pattern against (say) a PRICE element, the match will only succeed
  * if the name of the element is "PRICE" and its ancestors are, in order, a BOOK element, a
  * BOOKS element, and the anchor node $A.
  */
class AnchorPattern  () extends Pattern {

  /**
    * Get a UType indicating which kinds of items this Pattern can match.
    *
    * @return a UType indicating all the primitive types of item that the pattern can match.
    */
  override def getUType: UType = UType.PARENT_NODE_KINDS

  override def typeCheck(visitor: ExpressionVisitor,
                         contextItemType: ContextItemStaticInfo): Pattern = this

  override def matchesBeneathAnchor(node: NodeInfo,
                                    anchor: NodeInfo,
                                    context: XPathContext): Boolean =
    anchor == null || node == anchor

  def matches(item: Item, context: XPathContext): Boolean =
    throw new AssertionError()

  override def getItemType: ItemType = AnyNodeTest.getInstance

  /*@NotNull*/

  override def toString: String = "."

  override def export(presenter: ExpressionPresenter): Unit = {
    presenter.startElement("p.anchor")
    presenter.endElement()
  }

  /*@NotNull*/

  def copy(rebindings: RebindingMap): Pattern = this

}

// Copyright (c) 2018-2020 Saxonica Limited
// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is "Incompatible With Secondary Licenses", as defined by the Mozilla Public License, v. 2.0.
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
