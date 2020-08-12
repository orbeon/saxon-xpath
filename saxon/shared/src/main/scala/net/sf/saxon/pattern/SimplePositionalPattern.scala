////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
package net.sf.saxon.pattern

import net.sf.saxon.expr.XPathContext

import net.sf.saxon.expr.parser.ExpressionTool

import net.sf.saxon.expr.parser.RebindingMap

import net.sf.saxon.model.AlphaCode

import net.sf.saxon.model.ItemType

import net.sf.saxon.model.UType

import net.sf.saxon.om.Item

import net.sf.saxon.om.NodeInfo

import net.sf.saxon.trace.ExpressionPresenter

import net.sf.saxon.tree.util.Navigator

import scala.beans.{BeanProperty, BooleanBeanProperty}




class SimplePositionalPattern(@BeanProperty var nodeTest: NodeTest,
                              @BeanProperty var position: Int)
    extends Pattern {

  def matches(item: Item, context: XPathContext): Boolean =
    item.isInstanceOf[NodeInfo] &&
      matchesBeneathAnchor(item.asInstanceOf[NodeInfo], null, context)

  /**
    * Get a UType indicating which kinds of items this Pattern can match.
    *
    * @return a UType indicating all the primitive types of item that the pattern can match.
    */
  override def getUType(): UType = nodeTest.getUType

  override def getFingerprint(): Int = nodeTest.getFingerprint

  override def getItemType(): ItemType = nodeTest.getPrimitiveItemType

  override def equals(other: Any): Boolean = other match {
    case other: SimplePositionalPattern => {
      val fp: SimplePositionalPattern = other
      nodeTest == fp.nodeTest && position == fp.position
    }
    case _ => false

  }

  override def computeHashCode(): Int = nodeTest.hashCode ^ (position << 3)

  override def isMotionless(): Boolean = false

  override def matchesBeneathAnchor(node: NodeInfo,
                                    anchor: NodeInfo,
                                    context: XPathContext): Boolean = {
    if (!nodeTest.test(node)) {
      false
    }
    if (anchor != null && node.getParent != anchor) {
      false
    }
    position ==
      Navigator.getSiblingPosition(node, nodeTest, position)
  }

  /*@NotNull*/

  def copy(rebindings: RebindingMap): Pattern = {
    val n: SimplePositionalPattern =
      new SimplePositionalPattern(nodeTest.copy(), position)
    ExpressionTool.copyLocationInfo(this, n)
    n
  }

  /**
    * Get the original pattern text
    */
  override def toString(): String = nodeTest + "[" + position + "]"

  def export(presenter: ExpressionPresenter): Unit = {
    presenter.startElement("p.simPos")
    presenter.emitAttribute("test", AlphaCode.fromItemType(nodeTest))
    presenter.emitAttribute("pos", position + "")
    presenter.endElement()
  }

}

// Copyright (c) 2018-2020 Saxonica Limited
// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is "Incompatible With Secondary Licenses", as defined by the Mozilla Public License, v. 2.0.
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
/**
  * A SimplePositionalPattern is a pattern of the form A[N] where A is an axis expression using the child axis
  * and P is a numeric literal.
  */
