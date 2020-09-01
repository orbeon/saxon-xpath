////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
package net.sf.saxon.pattern

import net.sf.saxon.expr.XPathContext

import net.sf.saxon.expr.parser.ExpressionTool

import net.sf.saxon.expr.parser.RebindingMap

import net.sf.saxon.model.ItemType

import net.sf.saxon.model.UType

import net.sf.saxon.om.Item

import net.sf.saxon.om.NodeInfo

import net.sf.saxon.trans.XPathException

import java.util.HashSet

import java.util.Set




class ExceptPattern(p1: Pattern, p2: Pattern) extends VennPattern(p1, p2) {

  /**
    * Get an ItemType that all the items matching this pattern must satisfy
    */
  override def getItemType: ItemType = p1.getItemType

  /**
    * Get a UType indicating which kinds of items this Pattern can match.
    *
    * @return a UType indicating all the primitive types of item that the pattern can match.
    */
  override def getUType(): UType = p1.getUType

  override def getDefaultPriority: Double = p1.getDefaultPriority

  def matches(item: Item, context: XPathContext): Boolean =
    p1.matches(item, context) && !p2.matches(item, context)

  override def matchesBeneathAnchor(node: NodeInfo,
                                    anchor: NodeInfo,
                                    context: XPathContext): Boolean =
    p1.matchesBeneathAnchor(node, anchor, context) && !p2.matchesBeneathAnchor(
      node,
      anchor,
      context)

  /**
    * Convert the pattern to a typed pattern, in which an element name is treated as
    * schema-element(N)
    *
    * @param val either "strict" or "lax" depending on the value of xsl:mode/@typed
    * @return either the original pattern unchanged, or a new pattern as the result of the
    *         conversion
    * @throws net.sf.saxon.trans.XPathException
    *          if the pattern cannot be converted
    */
  override def convertToTypedPattern(`val`: String): Pattern = {
    val np1: Pattern = p1.convertToTypedPattern(`val`)
    val np2: Pattern = p2.convertToTypedPattern(`val`)
    if (p1 == np1 && p2 == np2) {
      this
    } else {
      new ExceptPattern(np1, np2)
    }
  }

  override def equals(other: Any): Boolean = other match {
    case other: ExceptPattern => {
      val s0: Set[Pattern] = new HashSet[Pattern](10)
      gatherComponentPatterns(s0)
      val s1: Set[Pattern] = new HashSet[Pattern](10)
      other.gatherComponentPatterns(s1)
      s0 == s1
    }
    case _ => false

  }

  override def computeHashCode(): Int = 0x9bd7dfa6 ^ p1.hashCode ^ p2.hashCode

  /**
    * Get the relevant operator: "union", "intersect", or "except"
    *
    * @return the operator, as a string
    */
   override def getOperatorName(): String = "except"

  /*@NotNull*/

  def copy(rebindings: RebindingMap): Pattern = {
    val n: ExceptPattern =
      new ExceptPattern(p1.copy(rebindings), p2.copy(rebindings))
    ExpressionTool.copyLocationInfo(this, n)
    n
  }

}

// Copyright (c) 2018-2020 Saxonica Limited
// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is "Incompatible With Secondary Licenses", as defined by the Mozilla Public License, v. 2.0.
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
/**
  * A pattern formed as the difference of two other patterns
  */
