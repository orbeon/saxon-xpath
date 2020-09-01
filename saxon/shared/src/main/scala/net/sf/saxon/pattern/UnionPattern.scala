package net.sf.saxon.pattern

import net.sf.saxon.expr.XPathContext

import net.sf.saxon.expr.parser.ExpressionTool

import net.sf.saxon.expr.parser.RebindingMap

import net.sf.saxon.model.ItemType

import net.sf.saxon.model.Type

import net.sf.saxon.model.UType

import net.sf.saxon.om.Item

import net.sf.saxon.om.NodeInfo

import java.util.HashSet

import java.util.Set

class UnionPattern(p1: Pattern, p2: Pattern) extends VennPattern(p1, p2) {

  this.priority = java.lang.Double.NaN

  override def getItemType: ItemType = {
    val t1: ItemType = p1.getItemType
    val t2: ItemType = p2.getItemType
    Type.getCommonSuperType(t1, t2)
  }

  override def getUType(): UType = p1.getUType.union(p2.getUType)

  def matches(item: Item, context: XPathContext): Boolean =
    p1.matches(item, context) || p2.matches(item, context)

  override def matchesBeneathAnchor(node: NodeInfo,
                                    anchor: NodeInfo,
                                    context: XPathContext): Boolean =
    p1.matchesBeneathAnchor(node, anchor, context) || p2.matchesBeneathAnchor(
      node,
      anchor,
      context)

  override def convertToTypedPattern(`val`: String): Pattern = {
    val np1: Pattern = p1.convertToTypedPattern(`val`)
    val np2: Pattern = p2.convertToTypedPattern(`val`)
    if (p1 == np1 && p2 == np2) {
      this
    } else {
      new UnionPattern(np1, np2)
    }
  }

  override def equals(other: Any): Boolean = other match {
    case other: UnionPattern => {
      val s0: Set[Pattern] = new HashSet[Pattern](10)
      gatherComponentPatterns(s0)
      val s1: Set[Pattern] = new HashSet[Pattern](10)
      other.gatherComponentPatterns(s1)
      s0 == s1
    }
    case _ => false

  }

  override def computeHashCode(): Int = 0x9bd723a6 ^ p1.hashCode ^ p2.hashCode

   override def getOperatorName(): String = "union"

  def copy(rebindings: RebindingMap): Pattern = {
    val n: UnionPattern =
      new UnionPattern(p1.copy(rebindings), p2.copy(rebindings))
    ExpressionTool.copyLocationInfo(this, n)
    n
  }

}
