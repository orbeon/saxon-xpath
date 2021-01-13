////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// Copyright (c) 2018-2020 Saxonica Limited
// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is "Incompatible With Secondary Licenses", as defined by the Mozilla Public License, v. 2.0.
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
package org.orbeon.saxon.pattern

import org.orbeon.saxon.expr._
import org.orbeon.saxon.expr.instruct.SlotManager
import org.orbeon.saxon.expr.parser._
import org.orbeon.saxon.model.{AlphaCode, ItemType, UType}
import org.orbeon.saxon.om._
import org.orbeon.saxon.trace.ExpressionPresenter
import org.orbeon.saxon.trans.XPathException
import org.orbeon.saxon.tree.iter.ManualIterator
import org.orbeon.saxon.value.SequenceType


/**
  * A NodeSetPattern is a pattern based on an expression that is evaluated to return a set of nodes;
  * a node matches the pattern if it is a member of this node-set.
 *
  * In XSLT 2.0 there are two forms of NodeSetPattern allowed, represented by calls on the id() and
  * key() functions. In XSLT 3.0, additional forms are allowed, for example a variable reference, and
  * a call to the doc() function. This class provides the general capability to use any expression
  * at the head of a pattern. This is used also to support streaming, where streaming XPath expressions
  * are mapped to patterns.
  */
class NodeSetPattern(exp: Expression)
    extends Pattern {

  private val selectionOp: Operand =
    new Operand(this, exp, OperandRole.NAVIGATE)

  private var itemType: ItemType = _

  /**
    * Get the immediate sub-expressions of this expression, with information about the relationship
    * of each expression to its parent expression. Default implementation
    * works off the results of iterateSubExpressions()
    * <p>If the expression is a Callable, then it is required that the order of the operands
    * returned by this function is the same as the order of arguments supplied to the corresponding
    * call() method.</p>
    *
    * @return an iterator containing the sub-expressions of this expression
    */
  override def operands: java.lang.Iterable[Operand] = selectionOp

  def getSelectionExpression: Expression = selectionOp.getChildExpression

  override def typeCheck(visitor: ExpressionVisitor,
                         contextItemType: ContextItemStaticInfo): Pattern = {
    selectionOp.setChildExpression(
      getSelectionExpression.typeCheck(visitor, contextItemType))
    val role = new RoleDiagnostic(
      RoleDiagnostic.MATCH_PATTERN,
      getSelectionExpression.toString,
      0)
    val tc   = visitor.getConfiguration.getTypeChecker(false)
    var checked = getSelectionExpression
    try tc.staticTypeCheck(getSelectionExpression,
                           SequenceType.NODE_SEQUENCE,
                           role,
                           visitor)
    catch {
      case e: XPathException =>
        visitor.issueWarning(
          "Pattern will never match anything. " + e.getMessage,
          getLocation)
        checked = Literal.makeEmptySequence
    }
    selectionOp.setChildExpression(checked)
    itemType = getSelectionExpression.getItemType
    this
  }

  override def optimize(visitor: ExpressionVisitor, contextInfo: ContextItemStaticInfo): Pattern = {
    visitor.obtainOptimizer().optimizeNodeSetPattern(this)
    this
  }

  def setItemType(`type`: ItemType): Unit =
    this.itemType = `type`

  override def getDependencies(): Int = getSelectionExpression.getDependencies

  override def allocateSlots(slotManager: SlotManager, nextFree: Int): Int =
    ExpressionTool.allocateSlots(getSelectionExpression, nextFree, slotManager)

  override def selectNodes(doc: TreeInfo, context: XPathContext): SequenceIterator = {
    val c2 = context.newMinorContext()
    val mi = new ManualIterator(doc.getRootNode)
    c2.setCurrentIterator(mi)
    getSelectionExpression.iterate(c2)
  }

  def matches(item: Item, context: XPathContext): Boolean =
    item match {
      case nodeInfo: NodeInfo =>
        val exp = getSelectionExpression
        exp match {
          case varRef: GlobalVariableReference =>
            val value = varRef.evaluateVariable(context)
            value.containsNode(nodeInfo)
          case _                               =>
            val iter = exp.iterate(context)
            SingletonIntersectExpression.containsNode(
              iter,
              nodeInfo
            )
        }
      case _ =>
        false
    }

  /**
    * Get a UType indicating which kinds of items this Pattern can match.
    *
    * @return a UType indicating all the primitive types of item that the pattern can match.
    */
  def getUType: UType = getItemType.getUType

  override def getItemType: ItemType = {
    if (itemType == null)
      itemType = getSelectionExpression.getItemType
    if (itemType.isInstanceOf[NodeTest])
      itemType
    else
      AnyNodeTest
  }

  override def equals(other: Any): Boolean =
    (other.isInstanceOf[NodeSetPattern]) &&
      other
        .asInstanceOf[NodeSetPattern]
        .getSelectionExpression
        .isEqual(getSelectionExpression)

  override def computeHashCode(): Int = 0x73108728 ^ getSelectionExpression.hashCode

  /*@NotNull*/
  def copy(rebindings: RebindingMap): Pattern = {
    val n = new NodeSetPattern(getSelectionExpression.copy(rebindings))
    ExpressionTool.copyLocationInfo(this, n)
    n
  }

  def export(presenter: ExpressionPresenter): Unit = {
    presenter.startElement("p.nodeSet")
    if (itemType != null)
      presenter.emitAttribute("test", AlphaCode.fromItemType(itemType))
    getSelectionExpression.export(presenter)
    presenter.endElement()
  }
}
