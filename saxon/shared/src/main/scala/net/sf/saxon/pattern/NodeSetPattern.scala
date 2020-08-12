////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
package net.sf.saxon.pattern

import net.sf.saxon.expr._

import net.sf.saxon.expr.instruct.SlotManager

import net.sf.saxon.expr.parser._

import net.sf.saxon.model.AlphaCode

import net.sf.saxon.model.ItemType

import net.sf.saxon.model.UType

import net.sf.saxon.om._

import net.sf.saxon.trace.ExpressionPresenter

import net.sf.saxon.trans.XPathException

import net.sf.saxon.tree.iter.ManualIterator

import net.sf.saxon.value.SequenceType




class NodeSetPattern /**
  * Create a node-set pattern.
  *
  * @param exp an expression that can be evaluated to return a node-set; a node matches the pattern
  *            if it is present in this node-set. The expression must not depend on the focus, though it can depend on
  *            other aspects of the dynamic context such as local or global variables.
  */
(exp: Expression)
    extends Pattern {

  private var selectionOp: Operand =
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
  override def operands(): java.lang.Iterable[Operand] = selectionOp

  def getSelectionExpression(): Expression = selectionOp.getChildExpression

  override def typeCheck(visitor: ExpressionVisitor,
                         contextItemType: ContextItemStaticInfo): Pattern = {
    selectionOp.setChildExpression(
      getSelectionExpression.typeCheck(visitor, contextItemType))
    val role: RoleDiagnostic = new RoleDiagnostic(
      RoleDiagnostic.MATCH_PATTERN,
      getSelectionExpression.toString,
      0)
    val tc: TypeChecker = visitor.getConfiguration.getTypeChecker(false)
    var checked: Expression = getSelectionExpression
    try tc.staticTypeCheck(getSelectionExpression,
                           SequenceType.NODE_SEQUENCE,
                           role,
                           visitor)
    catch {
      case e: XPathException => {
        visitor.issueWarning(
          "Pattern will never match anything. " + e.getMessage,
          getLocation)
        checked = Literal.makeEmptySequence()
      }

    }
    selectionOp.setChildExpression(checked)
    itemType = getSelectionExpression.getItemType
    this
  }

  override def optimize(visitor: ExpressionVisitor,
                        contextInfo: ContextItemStaticInfo): Pattern = {
    visitor.obtainOptimizer().optimizeNodeSetPattern(this)
    this
  }

  def setItemType(`type`: ItemType): Unit = {
    this.itemType = `type`
  }

  override def getDependencies(): Int = getSelectionExpression.getDependencies

  override def allocateSlots(slotManager: SlotManager, nextFree: Int): Int =
    ExpressionTool.allocateSlots(getSelectionExpression, nextFree, slotManager)

  override def selectNodes(doc: TreeInfo, context: XPathContext): SequenceIterator = {
    val c2: XPathContext = context.newMinorContext()
    val mi: ManualIterator = new ManualIterator(doc.getRootNode)
    c2.setCurrentIterator(mi)
    getSelectionExpression.iterate(c2)
  }

  def matches(item: Item, context: XPathContext): Boolean =
    if (item.isInstanceOf[NodeInfo]) {
      val exp: Expression = getSelectionExpression
      if (exp.isInstanceOf[GlobalVariableReference]) {
        val value: GroundedValue =
          exp.asInstanceOf[GlobalVariableReference].evaluateVariable(context)
        value.containsNode(item.asInstanceOf[NodeInfo])
      } else {
        val iter: SequenceIterator = exp.iterate(context)
        SingletonIntersectExpression.containsNode(iter,
                                                  item.asInstanceOf[NodeInfo])
      }
    } else {
      false
    }

  /**
    * Get a UType indicating which kinds of items this Pattern can match.
    *
    * @return a UType indicating all the primitive types of item that the pattern can match.
    */
  override def getUType(): UType = getItemType.getUType

  override def getItemType(): ItemType = {
    if (itemType == null) {
      itemType = getSelectionExpression.getItemType
    }
    if (itemType.isInstanceOf[NodeTest]) {
      itemType
    } else {
      AnyNodeTest.getInstance
    }
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
    val n: NodeSetPattern = new NodeSetPattern(
      getSelectionExpression.copy(rebindings))
    ExpressionTool.copyLocationInfo(this, n)
    n
  }

  def export(presenter: ExpressionPresenter): Unit = {
    presenter.startElement("p.nodeSet")
    if (itemType != null) {
      presenter.emitAttribute("test", AlphaCode.fromItemType(itemType))
    }
    getSelectionExpression.export(presenter)
    presenter.endElement()
  }

}

// Copyright (c) 2018-2020 Saxonica Limited
// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is "Incompatible With Secondary Licenses", as defined by the Mozilla Public License, v. 2.0.
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
/**
  * A NodeSetPattern is a pattern based on an expression that is evaluated to return a set of nodes;
  * a node matches the pattern if it is a member of this node-set.
  * <p>In XSLT 2.0 there are two forms of NodeSetPattern allowed, represented by calls on the id() and
  * key() functions. In XSLT 3.0, additional forms are allowed, for example a variable reference, and
  * a call to the doc() function. This class provides the general capability to use any expression
  * at the head of a pattern. This is used also to support streaming, where streaming XPath expressions
  * are mapped to patterns.</p>
  */
