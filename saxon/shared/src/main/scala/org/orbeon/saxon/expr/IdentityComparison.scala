////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
package org.orbeon.saxon.expr

import org.orbeon.saxon.expr.parser._

import org.orbeon.saxon.expr.sort.GlobalOrderComparer

import org.orbeon.saxon.model.BuiltInAtomicType

import org.orbeon.saxon.model.ItemType

import org.orbeon.saxon.model.UType

import org.orbeon.saxon.om.NodeInfo

import org.orbeon.saxon.trans.XPathException

import org.orbeon.saxon.value.BooleanValue

import org.orbeon.saxon.value.Cardinality

import org.orbeon.saxon.value.SequenceType

import IdentityComparison._

import scala.beans.{BeanProperty, BooleanBeanProperty}




object IdentityComparison {

  private def getNode(exp: Expression, c: XPathContext): NodeInfo =
    exp.evaluateItem(c).asInstanceOf[NodeInfo]

}

class IdentityComparison(p1: Expression, op: Int, p2: Expression)
    extends BinaryExpression(p1, op, p2) {

  @BooleanBeanProperty
  var generateIdEmulation: Boolean = false

  /*@NotNull*/

  override def typeCheck(visitor: ExpressionVisitor,
                contextInfo: ContextItemStaticInfo): Expression = {
    getLhs.typeCheck(visitor, contextInfo)
    getRhs.typeCheck(visitor, contextInfo)
    if (!generateIdEmulation) {
      if (Literal.isEmptySequence(getLhsExpression) || Literal.isEmptySequence(
            getRhsExpression)) {
        Literal.makeEmptySequence
      }
    }
    val role0: RoleDiagnostic =
      new RoleDiagnostic(RoleDiagnostic.BINARY_EXPR, Token.tokens(op), 0)
    val tc: TypeChecker = visitor.getConfiguration.getTypeChecker(false)
//role0.setSourceLocator(this);
    this.setLhsExpression(tc.staticTypeCheck(getLhsExpression,
                                            SequenceType.OPTIONAL_NODE,
                                            role0,
                                            visitor))
    val role1: RoleDiagnostic =
      new RoleDiagnostic(RoleDiagnostic.BINARY_EXPR, Token.tokens(op), 1)
//role1.setSourceLocator(this);
    this.setRhsExpression(tc.staticTypeCheck(getRhsExpression,
                                            SequenceType.OPTIONAL_NODE,
                                            role1,
                                            visitor))
    if (!Cardinality.allowsZero(getLhsExpression.getCardinality) &&
        !Cardinality.allowsZero(getRhsExpression.getCardinality)) {
      generateIdEmulation = false
    }
// because the flag only makes a difference if one of the operands evaluates to ()
// because the flag only makes a difference if one of the operands evaluates to ()
    this
  }

  /*@NotNull*/

  override def optimize(visitor: ExpressionVisitor,
               contextItemType: ContextItemStaticInfo): Expression = {
    val r: Expression = super.optimize(visitor, contextItemType)
    if (r != this) {
      if (!generateIdEmulation) {
        if (Literal.isEmptySequence(getLhsExpression) || Literal
              .isEmptySequence(getRhsExpression)) {
          Literal.makeEmptySequence
        }
      }
    }
    r
  }

   override def getOperandRole(arg: Int): OperandRole =
    OperandRole.INSPECT

  /*@NotNull*/

  def copy(rebindings: RebindingMap): Expression = {
    val ic: IdentityComparison = new IdentityComparison(
      getLhsExpression.copy(rebindings),
      op,
      getRhsExpression.copy(rebindings))
    ExpressionTool.copyLocationInfo(this, ic)
    ic.generateIdEmulation = generateIdEmulation
    ic
  }

  /**
    * Get the element name used to identify this expression in exported expression format
    *
    * @return the element name used to identify this expression
    */
   override def tag(): String = op match {
    case Token.IS => "is"
    case Token.PRECEDES => "precedes"
    case Token.FOLLOWS => "follows"
    case _ => "?"

  }

  /*@Nullable*/

  override def evaluateItem(context: XPathContext): BooleanValue = {
    val node0: NodeInfo = getNode(getLhsExpression, context)
    if (node0 == null) {
      if (generateIdEmulation) {
        BooleanValue.get(getNode(getRhsExpression, context) == null)
      }
      return null
    }
    val node1: NodeInfo = getNode(getRhsExpression, context)
    if (node1 == null) {
      if (generateIdEmulation) {
        BooleanValue.FALSE
      }
      return null
    }
    BooleanValue.get(compareIdentity(node0, node1))
  }

  override def effectiveBooleanValue(context: XPathContext): Boolean = {
    val node0: NodeInfo = getNode(getLhsExpression, context)
    if (node0 == null) {
      generateIdEmulation && getNode(getRhsExpression, context) == null
    }
    val node1: NodeInfo = getNode(getRhsExpression, context)
    node1 != null && compareIdentity(node0, node1)
  }

  private def compareIdentity(node0: NodeInfo, node1: NodeInfo): Boolean =
    op match {
      case Token.IS => node0 == node1
      case Token.PRECEDES =>
        GlobalOrderComparer.getInstance.compare(node0, node1) <
          0
      case Token.FOLLOWS =>
        GlobalOrderComparer.getInstance.compare(node0, node1) >
          0
      case _ =>
        throw new UnsupportedOperationException("Unknown node identity test")

    }

  /*@NotNull*/

  def getItemType: ItemType = BuiltInAtomicType.BOOLEAN

  /**
    * Get the static type of the expression as a UType, following precisely the type
    * inference rules defined in the XSLT 3.0 specification.
    *
    * @return the static item type of the expression according to the XSLT 3.0 defined rules
    * @param contextItemType
    */
  override def getStaticUType(contextItemType: UType): UType = UType.BOOLEAN

  /**
    * Get a name identifying the kind of expression, in terms meaningful to a user.
    *
    * @return a name identifying the kind of expression, in terms meaningful to a user.
    * The name will always be in the form of a lexical XML QName, and should match the name used
    * in export() output displaying the expression.
    */
  override def getExpressionName: String = "nodeComparison"

}

// Copyright (c) 2018-2020 Saxonica Limited
// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is "Incompatible With Secondary Licenses", as defined by the Mozilla Public License, v. 2.0.
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
/**
  * IdentityComparison: a boolean expression that compares two nodes
  * for equals, not-equals, greater-than or less-than based on identity and
  * document ordering
  */
