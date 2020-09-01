////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// Copyright (c) 2018-2020 Saxonica Limited
// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is "Incompatible With Secondary Licenses", as defined by the Mozilla Public License, v. 2.0.
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
package net.sf.saxon.expr

import java.io.{PrintWriter, StringWriter}
import java.util.Iterator

import net.sf.saxon.expr.Operand._
import net.sf.saxon.expr.OperandUsage.OperandUsage
import net.sf.saxon.expr.parser.{ContextItemStaticInfo, ExpressionTool, ExpressionVisitor}
import net.sf.saxon.model.AnyItemType
import net.sf.saxon.pattern.NodeTest
import net.sf.saxon.trans.{XPathException, XmlProcessingException}
import net.sf.saxon.tree.jiter.MonoIterator
import net.sf.saxon.value.SequenceType

import scala.beans.BeanProperty


/**
  * Information about a sub-expression and its relationship to the parent expression
  */
object Operand {

  private val DEBUG: Boolean = false

  def typeDeterminedUsage(`type`: net.sf.saxon.model.ItemType): OperandUsage =
    if (`type`.isPlainType) {
      OperandUsage.ABSORPTION
    } else if (`type`.isInstanceOf[NodeTest] || `type` == AnyItemType) {
      OperandUsage.NAVIGATION
    } else {
      OperandUsage.INSPECTION
    }
}

class Operand(@BeanProperty val parentExpression: Expression,
              var childExpression: Expression,
              private var role: OperandRole)
    extends java.lang.Iterable[Operand]
    with ExpressionOwner {

  def setChildExpression(childExpression: Expression): Unit = {
    if (childExpression != this.childExpression) {
      if (role.isConstrainedClass && this.childExpression != null &&
          childExpression.getClass != this.childExpression.getClass) {
        throw new AssertionError()
      }
      this.childExpression = childExpression
      parentExpression.adoptChildExpression(childExpression)
      parentExpression.resetLocalStaticProperties()
    }
//childExpression.verifyParentPointers();
//childExpression.verifyParentPointers();
  }

  def getChildExpression: Expression = childExpression

  def detachChild(): Unit = {
    if (DEBUG) {
      childExpression.setParentExpression(null)
      val sw = new StringWriter()
      new XPathException("dummy").printStackTrace(new PrintWriter(sw))
      childExpression = new ErrorExpression(
        "child expression has been detached: " + sw.toString,
        "ZZZ",
        false)
      ExpressionTool.copyLocationInfo(parentExpression, childExpression)
    }
  }

  def getOperandRole: OperandRole = role

  def setOperandRole(role: OperandRole): Unit = {
    this.role = role
  }

  /**
    * Ask whether the child expression sets a new focus for evaluation of other operands
    *
    * @return true if the child expression is evaluated with the same focus as its parent expression
    */
  def setsNewFocus(): Boolean = role.setsNewFocus()

  def hasSpecialFocusRules: Boolean = role.hasSpecialFocusRules

  /**
    * Ask whether the child expression is evaluated with the same focus as its parent expression
    *
    * @return true if the child expression is evaluated with the same focus as its parent expression
    */
  def hasSameFocus: Boolean = role.hasSameFocus

  /**
    * Ask whether the operand is a higher-order operand,: typically this means that the child expression
    * is evaluated repeatedly during a single evaluation of the parent expression (but there are some cases
    * like xsl:for-each select=".." where the operand is higher-order despite not being evaluated
    * repeatedly).
    *
    * @return true if the operand is higher-order
    */
  def isHigherOrder: Boolean = role.isHigherOrder

  /**
    * Ask whether the operand is is evaluated repeatedly during a single evaluation of the parent
    * expression. This is true if the operand is higher-order and the operand does not have the
    * {@link OperandRole#SINGLETON} property.
    *
    * @return true if the operand is higher-order
    */
  def isEvaluatedRepeatedly: Boolean = role.isEvaluatedRepeatedly

  /**
    * Get the usage of the operand
    *
    * @return the usage
    */
  def getUsage: OperandUsage = role.getUsage

  def setUsage(usage: OperandUsage): Unit = {
    role = new OperandRole(role.properties, usage, role.getRequiredType)
  }

  /**
    * Get the required type of the operand
    *
    * @return the required type
    */
  def getRequiredType: SequenceType = role.getRequiredType

  def isInChoiceGroup: Boolean = role.isInChoiceGroup

  def iterator(): Iterator[Operand] = new MonoIterator(this).asInstanceOf[Iterator[Operand]]

  def typeCheck(visitor: ExpressionVisitor,
                contextInfo: ContextItemStaticInfo): Unit = {
    try this.childExpression =
      getChildExpression.typeCheck(visitor, contextInfo)
    catch {
      case e: XPathException => {
        e.maybeSetLocation(getChildExpression.getLocation)
        if (!e.isReportableStatically) {
          visitor.getStaticContext.issueWarning(
            "Evaluation will always throw a dynamic error: " + e.getMessage,
            getChildExpression.getLocation)
          this.childExpression = new ErrorExpression(
            new XmlProcessingException(e))
        } else {
          throw e
        }
      }

    }
  }

  def optimize(visitor: ExpressionVisitor,
               contextInfo: ContextItemStaticInfo): Unit = {
    try this.childExpression =
      getChildExpression.optimize(visitor, contextInfo)
    catch {
      case e: XPathException => {
        e.maybeSetLocation(getChildExpression.getLocation)
        if (!e.isReportableStatically) {
          visitor.getStaticContext.issueWarning(
            "Evaluation will always throw a dynamic error: " + e.getMessage,
            getChildExpression.getLocation)
          this.childExpression = new ErrorExpression(
            new XmlProcessingException(e))
        } else {
          throw e
        }
      }

    }
  }

}