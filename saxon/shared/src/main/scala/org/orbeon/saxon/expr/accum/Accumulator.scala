////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
package org.orbeon.saxon.expr.accum

import org.orbeon.saxon.expr.Component
import org.orbeon.saxon.expr.Expression
import org.orbeon.saxon.expr.Literal
import org.orbeon.saxon.expr.instruct.Actor
import org.orbeon.saxon.expr.instruct.Block
import org.orbeon.saxon.expr.instruct.SlotManager
import org.orbeon.saxon.lib.NamespaceConstant
import org.orbeon.saxon.om.StandardNames
import org.orbeon.saxon.om.StructuredQName
import org.orbeon.saxon.trace.ExpressionPresenter
import org.orbeon.saxon.trans.{Mode, SimpleMode, SymbolicName, XPathException}
import org.orbeon.saxon.trans.rules.Rule
import org.orbeon.saxon.value.SequenceType
import java.util.Map

import scala.beans.{BeanProperty, BooleanBeanProperty}


/**
 * Represents a single accumulator declared in an XSLT 3.0 stylesheet
 */
class Accumulator extends Actor {

  @BeanProperty
  var accumulatorName: StructuredQName = _

  @BeanProperty
  var preDescentRules: SimpleMode = new SimpleMode(
    new StructuredQName("saxon", NamespaceConstant.SAXON, "preDescent"))

  @BeanProperty
  var postDescentRules: SimpleMode = new SimpleMode(
    new StructuredQName("saxon", NamespaceConstant.SAXON, "postDescent"))

  @BeanProperty
  var initialValueExpression: Expression = _

  @BeanProperty
  var seqType: SequenceType = _

  private var streamable: Boolean = _

  @BooleanBeanProperty
  var universallyApplicable: Boolean = _

  @BeanProperty
  var importPrecedence: Int = _

  @BooleanBeanProperty
  var tracing: Boolean = _

  @BeanProperty
  var slotManagerForInitialValueExpression: SlotManager = _

  // The "body" of an accumulator is an artificial expression that contains all the constituent expressions, for ease of management.
  body = Literal.makeEmptySequence

  /**
   * Get the symbolic name of the component
   *
   * @return the symbolic name
   */
  override def getSymbolicName: SymbolicName =
    new SymbolicName(StandardNames.XSL_ACCUMULATOR, getAccumulatorName)

  def isDeclaredStreamable: Boolean = streamable

  def setDeclaredStreamable(streamable: Boolean): Unit = {
    this.streamable = streamable
  }

  def addChildExpression(expression: Expression): Unit = {
    val e: Expression = Block.makeBlock(getBody, expression)
    this.body = e
  }

  def isCompatible(other: Accumulator): Boolean =
    getAccumulatorName == other.getAccumulatorName

  /**
   * Get a name identifying the object of the expression, for example a function name, template name,
   * variable name, key name, element name, etc. This is used only where the name is known statically.
   *
   * @return the QName of the object declared or manipulated by this instruction or expression
   */
  def getObjectName: StructuredQName = accumulatorName

  /**
   * Export expression structure. The abstract expression tree
   * is written to the supplied outputstream.
   *
   * @param presenter the expression presenter used to generate the XML representation of the structure
   */
  override def export(presenter: ExpressionPresenter): Unit = {
    export(presenter, null)
  }

  def export(out: ExpressionPresenter,
             componentIdMap: Map[Component, Integer]): Unit = {
    //        }
    out.startElement("accumulator")
    out.emitAttribute("name", getObjectName)
    out.emitAttribute("line", getLineNumber.toString)
    out.emitAttribute("module", getSystemId)
    out.emitAttribute("as", seqType.toAlphaCode)
    out.emitAttribute("streamable", if (streamable) "1" else "0")
    out.emitAttribute(
      "slots",
      getSlotManagerForInitialValueExpression.getNumberOfVariables.toString +
        "")
    if (componentIdMap != null) {
      out.emitAttribute(
        "binds",
        "" +
          getDeclaringComponent.listComponentReferences(componentIdMap))
    }
    if (isUniversallyApplicable) {
      out.emitAttribute("flags", "u")
    }
    out.setChildRole("init")
    initialValueExpression.export(out)
    val action: Mode.RuleAction = new Mode.RuleAction {
      def processRule(r: Rule): Unit = {
        out.startElement("accRule")
        out.emitAttribute("slots",
          r.getAction
            .asInstanceOf[AccumulatorRule]
            .getStackFrameMap
            .getNumberOfVariables.toString +
            "")
        out.emitAttribute("rank", "" + r.getRank)
        if (r.getAction.asInstanceOf[AccumulatorRule].isCapturing) {
          out.emitAttribute("flags", "c")
        }
        r.getPattern.export(out)
        r.getAction.export(out)
        out.endElement()
      }
    }
    out.startElement("pre")
    out.emitAttribute("slots", preDescentRules.getStackFrameSlotsNeeded.toString)
    preDescentRules.processRules(action)
    out.endElement()
    out.startElement("post")
    out.emitAttribute("slots", postDescentRules.getStackFrameSlotsNeeded.toString)
    postDescentRules.processRules(action)
    out.endElement()
    out.endElement()
  }

  //        if ("JS".equals(out.getOption("target"))) {
  //            throw new XPathException("xsl:accumulator is not supported in Saxon-JS", SaxonErrorCode.SXJS0001);
  //        if ("JS".equals(out.getOption("target"))) {
  //            throw new XPathException("xsl:accumulator is not supported in Saxon-JS", SaxonErrorCode.SXJS0001);

}

// Copyright (c) 2018-2020 Saxonica Limited
// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is "Incompatible With Secondary Licenses", as defined by the Mozilla Public License, v. 2.0.
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
