////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
package net.sf.saxon.trans.rules

import net.sf.saxon.expr.XPathContextMajor

import net.sf.saxon.expr.instruct.TemplateRule

import net.sf.saxon.expr.parser.RebindingMap

import net.sf.saxon.om.Item

import net.sf.saxon.pattern.Pattern

import net.sf.saxon.trace.ExpressionPresenter

import net.sf.saxon.trans.XPathException

import scala.beans.{BeanProperty, BooleanBeanProperty}




class Rule {

// The pattern that fires this rule
   var pattern: Pattern = _

// The action associated with this rule (usually a Template)
   var action: RuleTarget = _

// The import precedence
   var precedence: Int = _

// The minimum import precedence to be considered by xsl:apply-imports
   var minImportPrecedence: Int = _

// The priority of the rule
   var priority: Double = _

// The next rule after this one in the chain of rules
   var next: Rule = _

// The relative position of this rule, its position in declaration order
   var sequence: Int = _

// The relative position of this rule relative to others formed by splitting
   var part: Int = _

// True if the pattern does not need to be tested, because the rule
  @BooleanBeanProperty
  var alwaysMatches: Boolean = _

// Indicates the relative precedence/priority of a rule within a mode;
  @BeanProperty
  var rank: Int = _

  def this(p: Pattern,
           o: RuleTarget,
           prec: Int,
           min: Int,
           prio: Double,
           seq: Int,
           part: Int) = {
    this()
    pattern = p
    action = o
    precedence = prec
    minImportPrecedence = min
    priority = prio
    next = null
    sequence = seq
    this.part = part
    o.registerRule(this)
  }

   def copyFrom(r: Rule, copyChain: Boolean): Unit = {
    pattern = r.pattern.copy(new RebindingMap())
    action =
      if (r.action.isInstanceOf[TemplateRule])
        r.action.asInstanceOf[TemplateRule].copy()
      else r.action
    precedence = r.precedence
    minImportPrecedence = r.minImportPrecedence
    priority = r.priority
    sequence = r.sequence
    part = r.part
    next = if (r.next == null || !copyChain) null else r.next.copy(true)
    action.registerRule(this)
  }

  def copy(copyChain: Boolean): Rule = {
    val r2: Rule = new Rule()
    r2.copyFrom(this, copyChain)
    r2
  }

  def getSequence(): Int = sequence

  def getPartNumber(): Int = part

  def setAction(action: RuleTarget): Unit = {
    this.action = action
  }

  /*@NotNull*/

  def getAction(): RuleTarget = action

  /*@Nullable*/

  def getNext(): Rule = next

  def setNext(next: Rule): Unit = {
    this.next = next
  }

  /*@NotNull*/

  def getPattern(): Pattern = pattern

  def setPattern(pattern: Pattern): Unit = {
    this.pattern = pattern
  }

  def getPrecedence(): Int = precedence

  def getMinImportPrecedence(): Int = minImportPrecedence

  def getPriority(): Double = priority

  /**
    * Export this rule
    * @param out   the destination for the export
    * @param modeStreamable    if the mode for this rule is streamable (should be EE only?)
    */
  def export(out: ExpressionPresenter, modeStreamable: Boolean): Unit = {
    val target: RuleTarget = getAction
    var template: TemplateRule = null
    if (target.isInstanceOf[TemplateRule]) {
      template = target.asInstanceOf[TemplateRule]
      val s: Int = out.startElement("templateRule")
      out.emitAttribute("prec", getPrecedence.toString)
      out.emitAttribute("prio", getPriority.toString)
      out.emitAttribute("seq", getSequence.toString)
      if (part != 0) {
        out.emitAttribute("part", "" + part)
      }
      out.emitAttribute("rank", "" + getRank.toString)
      out.emitAttribute("minImp", getMinImportPrecedence.toString)
      out.emitAttribute("slots", template.getStackFrameMap.getNumberOfVariables.toString)
      out.emitAttribute("matches", pattern.getItemType.getFullAlphaCode)
      template.explainProperties(out)
      exportOtherProperties(out)
      out.setChildRole("match")
      getPattern.export(out)
      if (template.getBody != null) {
        out.setChildRole("action")
        template.getBody.export(out)
      }
      val e: Int = out.endElement()
      if (s != e) {
        throw new IllegalStateException(
          "exported expression tree unbalanced in template at line " +
            (if (template != null)
               template.getLineNumber.toString + " of " + template.getSystemId.toString
             else ""))
      }
    } else {
      target.export(out)
    }
  }

  /**
    * Add other exported properties as required
    * @param out  the export destination
    */
  def exportOtherProperties(out: ExpressionPresenter): Unit = ()

  def compareRank(other: Rule): Int = rank - other.rank

  def compareComputedRank(other: Rule): Int =
    if (precedence == other.precedence) {
      java.lang.Double.compare(priority, other.priority)
    } else if (precedence < other.precedence) {
      -1
    } else {
      +1
    }

  def matches(item: Item, context: XPathContextMajor): Boolean =
    alwaysMatches || pattern.matches(item, context)

}

// Copyright (c) 2018-2020 Saxonica Limited
// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is "Incompatible With Secondary Licenses", as defined by the Mozilla Public License, v. 2.0.
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
/**
  * Rule: a template rule, or a strip-space rule used to support the implementation
  */
