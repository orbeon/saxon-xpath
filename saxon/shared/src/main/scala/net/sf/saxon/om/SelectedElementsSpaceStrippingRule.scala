////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// Copyright (c) 2018-2020 Saxonica Limited
// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is "Incompatible With Secondary Licenses", as defined by the Mozilla Public License, v. 2.0.
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
package net.sf.saxon.om

import net.sf.saxon.event.ProxyReceiver
import net.sf.saxon.event.Receiver
import net.sf.saxon.event.Stripper
import net.sf.saxon.model.AlphaCode
import net.sf.saxon.model.SchemaType
import net.sf.saxon.model.Type
import net.sf.saxon.pattern._

/*import net.sf.saxon.style.StylesheetModule*/
import net.sf.saxon.trace.ExpressionPresenter
import net.sf.saxon.trans.XPathException
import net.sf.saxon.trans.rules.Rule
import net.sf.saxon.tree.util.FastStringBuffer
import java.util
import scala.jdk.CollectionConverters._
import scala.util.control.Breaks._

/**
 * A whitespace stripping rule that strips elected elements unless xml:space indicates that whitespace
 * should be preserved.
 */
object SelectedElementsSpaceStrippingRule {
  private def exportRule(rule: Rule, presenter: ExpressionPresenter): Unit = {
    val which = if (rule.getAction eq Stripper.STRIP) "s"
    else "p"
    presenter.startElement(which)
    presenter.emitAttribute("test", AlphaCode.fromItemType(rule.getPattern.getItemType))
    presenter.emitAttribute("prec", rule.getPrecedence + "")
    presenter.endElement
  }

  private def exportRuleJS(rule: Rule, fsb: FastStringBuffer) = {
    val which = if (rule.getAction eq Stripper.STRIP) "true"
    else "false"
    val test = rule.getPattern.getItemType.asInstanceOf[NodeTest]
    if (test.isInstanceOf[NodeKindTest]) { // elements="*"
      fsb.append("return " + which + ";")
    }
    else if (test.isInstanceOf[NameTest]) fsb.append("if (uri=='" + test.getMatchingNodeName.getURI + "' && local=='" + test.getMatchingNodeName.getLocalPart + "') return " + which + ";")
    else if (test.isInstanceOf[NamespaceTest]) fsb.append("if (uri=='" + test.asInstanceOf[NamespaceTest].getNamespaceURI + "') return " + which + ";")
    else if (test.isInstanceOf[LocalNameTest]) fsb.append("if (local=='" + test.asInstanceOf[LocalNameTest].getLocalName + "') return " + which + ";")
    else throw new IllegalStateException("Cannot export " + test.getClass)
  }
}

class SelectedElementsSpaceStrippingRule(var rejectDuplicates: Boolean) extends SpaceStrippingRule {
  private var anyElementRule: Rule = null
  private var unnamedElementRuleChain: Rule = null
  private val namedElementRules = new util.HashMap[NodeName, Rule](32)
  private var sequence = 0

  /**
   * Decide whether an element is in the set of white-space preserving element names
   *
   * @param fingerprint Identifies the name of the element whose whitespace is to
   *                    be preserved
   * @param schemaType
   * @return ALWAYS_PRESERVE if the element is in the set of white-space preserving
   *         element types, ALWAYS_STRIP if the element is to be stripped regardless of the
   *         xml:space setting, and STRIP_DEFAULT otherwise
   */
  @throws[XPathException]
  override def isSpacePreserving(fingerprint: NodeName, schemaType: SchemaType): Int = {
    val rule = getRule(fingerprint)
    if (rule == null) return Stripper.ALWAYS_PRESERVE
    if (rule.getAction eq Stripper.PRESERVE) Stripper.ALWAYS_PRESERVE
    else Stripper.STRIP_DEFAULT
  }

  @throws[XPathException]
  def addRule(test: NodeTest, action: Stripper.StripRuleTarget, /*module: StylesheetModule,*/ lineNumber: Int): Unit = { // for fast lookup, we maintain one list for each element name for patterns that can only
    // match elements of a given name, one list for each node type for patterns that can only
    // match one kind of non-element node, and one generic list.
    // Each list is sorted in precedence/priority order so we find the highest-priority rule first
    /* val precedence = module.getPrecedence
     val minImportPrecedence = module.getMinImportPrecedence*/
    val pattern = new NodeTestPattern(test)
    //pattern.setSystemId(module.getRootElement().getSystemId());
    //pattern.setLineNumber(lineNumber);
    addRule(pattern, action, 0, 0)
  }

  @throws[XPathException]
  def addRule(pattern: NodeTestPattern, action: Stripper.StripRuleTarget, precedence: Int, minImportPrecedence: Int): Unit = {
    val test = pattern.getNodeTest
    val priority = test.getDefaultPriority
    val newRule = new Rule(pattern, action, precedence, minImportPrecedence, priority, {
      sequence += 1;
      sequence - 1
    }, 0)
    val prio = if (priority == 0) 2
    else if (priority == -0.25) 1
    else 0
    newRule.setRank((precedence << 18) + (prio << 16) + sequence)
    if (test.isInstanceOf[NodeKindTest]) {
      newRule.setAlwaysMatches(true)
      anyElementRule = addRuleToList(newRule, anyElementRule, true)
    }
    else if (test.isInstanceOf[NameTest]) {
      newRule.setAlwaysMatches(true)
      val fp = test.getFingerprint
      val pool = test.asInstanceOf[NameTest].getNamePool
      val key = new FingerprintedQName(pool.getUnprefixedQName(fp), pool)
      val chain = namedElementRules.get(key)
      namedElementRules.put(key, addRuleToList(newRule, chain, true))
    }
    else unnamedElementRuleChain = addRuleToList(newRule, unnamedElementRuleChain, false)
  }

  /**
   * Insert a new rule into this list before others of the same precedence
   * (we rely on the fact that all rules in a list have the same priority)
   *
   * @param newRule       the new rule to be added into the list
   * @param list          the Rule at the head of the list, or null if the list is empty
   * @param dropRemainder if only one rule needs to be retained
   * @return the new head of the list (which might be the old head, or the new rule if it
   *         was inserted at the start)
   */
  @throws[XPathException]
  private def addRuleToList(newRule: Rule, list: Rule, dropRemainder: Boolean): Rule = {
    if (list == null) return newRule
    val precedence = newRule.getPrecedence
    var rule = list
    var prev: Rule = null
    breakable {
      while (rule != null) if (rule.getPrecedence <= precedence) {
        if (rejectDuplicates && rule.getPrecedence == precedence && !(rule.getAction == newRule.getAction))
          throw new XPathException("There are conflicting xsl:strip-space and xsl:preserve-space declarations for " + rule.getPattern + " at the same import precedence", "XTSE0270")
        newRule.setNext(if (dropRemainder) null
        else rule)
        if (prev == null) return newRule
        else prev.setNext(newRule)
        break
      }
      else {
        prev = rule
        rule = rule.getNext
      }
    }
    if (rule == null) {
      prev.setNext(newRule)
      newRule.setNext(null)
    }
    list
  }

  /**
   * Get the rule corresponding to a given element node, by finding the best pattern match.
   *
   * @param nodeName the name of the element node to be matched
   * @return the best matching rule, if any (otherwise null).
   */
  /*@Nullable*/ def getRule(nodeName: NodeName) = { // search the specific list for this node type / node name
    var bestRule = namedElementRules.get(nodeName)
    // search the list for *:local and prefix:* node tests
    if (unnamedElementRuleChain != null) bestRule = searchRuleChain(nodeName, bestRule, unnamedElementRuleChain)
    // See if there is a "*" rule matching all elements
    if (anyElementRule != null) bestRule = searchRuleChain(nodeName, bestRule, anyElementRule)
    bestRule
  }

  /**
   * Search a chain of rules
   *
   * @param nodeName the name of the element node being matched
   * @param bestRule the best rule so far in terms of precedence and priority (may be null)
   * @param head     the rule at the head of the chain to be searched
   * @return the best match rule found in the chain, or the previous best rule, or null
   */
  private def searchRuleChain(nodeName: NodeName, bestRule: Rule, head: Rule): Rule = {
    var bstRule = bestRule
    var headRule = head
    breakable {
      while (headRule != null) {
        if (bstRule != null) {
          val rank = headRule.compareRank(bstRule)
          if (rank < 0) { // if we already have a match, and the precedence or priority of this
            // rule is lower, quit the search
            break
          }
          else if (rank == 0) { // this rule has the same precedence and priority as the matching rule already found
            if (headRule.isAlwaysMatches || headRule.getPattern.getItemType.asInstanceOf[NodeTest].matches(Type.ELEMENT, nodeName, null)) { // reportAmbiguity(bestRule, head);
              // We no longer report the recoverable error XTRE0270, we always
              // take the recovery action.
              // choose whichever one comes last (assuming the error wasn't fatal)
              bstRule = headRule
              break
            }
            else {
              // keep searching other rules of the same precedence and priority
            }
          }
          else { // this rule has higher rank than the matching rule already found
            if (headRule.isAlwaysMatches || headRule.getPattern.getItemType.asInstanceOf[NodeTest].matches(Type.ELEMENT, nodeName, null)) bstRule = headRule
          }
        }
        else if (headRule.isAlwaysMatches || headRule.getPattern.getItemType.asInstanceOf[NodeTest].matches(Type.ELEMENT, nodeName, null)) {
          bstRule = headRule
          break
        }
        headRule = headRule.getNext
      }
    }
    bestRule
  }

  /**
   * Get all the rules in rank order, highest-ranking rules first
   *
   * @return the rules in rank order
   */
  def getRankedRules = {
    val treeMap = new util.TreeMap[Integer, Rule]
    var rule = anyElementRule
    while ( {
      rule != null
    }) {
      treeMap.put(-rule.getRank, rule)
      rule = rule.getNext
    }
    rule = unnamedElementRuleChain
    while ( {
      rule != null
    }) {
      treeMap.put(-rule.getRank, rule)
      rule = rule.getNext
    }

    for (r <- namedElementRules.values.asScala) {
      treeMap.put(-r.getRank, r)
    }
    treeMap.values.iterator
  }

  /**
   * Make a filter to implement these space-stripping rules, or null if no filtering
   * is necessary
   *
   * @return a filter in the form of a ProxyReceiver, or null
   * @param next
   */
  override def makeStripper(next: Receiver) = new Stripper(this, next)

  /**
   * Export this rule as part of an exported stylesheet
   *
   * @param presenter the output handler
   */
  @throws[XPathException]
  override def `export`(presenter: ExpressionPresenter) = {
    presenter.startElement("strip")
    var rule = anyElementRule
    while ( {
      rule != null
    }) {
      SelectedElementsSpaceStrippingRule.exportRule(rule, presenter)
      rule = rule.getNext
    }
    rule = unnamedElementRuleChain
    while ( {
      rule != null
    }) {
      SelectedElementsSpaceStrippingRule.exportRule(rule, presenter)
      rule = rule.getNext
    }

    for (r <- namedElementRules.values.asScala) {
      SelectedElementsSpaceStrippingRule.exportRule(r, presenter)
    }
    presenter.endElement
  }
}