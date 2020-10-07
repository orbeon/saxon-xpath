////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// Copyright (c) 2018-2020 Saxonica Limited
// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is "Incompatible With Secondary Licenses", as defined by the Mozilla Public License, v. 2.0.
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
package org.orbeon.saxon.trans

import org.orbeon.saxon.expr.{ComponentBinding, XPathContext, XPathContextMajor}
import org.orbeon.saxon.expr.instruct.{Actor, SlotManager, TemplateRule}
import org.orbeon.saxon.model.{ErrorType, Type, UType}
import org.orbeon.saxon.om._
import org.orbeon.saxon.pattern._
import org.orbeon.saxon.trans.Mode.RuleAction
import org.orbeon.saxon.trans.RecoveryPolicy._

import scala.util.control.Breaks._

/*import org.orbeon.saxon.style.StylesheetModule*/
// StylesheetModule not exist
import java.util._

import org.orbeon.saxon.style.StylesheetPackage
import org.orbeon.saxon.trace.ExpressionPresenter
import org.orbeon.saxon.trans.rules._
import org.orbeon.saxon.tree.util.Navigator
import org.orbeon.saxon.value.{AtomicValue, Whitespace}
import org.orbeon.saxon.z.IntHashMap

//import scala.collection.compat._
import scala.jdk.CollectionConverters._

/**
 * A Mode is a collection of rules; the selection of a rule to apply to a given element
 * is determined by a Pattern. A SimpleMode is a mode contained within a single package,
 * as opposed to a CompoundMode which can combine rules from several packages
 */
object SimpleMode {

  def copyRules(from: SimpleMode, to: SimpleMode) = {
    try from.processRules((r: Rule) => {
      def foo(r: Rule) = {
        val r2 = r.copy(false)
        to.addRule(r2.getPattern, r2)
      }

      foo(r)
    })
    catch {
      case e: XPathException =>
        throw new AssertionError(e)
    }
    to.mostRecentRule = from.mostRecentRule
    to.mostRecentModuleHash = from.mostRecentModuleHash
  }

  private def indexByQName(pool: NamePool, indexByFP: IntHashMap[RuleChain], indexByQN: Map[StructuredQName, RuleChain]) = {
    val ii = indexByFP.keyIterator
    while ( {
      ii.hasNext
    }) {
      val fp = ii.next
      val eChain = indexByFP.get(fp)
      val name = pool.getStructuredQName(fp)
      indexByQN.put(name, eChain)
    }
  }

  private def showPattern(p: Pattern) = { // Complex patterns can be laid out with lots of whitespace, which looks messy in the error message
    Whitespace.collapseWhitespace(p.toString).toString
  }

  def forceAllocateAllBindingSlots(pack: StylesheetPackage, mode: SimpleMode, bindings: List[ComponentBinding]): Unit = {
    val rulesProcessed = new HashSet[TemplateRule]
    val patternsProcessed = new IdentityHashMap[Pattern, Boolean]
    try mode.processRules((r: Rule) => {
      def foo(r: Rule) = { // A rule can appear twice, for example at different import precedences or
        // because the match pattern is a union pattern; only allocate slots once
        val pattern = r.getPattern
        if (!patternsProcessed.containsKey(pattern)) {
          Actor.allocateBindingSlotsRecursive(pack, mode, pattern, bindings)
          patternsProcessed.put(pattern, true)
        }
        val tr = r.getAction.asInstanceOf[TemplateRule]
        if (tr.getBody != null && !rulesProcessed.contains(tr)) {
          Actor.allocateBindingSlotsRecursive(pack, mode, tr.getBody, bindings)
          rulesProcessed.add(tr)
        }
      }

      foo(r)
    })
    catch {
      case e: XPathException =>
        throw new AssertionError(e)
    }
  }

  private class MaxPrecedenceAction extends Mode.RuleAction {
    var max = 0

    override def processRule(r: Rule) = if (r.getPrecedence > max) max = r.getPrecedence
  }

  /**
   * Supporting class used at compile time to sort all the rules into precedence/priority
   * order and allocate a rank to each one, so that at run-time, comparing two rules to see
   * which has higher precedence/priority is a simple integer subtraction.
   */
  private class RuleSorter(var start: Int) {
    var rules = new ArrayList[Rule](100)

    def addRule(rule: Rule): Boolean = rules.add(rule)

    def allocateRanks(): Unit = {
      rules.asScala.sortBy((rule: Rule) => rule.compareComputedRank(rule))
      //GenericSorter.quickSort(0, rules.size(), this);
      var rank = start
      for (i <- 0 until rules.size) {
        if (i > 0 && rules.get(i - 1).compareComputedRank(rules.get(i)) != 0) rank += 1
        rules.get(i).setRank(rank)
      }
    }

    def getNumberOfRules: Int = rules.size
  }

  /**
   * Interface used around a group of rules - principally at the
   * group start and the group end
   */
  trait RuleGroupAction {
    /**
     * Set some string parameter for the group
     *
     * @param s a string value to be used for the group
     */
    def setString(s: String): Unit

    /**
     * Start of a generic group
     */
    def start(): Unit

    /**
     * Start of a group characterised by some integer parameter
     *
     * @param i characteristic of this group
     */
    def start(i: Int): Unit

    /**
     * Invoked at the end of a group
     */
    def end(): Unit
  }

}

class SimpleMode(val structModeName: StructuredQName) extends Mode(structModeName) {

  val genericRuleChain: RuleChain = new RuleChain()

  var atomicValueRuleChain: RuleChain = new RuleChain()

  var functionItemRuleChain: RuleChain = new RuleChain()

  var documentRuleChain: RuleChain = new RuleChain()

  var textRuleChain: RuleChain = new RuleChain()

  var commentRuleChain: RuleChain = new RuleChain()

  var processingInstructionRuleChain: RuleChain = new RuleChain()

  var namespaceRuleChain: RuleChain = new RuleChain()

  var unnamedElementRuleChain: RuleChain = new RuleChain()

  var unnamedAttributeRuleChain: RuleChain = new RuleChain()

  var namedElementRuleChains: IntHashMap[RuleChain] = new IntHashMap(
    32)

  var namedAttributeRuleChains: IntHashMap[RuleChain] =
    new IntHashMap(8)

  var qNamedElementRuleChains: Map[StructuredQName, RuleChain] = _

  var qNamedAttributeRuleChains: Map[StructuredQName, RuleChain] = _

  private var builtInRuleSet: BuiltInRuleSet = TextOnlyCopyRuleSet.getInstance

  private var mostRecentRule: Rule = _

  private var mostRecentModuleHash: Int = _

  private var stackFrameSlotsNeeded: Int = 0

  private var highestRank: Int = _

  private var explicitPropertyPrecedences: Map[String, Integer] = new HashMap()

  private var explicitPropertyValues: Map[String, String] = new HashMap()

  /**
   * Set the built-in template rules to be used with this Mode in the case where there is no
   * explicit template rule
   *
   * @param defaultRules the built-in rule set
   */
  def setBuiltInRuleSet(defaultRules: BuiltInRuleSet) = {
    this.builtInRuleSet = defaultRules
    hasRules = true // if mode is explicitly declared, treat it as containing rules
  }

  /**
   * Get the built-in template rules to be used with this Mode in the case where there is no
   * explicit template rule
   *
   * @return the built-in rule set, defaulting to the TextOnlyCopyRuleSet if no other rule set has
   *         been supplied
   */
  override def getBuiltInRuleSet = this.builtInRuleSet

  /**
   * Get the active component of this mode. For a simple mode this is the mode itself;
   * for a compound mode it is the "overriding" part
   */
  override def getActivePart = this

  /**
   * Check that the mode does not contain conflicting property values
   *
   * @throws XPathException if there are conflicts
   */
  @throws[XPathException]
  def checkForConflictingProperties(): Unit = {

    for (entry <- getActivePart.explicitPropertyValues.entrySet.asScala) {
      val prop = entry.getKey
      val value = entry.getValue
      if (value == "##conflict##") throw new XPathException("For " + getLabel + ", there are conflicting values for xsl:mode/@" + prop + " at the same import precedence", "XTSE0545")
      if (prop == "typed") {
        mustBeTyped = "yes" == value || "strict" == value || "lax" == value
        mustBeUntyped = "no" == value
      }
      else if (prop == "on-no-match") {
        var base: BuiltInRuleSet = null
        value match {
          case "text-only-copy" =>
            base = TextOnlyCopyRuleSet.getInstance
          case "shallow-copy" =>
            base = ShallowCopyRuleSet.getInstance
          case "deep-copy" =>
            base = DeepCopyRuleSet.getInstance
          case "shallow-skip" =>
            base = ShallowSkipRuleSet.getInstance
          case "deep-skip" =>
            base = DeepSkipRuleSet.getInstance
          case "fail" =>
            base = FailRuleSet.getInstance
          case _ =>
          // already validated
        }
        if ("yes" == explicitPropertyValues.get("warning-on-no-match")) base = new RuleSetWithWarnings(base)
        setBuiltInRuleSet(base)
      }
    }
  }

  /**
   * Get an identifier for the mode for use in error messages
   *
   * @return either the string "the unnamed mode" or the string "mode NNNN" where "NNNN"
   *         is the display form of the mode's name.
   */
  def getLabel: String = if (isUnnamedMode) "the unnamed mode"
  else "mode " + structModeName.getDisplayName

  /**
   * Generate a search state for processing a given node
   *
   * @return a new object capable of holding the state of a search for a rule
   */
  def makeRuleSearchState(chain: RuleChain, context: XPathContext) = new RuleSearchState

  /**
   * Ask whether there are any template rules in this mode
   * (a mode could exist merely because it is referenced in apply-templates)
   *
   * @return true if no template rules exist in this mode
   */
  override def isEmpty = !hasRules

  /**
   * Set an explicit property at a particular precedence. Used for detecting conflicts
   *
   * @param name       the name of the property
   * @param value      the value of the property
   * @param precedence the import precedence of this property value
   */
  def setExplicitProperty(name: String, value: String, precedence: Int) = {
    val p = explicitPropertyPrecedences.get(name)
    if (p != null) if (p < precedence) {
      explicitPropertyPrecedences.put(name, precedence)
      explicitPropertyValues.put(name, value)
    }
    else if (p.equals(precedence)) {
      val v = explicitPropertyValues.get(name)
      if (v != null & !(v == value)) { // We don't throw an exception, because the conflict is an error only if this
        // is the highest-precedence declaration of this mode
        explicitPropertyValues.put(name, "##conflict##")
      }
    }
    else {
      // no action
    }
    else {
      explicitPropertyPrecedences.put(name, precedence)
      explicitPropertyValues.put(name, value)
    }
    val typed = explicitPropertyValues.get("typed")
    mustBeTyped = "yes" == typed || "strict" == typed || "lax" == typed
    mustBeUntyped = "no" == typed
  }

  /**
   * Get the value of a property of this mode, e.g. the "typed" property
   *
   * @param name the property name, e.g. "typed"
   * @return the property value
   */
  def getPropertyValue(name: String) = explicitPropertyValues.get(name)

  /**
   * Get the "explicit namespaces" matched by this mode. Returns a set containing all the namespaces
   * matched by specific template rules in this mode
   *
   * @param pool the NamePool for the configuration
   * @return the set of all namespace URIs of names explicitly matched by rules in this mode
   */
  override def getExplicitNamespaces(pool: NamePool) = {
    val namespaces = new HashSet[String]
    val ii = namedElementRuleChains.keyIterator
    while ( {
      ii.hasNext
    }) {
      val fp = ii.next
      namespaces.add(pool.getURI(fp))
    }
    namespaces
  }

  def addRule(pattern: Pattern, action: RuleTarget /*, module: StylesheetModule*/ , precedence: Int, priority: Double, position: Int, part: Int): Unit = {
    hasRules = true
    // Ignore a pattern that will never match, e.g. "@comment"
    if (pattern.getItemType eq ErrorType)
      return
    // for fast lookup, we maintain one list for each element name for patterns that can only
    // match elements of a given name, one list for each node type for patterns that can only
    // match one kind of non-element node, and one generic list.
    // Each list is sorted in precedence/priority order so we find the highest-priority rule first
    // This logic is designed to ensure that when a UnionPattern contains multiple branches
    // with the same priority, next-match doesn't select the same template twice (next-match-024)
    /*val moduleHash = module.hashCode*/
    //        int sequence;
    //        if (mostRecentRule == null) {
    //            sequence = 0;
    //        } else if (action == mostRecentRule.getAction() && moduleHash == mostRecentModuleHash) {
    //            sequence = mostRecentRule.getSequence();
    //        } else {
    //            sequence = mostRecentRule.getSequence() + 1;
    //        }
    //int precedence = module.getPrecedence();
    // val minImportPrecedence = module.getMinImportPrecedence
    val newRule = makeRule(pattern, action, precedence, 0, priority, position, part)
    if (pattern.isInstanceOf[NodeTestPattern]) {
      val test = pattern.getItemType
      test match {
        case _: AnyNodeTest => newRule.setAlwaysMatches(true)
        case _: NodeKindTest => newRule.setAlwaysMatches(true)
        case _: NameTest =>
          val kind = test.getPrimitiveType
          if (kind == Type.ELEMENT || kind == Type.ATTRIBUTE)
            newRule.setAlwaysMatches(true)
        case _ =>
      }
    }
    mostRecentRule = newRule
    // mostRecentModuleHash = moduleHash
    addRule(pattern, newRule)
  }

  /**
   * Generate a new rule - so it can be overridden to make more specialist rules
   *
   * @param pattern             the pattern that this rule matches
   * @param action              the object invoked by this rule (usually a Template)
   * @param precedence          the precedence of the rule
   * @param minImportPrecedence the minimum import precedence for xsl:apply-imports
   * @param priority            the priority of the rule
   * @param sequence            a sequence number for ordering of rules
   * @param part                distinguishes rules formed by splitting a rule on a union pattern
   * @return the newly created rule
   */
  def makeRule(/*@NotNull*/ pattern: Pattern, action: RuleTarget, precedence: Int, minImportPrecedence: Int, priority: Double, sequence: Int, part: Int) = new Rule(pattern, action, precedence, minImportPrecedence, priority, sequence, part)

  def addRule(pattern: Pattern, newRule: Rule) = {
    val uType = pattern.getUType
    if (uType == UType.ELEMENT) {
      val fp = pattern.getFingerprint
      addRuleToNamedOrUnnamedChain(newRule, fp, unnamedElementRuleChain, namedElementRuleChains)
    }
    else if (uType == UType.ATTRIBUTE) {
      val fp = pattern.getFingerprint
      addRuleToNamedOrUnnamedChain(newRule, fp, unnamedAttributeRuleChain, namedAttributeRuleChains)
    }
    else if (uType == UType.DOCUMENT) addRuleToList(newRule, documentRuleChain)
    else if (uType == UType.TEXT) addRuleToList(newRule, textRuleChain)
    else if (uType == UType.COMMENT) addRuleToList(newRule, commentRuleChain)
    else if (uType == UType.PI) addRuleToList(newRule, processingInstructionRuleChain)
    else if (uType == UType.NAMESPACE) addRuleToList(newRule, namespaceRuleChain)
    else if (UType.ANY_ATOMIC.subsumes(uType)) addRuleToList(newRule, atomicValueRuleChain)
    else if (UType.FUNCTION.subsumes(uType)) addRuleToList(newRule, functionItemRuleChain)
    else addRuleToList(newRule, genericRuleChain)
  }

  def addRuleToNamedOrUnnamedChain(newRule: Rule, fp: Int, unnamedRuleChain: RuleChain, namedRuleChains: IntHashMap[RuleChain]) = if (fp == -1) addRuleToList(newRule, unnamedRuleChain)
  else {
    var chain = namedRuleChains.get(fp)
    if (chain == null) {
      chain = new RuleChain(newRule)
      namedRuleChains.put(fp, chain)
    }
    else addRuleToList(newRule, chain)
  }

  /**
   * Insert a new rule into this list before others of the same precedence/priority
   *
   * @param newRule the new rule to be added into the list
   * @param list    the Rule at the head of the list, or null if the list is empty
   */
  private def addRuleToList(newRule: Rule, list: RuleChain) = if (list.head == null) list.setHead(newRule)
  else {
    val precedence = newRule.getPrecedence
    val priority = newRule.getPriority
    var rule = list.head
    var prev: Rule = null
    breakable {
      while (rule != null) if ((rule.getPrecedence < precedence) || (rule.getPrecedence == precedence && rule.getPriority <= priority)) {
        newRule.setNext(rule)
        if (prev == null) list.setHead(newRule)
        else prev.setNext(newRule)
        break()
      }
      else {
        prev = rule
        rule = rule.getNext
      }
    }
    if (rule == null) {
      assert(prev != null)
      prev.setNext(newRule)
      newRule.setNext(null)
    }
  }

  /**
   * Specify how many slots for local variables are required by a particular pattern
   *
   * @param slots the number of slots needed
   */
  def allocatePatternSlots(slots: Int) = stackFrameSlotsNeeded = Math.max(stackFrameSlotsNeeded, slots)

  /**
   * Get the rule corresponding to a given item, by finding the best pattern match.
   *
   * @param item    the item to be matched
   * @param context the XPath dynamic evaluation context
   * @return the best matching rule, if any (otherwise null).
   * @throws XPathException if an error occurs matching a pattern
   */
  @throws[XPathException]
  override def getRule(item: Item, context: XPathContext): Rule = {
    var XPathContext = context
    if (stackFrameSlotsNeeded > 0) XPathContext = makeNewContext(XPathContext)
    // search the specific list for this node type / node name
    var unnamedNodeChain: RuleChain = null
    var bestRule: Rule = null
    if (item.isInstanceOf[NodeInfo]) {
      val node = item.asInstanceOf[NodeInfo]
      node.getNodeKind match {
        case Type.DOCUMENT =>
          unnamedNodeChain = documentRuleChain
        case Type.ELEMENT =>
          unnamedNodeChain = unnamedElementRuleChain
          var namedNodeChain: RuleChain = null
          if (node.hasFingerprint) namedNodeChain = namedElementRuleChains.get(node.getFingerprint)
          else namedNodeChain = getNamedRuleChain(XPathContext, Type.ELEMENT, node.getURI, node.getLocalPart)
          if (namedNodeChain != null) bestRule = searchRuleChain(node, XPathContext, null, namedNodeChain)
        case Type.ATTRIBUTE =>
          unnamedNodeChain = unnamedAttributeRuleChain
          var namedNodeChain: RuleChain = null
          if (node.hasFingerprint) namedNodeChain = namedAttributeRuleChains.get(node.getFingerprint)
          else namedNodeChain = getNamedRuleChain(XPathContext, Type.ATTRIBUTE, node.getURI, node.getLocalPart)
          if (namedNodeChain != null) bestRule = searchRuleChain(node, XPathContext, null, namedNodeChain)
        case Type.TEXT =>
          unnamedNodeChain = textRuleChain
        case Type.COMMENT =>
          unnamedNodeChain = commentRuleChain
        case Type.PROCESSING_INSTRUCTION =>
          unnamedNodeChain = processingInstructionRuleChain
        case Type.NAMESPACE =>
          unnamedNodeChain = namespaceRuleChain
        case _ =>
          throw new AssertionError("Unknown node kind")
      }
      // search the list for unnamed nodes of a particular kind
      if (unnamedNodeChain != null) bestRule = searchRuleChain(node, XPathContext, bestRule, unnamedNodeChain)
      // Search the list for rules for nodes of unknown node kind
      bestRule = searchRuleChain(node, XPathContext, bestRule, genericRuleChain)
    }
    else if (item.isInstanceOf[AtomicValue]) {
      if (atomicValueRuleChain != null) bestRule = searchRuleChain(item, XPathContext, bestRule, atomicValueRuleChain)
      bestRule = searchRuleChain(item, XPathContext, bestRule, genericRuleChain)
    }
    else if (item.isInstanceOf[Function]) {
      if (functionItemRuleChain != null) bestRule = searchRuleChain(item, XPathContext, bestRule, functionItemRuleChain)
      bestRule = searchRuleChain(item, XPathContext, bestRule, genericRuleChain)
    }
    bestRule
  }

  /**
   * Get a rule chain for a particular node name without allocating a fingerprint from the name pool
   *
   * @param kind  the kind of node (element or attribute)
   * @param uri   the namespace URI of the node
   * @param local the local name of the node
   * @return the Rule at the head of the rule chain for nodes of this name, or null if there are no rules
   *         to consider
   */
  def getNamedRuleChain(c: XPathContext, kind: Int, uri: String, local: String) = {
    this synchronized {
      if (qNamedElementRuleChains == null) {
        qNamedElementRuleChains = new HashMap[StructuredQName, RuleChain](namedElementRuleChains.size)
        qNamedAttributeRuleChains = new HashMap[StructuredQName, RuleChain](namedAttributeRuleChains.size)
        val pool = c.getNamePool
        SimpleMode.indexByQName(pool, namedElementRuleChains, qNamedElementRuleChains)
        SimpleMode.indexByQName(pool, namedAttributeRuleChains, qNamedAttributeRuleChains)
      }
    }
    (if (kind == Type.ELEMENT) qNamedElementRuleChains
    else qNamedAttributeRuleChains).get(new StructuredQName("", uri, local))

  }

  /**
   * Search a chain of rules
   *
   * @param item     the item being matched
   * @param context  XPath dynamic context
   * @param bestRule the best rule so far in terms of precedence and priority (may be null)
   * @param chain    the chain to be searched
   * @return the best match rule found in the chain, or the previous best rule, or null
   * @throws XPathException if an error occurs matching a pattern
   */
  @throws[XPathException]
  def searchRuleChain(item: Item, context: XPathContext, bestRule: Rule, chain: RuleChain): Rule = {
    var XPathContext = context
    var bstRule = bestRule
    while (!XPathContext.isInstanceOf[XPathContextMajor])
      XPathContext = XPathContext.getCaller
    val ruleSearchState = makeRuleSearchState(chain, XPathContext)
    var head = if (chain == null) null
    else chain.head
    breakable {
      while (head != null) {
        if (bstRule != null) {
          val rank = head.compareRank(bstRule)
          if (rank < 0) {
            break()
          }
          else if (rank == 0) { // this rule has the same precedence and priority as the matching rule already found
            if (ruleMatches(head, item, XPathContext.asInstanceOf[XPathContextMajor], ruleSearchState)) {
              if (head.getSequence != bstRule.getSequence) reportAmbiguity(item, bstRule, head, XPathContext)
              // choose whichever one comes last (assuming the error wasn't fatal)
              val seqComp = Integer.compare(bstRule.getSequence, head.getSequence)
              if (seqComp > 0) return bstRule
              else if (seqComp < 0) return head
              else { // we're dealing with two rules formed by partitioning a union pattern
                bstRule = if (bstRule.getPartNumber > head.getPartNumber) bstRule
                else head
              }
              break()
            }
            else {
            }
          }
          else {
            if (ruleMatches(head, item, XPathContext.asInstanceOf[XPathContextMajor], ruleSearchState)) bstRule = head
          }
        }
        else if (ruleMatches(head, item, XPathContext.asInstanceOf[XPathContextMajor], ruleSearchState)) {
          bstRule = head
          if (getRecoveryPolicy eq RecoveryPolicy.RECOVER_SILENTLY) { //ruleSearchState.count();
            break()
          }
        }
        head = head.getNext
      }
    }
    bstRule
  }

  /**
   * Does this rule match the given item? Can be overridden
   *
   * @param r       the rule to check
   * @param item    the context item
   * @param context the static context for evaluation
   * @param pre     An appropriate matcher for preconditions in this mode
   * @return true if this rule does match
   * @throws XPathException if a dynamic error occurs while matching the pattern
   */
  @throws[XPathException]
  def ruleMatches(r: Rule, item: Item, context: XPathContextMajor, pre: RuleSearchState) = r.isAlwaysMatches || r.matches(item, context)

  /**
   * Get the rule corresponding to a given item, by finding the best Pattern match.
   *
   * @param item    the item to be matched
   * @param context the XPath dynamic evaluation context
   * @param filter  a filter to select which rules should be considered
   * @return the best matching rule, if any (otherwise null).
   * @throws XPathException if an error occurs
   */
  @throws[XPathException]
  override def getRule(item: Item, context: XPathContext, filter: Mode.RuleFilter): Rule = {
    var xPathCont = context
    if (stackFrameSlotsNeeded > 0) xPathCont = makeNewContext(xPathCont)
    // Get the rule search state object
    var ruleSearchState: RuleSearchState = null
    var bestRule: Rule = null
    var unnamedNodeChain: RuleChain = null
    // Search the list for unnamed nodes of a particular kind
    if (item.isInstanceOf[NodeInfo]) {
      val node = item.asInstanceOf[NodeInfo]
      node.getNodeKind match {
        case Type.DOCUMENT =>
          unnamedNodeChain = documentRuleChain
        case Type.ELEMENT =>
          unnamedNodeChain = unnamedElementRuleChain
          var namedNodeChain: RuleChain = null
          if (node.hasFingerprint) namedNodeChain = namedElementRuleChains.get(node.getFingerprint)
          else namedNodeChain = getNamedRuleChain(xPathCont, Type.ELEMENT, node.getURI, node.getLocalPart)
          if (namedNodeChain != null) {
            ruleSearchState = makeRuleSearchState(namedNodeChain, xPathCont)
            bestRule = searchRuleChain(item, xPathCont, null, namedNodeChain, ruleSearchState, filter)
          }
        case Type.ATTRIBUTE =>
          unnamedNodeChain = unnamedAttributeRuleChain
          var namedNodeChain: RuleChain = null
          if (node.hasFingerprint) namedNodeChain = namedAttributeRuleChains.get(node.getFingerprint)
          else namedNodeChain = getNamedRuleChain(xPathCont, Type.ATTRIBUTE, node.getURI, node.getLocalPart)
          if (namedNodeChain != null) {
            ruleSearchState = makeRuleSearchState(namedNodeChain, xPathCont)
            bestRule = searchRuleChain(item, xPathCont, null, namedNodeChain, ruleSearchState, filter)
          }
        case Type.TEXT =>
          unnamedNodeChain = textRuleChain
        case Type.COMMENT =>
          unnamedNodeChain = commentRuleChain
        case Type.PROCESSING_INSTRUCTION =>
          unnamedNodeChain = processingInstructionRuleChain
        case Type.NAMESPACE =>
          unnamedNodeChain = namespaceRuleChain
        case _ =>
          throw new AssertionError("Unknown node kind")
      }
      ruleSearchState = makeRuleSearchState(unnamedNodeChain, xPathCont)
      bestRule = searchRuleChain(item, xPathCont, bestRule, unnamedNodeChain, ruleSearchState, filter)
      ruleSearchState = makeRuleSearchState(genericRuleChain, xPathCont)
      searchRuleChain(item, xPathCont, bestRule, genericRuleChain, ruleSearchState, filter)
    }
    else if (item.isInstanceOf[AtomicValue]) {
      if (atomicValueRuleChain != null) {
        ruleSearchState = makeRuleSearchState(atomicValueRuleChain, xPathCont)
        bestRule = searchRuleChain(item, xPathCont, bestRule, atomicValueRuleChain, ruleSearchState, filter)
      }
      ruleSearchState = makeRuleSearchState(genericRuleChain, xPathCont)
      bestRule = searchRuleChain(item, xPathCont, bestRule, genericRuleChain, ruleSearchState, filter)
      bestRule
    }
    else if (item.isInstanceOf[Function]) {
      if (functionItemRuleChain != null) {
        ruleSearchState = makeRuleSearchState(functionItemRuleChain, xPathCont)
        bestRule = searchRuleChain(item, xPathCont, bestRule, functionItemRuleChain, ruleSearchState, filter)
      }
      ruleSearchState = makeRuleSearchState(genericRuleChain, xPathCont)
      bestRule = searchRuleChain(item, xPathCont, bestRule, genericRuleChain, ruleSearchState, filter)
      bestRule
    }
    else null
  }

  /**
   * Search a chain of rules
   *
   * @param item            the item being matched
   * @param context         XPath dynamic context
   * @param bestRule        the best rule so far in terms of precedence and priority (may be null)
   * @param chain           the chain to be searched
   * @param ruleSearchState An appropriate ruleState in this mode
   * @param filter          filter used to select which rules are candidates to be searched
   * @return the best match rule found in the chain, or the previous best rule, or null
   * @throws XPathException if an error occurs while matching a pattern
   */
  @throws[XPathException]
  def searchRuleChain(item: Item, context: XPathContext, bestRule: Rule, chain: RuleChain, ruleSearchState: RuleSearchState, filter: Mode.RuleFilter): Rule = {
    var head = if (chain == null) null else chain.head
    var xPathContext = context
    var bstRule = bestRule

    while (! xPathContext.isInstanceOf[XPathContextMajor])
      xPathContext = xPathContext.getCaller

    breakable {
      while (head != null) {
        if (filter == null || filter.testRule(head)) if (bstRule != null) {
          val rank = head.compareRank(bstRule)
          if (rank < 0) {
            break()
          } else if (rank == 0) {
            if (ruleMatches(head, item, xPathContext.asInstanceOf[XPathContextMajor], ruleSearchState)) {
              reportAmbiguity(item, bstRule, head, xPathContext)
              bstRule =
                if (bstRule.getSequence > head.getSequence)
                  bstRule
                else
                  head
              break()
            } else {
              // keep searching other rules of the same precedence and priority
            }
          } else {
            // this rule has higher rank than the matching rule already found
            if (ruleMatches(head, item, xPathContext.asInstanceOf[XPathContextMajor], ruleSearchState))
              bstRule = head
          }
        } else if (ruleMatches(head, item, xPathContext.asInstanceOf[XPathContextMajor], ruleSearchState)) {
          bstRule = head
          if (getRecoveryPolicy eq RecoveryPolicy.RECOVER_SILENTLY)
            break()
        }
      }
      head = head.getNext
    }
    bstRule
  }

  /**
   * Report an ambiguity, that is, the situation where two rules of the same
   * precedence and priority match the same node
   *
   * @param item The item that matches two or more rules
   * @param r1   The first rule that the node matches
   * @param r2   The second rule that the node matches
   * @param c    The context for the transformation
   * @throws XPathException if the system is configured to treat ambiguous template matching as a
   *                        non-recoverable error
   */
  @throws[XPathException]
  def reportAmbiguity(item: Item, r1: Rule, r2: Rule, c: XPathContext): Unit = {
    if (getRecoveryPolicy eq RecoveryPolicy.RECOVER_SILENTLY) return

    if ((r1.getAction eq r2.getAction) && r1.getSequence == r2.getSequence) return
    var path: String = null
    val errorCode = "XTDE0540"
    if (item.isInstanceOf[NodeInfo]) path = Navigator.getPath(item.asInstanceOf[NodeInfo])
    else path = item.toShortString
    val pat1 = r1.getPattern
    val pat2 = r2.getPattern
    var message: String = null
    if (r1.getAction eq r2.getAction) message = "Ambiguous rule match for " + path + ". " +
      "Matches \"" + SimpleMode.showPattern(pat1) + "\" on line " + pat1.getLocation.getLineNumber +
      " of " + pat1.getLocation.getSystemId + ", a rule which appears in the stylesheet more than once, because the containing module was included more than once"
    else message = "Ambiguous rule match for " + path + '\n' + "Matches both \"" + SimpleMode.showPattern(pat1) + "\" on line " + pat1.getLocation.getLineNumber + " of " + pat1.getLocation.getSystemId + "\nand \"" + SimpleMode.showPattern(pat2) + "\" on line " + pat2.getLocation.getLineNumber + " of " + pat2.getLocation.getSystemId
    getRecoveryPolicy match {
      case DO_NOT_RECOVER =>
        throw new XPathException(message, errorCode, getLocation)
      case RECOVER_WITH_WARNINGS =>
        c.getController.warning(message, errorCode, getLocation)
      case RECOVER_SILENTLY =>
      case _ =>
    }
  }

  /**
   * Prepare for possible streamability - null here, but can be subclassed
   *
   * @throws XPathException if a failure occurs
   */
  @throws[XPathException]
  def prepareStreamability() = {
  }

  /**
   * Allocate slot numbers to all the external component references in this component
   *
   * @param pack the containing package
   */
  override def allocateAllBindingSlots(pack: StylesheetPackage) = if ((getDeclaringComponent.getDeclaringPackage eq pack) && !bindingSlotsAllocated) {
    SimpleMode.forceAllocateAllBindingSlots(pack, this, getDeclaringComponent.getComponentBindings)
    bindingSlotsAllocated = true
  }

  /**
   * Compute the streamability of all template rules. No action in Saxon-HE.
   */
  @throws[XPathException]
  def computeStreamability() = {
    // Implemented in Saxon-EE
  }

  /**
   * For a streamable mode, invert all the templates to generate streamable code.
   * No action in Saxon-HE.
   *
   * @throws XPathException if there is a non-streamable template in the mode
   */
  @throws[XPathException]
  def invertStreamableTemplates() = {
  }

  /**
   * Explain all template rules in this mode by showing their
   * expression tree represented in XML. Note that this produces more information
   * than the simpler exportTemplateRules() method: this method is intended for
   * the human reader wanting diagnostic explanations, whereas exportTemplateRules()
   * is designed to produce a package that can be re-imported.
   *
   * @param out used to display the expression tree
   */
  @throws[XPathException]
  override def explainTemplateRules(out: ExpressionPresenter) = {
    val action: RuleAction = (r: Rule) => r.`export`(out, isDeclaredStreamable)
    val group = new SimpleMode.RuleGroupAction() {
      private[trans] var `type`: String = null

      override

      def start() = {
        out.startElement("ruleSet")
        out.emitAttribute("type", `type`)
      }

      override

      def setString(`type`: String) = this.`type` = `type`

      override

      def start(i: Int) = {
        out.startElement("ruleChain")
        out.emitAttribute("key", out.getNamePool.getClarkName(i))
      }

      override

      def end() = out.endElement
    }
    try processRules(action, group)
    catch {
      case err: XPathException =>

      // can't happen, and doesn't matter if it does
    }
  }

  /**
   * Export all template rules in this mode in a form that can be re-imported.
   * Note that template rules with union patterns may have been split into multiple
   * rules. We need to avoid outputting them more than once.
   *
   * @param out used to display the expression tree
   */
  @throws[XPathException]
  override def exportTemplateRules(out: ExpressionPresenter): Unit = {
    // TODO: if two rules share the same template, avoid duplicate output. This can happen with union patterns, and also
    // when a template is present in more than one mode.
    val action: RuleAction = (r: Rule) => {
      def foo(r: Rule): Unit = { // if (processedRules.add(r.getAction())) {
        r.`export`(out, isDeclaredStreamable)
        // }
      }

      foo(r)
    }
    processRules(action)
  }

  /**
   * Walk over all the rules, applying a specified action to each one.
   *
   * @param action an action that is to be applied to all the rules in this Mode
   * @throws XPathException if an error occurs processing any of the rules
   */
  @throws[XPathException]
  override def processRules(action: Mode.RuleAction): Unit = processRules(action, null)

  /**
   * Walk over all the rules, applying a specified action to each one.
   *
   * @param action an action that is to be applied to all the rules in this Mode
   * @param group  - actions to be performed at group start and group end
   * @throws XPathException if an error occurs processing any of the rules
   */
  @throws[XPathException]
  def processRules(action: Mode.RuleAction, group: SimpleMode.RuleGroupAction): Unit = {
    processRuleChain(documentRuleChain, action, setGroup(group, "document-node()"))
    processRuleChain(unnamedElementRuleChain, action, setGroup(group, "element()"))
    processRuleChains(namedElementRuleChains, action, setGroup(group, "namedElements"))
    processRuleChain(unnamedAttributeRuleChain, action, setGroup(group, "attribute()"))
    processRuleChains(namedAttributeRuleChains, action, setGroup(group, "namedAttributes"))
    processRuleChain(textRuleChain, action, setGroup(group, "text()"))
    processRuleChain(commentRuleChain, action, setGroup(group, "comment()"))
    processRuleChain(processingInstructionRuleChain, action, setGroup(group, "processing-instruction()"))
    processRuleChain(namespaceRuleChain, action, setGroup(group, "namespace()"))
    processRuleChain(genericRuleChain, action, setGroup(group, "node()"))
    processRuleChain(atomicValueRuleChain, action, setGroup(group, "atomicValue"))
    processRuleChain(functionItemRuleChain, action, setGroup(group, "function()"))
  }

  /**
   * Set the string associated with a rule group
   *
   * @param group the group action object
   * @param type  the type of the rule group
   * @return modified rulegroup action
   */
  def setGroup(group: SimpleMode.RuleGroupAction, `type`: String) = {
    if (group != null) group.setString(`type`)
    group
  }

  @throws[XPathException]
  def processRuleChains(chains: IntHashMap[RuleChain], action: Mode.RuleAction, group: SimpleMode.RuleGroupAction) = if (chains.size > 0) {
    if (group != null) group.start()
    val ii = chains.keyIterator
    while ( {
      ii.hasNext
    }) {
      val i = ii.next
      if (group != null) group.start(i)
      val r = chains.get(i)
      processRuleChain(r, action, null)
      if (group != null) group.end()
    }
    if (group != null) group.end()
  }

  @throws[XPathException]
  def processRuleChain(chain: RuleChain, action: Mode.RuleAction) = {
    var r = if (chain == null) null
    else chain.head
    while ( {
      r != null
    }) {
      action.processRule(r)
      r = r.getNext
    }
  }

  @throws[XPathException]
  def processRuleChain(chain: RuleChain, action: Mode.RuleAction, group: SimpleMode.RuleGroupAction) = {
    var r = if (chain == null) null
    else chain.head
    if (r != null) {
      if (group != null) group.start()
      while ( {
        r != null
      }) {
        action.processRule(r)
        r = r.getNext
      }
      if (group != null) group.end()
    }
  }

  /**
   * Perform optimization on the complete set of rules comprising this Mode. This is a null operation
   * in Saxon-HE.
   */
  def optimizeRules() = {
  }

  override def getMaxPrecedence = try {
    val action = new SimpleMode.MaxPrecedenceAction
    processRules(action)
    action.max
  } catch {
    case e: XPathException =>
      throw new AssertionError(e)
  }

  /**
   * Compute a rank for each rule, as a combination of the precedence and priority, to allow
   * rapid comparison.  This method also checks that there are no conflicts for
   * property values in different xsl:mode declarations
   *
   * @param start the lowest rank to use
   * @throws XPathException if an error occurs processing the rules
   */
  @throws[XPathException]
  override def computeRankings(start: Int) = { // Now sort the rules into ranking order
    val sorter = new SimpleMode.RuleSorter(start)
    // add all the rules in this Mode to the sorter
    processRules(sorter.addRule)
    // now allocate ranks to all the rules in this Mode
    sorter.allocateRanks()
    highestRank = start + sorter.getNumberOfRules
  }

  override def getMaxRank: Int = highestRank

  /**
   * Allocate slots for local variables in all patterns used by the rules in this mode.
   * Currently used only for accumulator rules
   */
  def allocateAllPatternSlots() = {
    val count = new ArrayList[Integer](1) // used to allow inner class to have side-effects
    count.add(0)
    val slotManager = new SlotManager // TODO: allocate this via the Configuration
    val slotAllocator: RuleAction = (r: Rule) => {
      def foo(r: Rule): Int = {
        val slots = r.getPattern.allocateSlots(slotManager, 0)
        val max = Math.max(count.get(0), slots)
        count.set(0, max)
      }

      foo(r)
    }
    try processRules(slotAllocator)
    catch {
      case e: XPathException =>
        throw new AssertionError(e)
    }
    stackFrameSlotsNeeded = count.get(0)
  }

  override def getStackFrameSlotsNeeded = stackFrameSlotsNeeded

  def setStackFrameSlotsNeeded(slots: Int) = this.stackFrameSlotsNeeded = slots
}