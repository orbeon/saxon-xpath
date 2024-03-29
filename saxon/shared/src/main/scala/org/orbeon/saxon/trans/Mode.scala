package org.orbeon.saxon.trans

import java.util.{Collections, Set}

import org.orbeon.saxon.event.Outputter
import org.orbeon.saxon.expr.{XPathContext, XPathContextMajor}
import org.orbeon.saxon.expr.accum.Accumulator
import org.orbeon.saxon.expr.instruct.{Actor, ParameterSet, TailCall, TemplateRule}
import org.orbeon.saxon.lib.{NamespaceConstant, TraceListener}
import org.orbeon.saxon.model.{BuiltInAtomicType, SchemaType, Type, Untyped}
import org.orbeon.saxon.om._
import org.orbeon.saxon.s9api.Location
import org.orbeon.saxon.trace.{ExpressionPresenter, ModeTraceListener}
import org.orbeon.saxon.trans.Mode._
import org.orbeon.saxon.trans.rules._
import org.orbeon.saxon.tree.iter.LookaheadIterator
import org.orbeon.saxon.utils.Controller
import org.orbeon.saxon.value.SequenceType

import scala.beans.BeanProperty
import scala.util.control.Breaks._

object Mode {

  val OMNI_MODE: StructuredQName =
    new StructuredQName("saxon", NamespaceConstant.SAXON, "_omniMode")

  val UNNAMED_MODE_NAME: StructuredQName =
    new StructuredQName("xsl", NamespaceConstant.XSLT, "unnamed")

  val DEFAULT_MODE_NAME: StructuredQName =
    new StructuredQName("xsl", NamespaceConstant.XSLT, "default")

  val RECOVER_WITH_WARNINGS: Int = 1

  trait RuleFilter {
    def testRule(r: Rule): Boolean
  }

  trait RuleAction {
    def processRule(r: Rule): Unit
  }
}

abstract class Mode(var modeName: StructuredQName) extends Actor {

  private var streamable: Boolean = _
  @BeanProperty
  val recoveryPolicy: RecoveryPolicy.RecoveryPolicy = RecoveryPolicy.RECOVER_WITH_WARNINGS
  var mustBeTyped: Boolean = false
  var mustBeUntyped: Boolean = false
  var hasRules: Boolean = false
  var bindingSlotsAllocated: Boolean = false
  var modeTracing: Boolean = false
  var defaultResultType: SequenceType = null
  private var accumulators: Set[_ <: Accumulator] = _

  def getBuiltInRuleSet: BuiltInRuleSet
  def isUnnamedMode: Boolean = modeName == UNNAMED_MODE_NAME
  def getModeName: StructuredQName = modeName
  def getActivePart: SimpleMode
  def getMaxPrecedence: Int
  def getMaxRank: Int
  def computeRankings(start: Int): Unit

  def getModeTitle: String =
    if (isUnnamedMode)
      "The unnamed mode"
    else
      "Mode " + getModeName.getDisplayName

  def setModeTracing(tracing: Boolean): Unit =
    this.modeTracing = tracing

  def isModeTracing: Boolean = modeTracing

  def getAccumulators: Set[_ <: Accumulator] =
    if (accumulators == null)
      Collections.emptySet()
    else
      accumulators

  def setAccumulators(accumulators: Set[_ <: Accumulator]): Unit =
    this.accumulators = accumulators

  override def getSymbolicName: SymbolicName =
    new SymbolicName(StandardNames.XSL_MODE, getModeName)

  def getObjectName: StructuredQName = getModeName

  def isEmpty: Boolean

  def setHasRules(hasRules: Boolean): Unit =
    this.hasRules = hasRules

  def setStreamable(streamable: Boolean): Unit = {
    this.streamable = streamable
  }

  def isDeclaredStreamable: Boolean = streamable

  def getExplicitNamespaces(pool: NamePool): Set[String]

  def setDefaultResultType(`type`: SequenceType): Unit =
    defaultResultType = `type`

  def getDefaultResultType: SequenceType = defaultResultType

  def processRules(action: RuleAction): Unit

  def makeNewContext(context: XPathContext): XPathContext = {
    val c2: XPathContextMajor = context.newContext()
    c2.setOrigin(context.getController)
    c2.openStackFrame(getStackFrameSlotsNeeded)
    if (! context.getCurrentComponent.getActor.isInstanceOf[Accumulator]) {
      c2.setCurrentComponent(context.getCurrentMode)
    }
    c2
  }

  def getRule(item: Item, context: XPathContext): Rule

  def getRule(item: Item,
              context: XPathContext,
              filter: Mode.RuleFilter): Rule

  def getRule(item: Item, min: Int, max: Int, context: XPathContext): Rule = {
    val filter: RuleFilter = r => {
      val p: Int = r.getPrecedence
      p >= min && p <= max
    }
    getRule(item, context, filter)
  }

  def getNextMatchRule(item: Item,
                       currentRule: Rule,
                       context: XPathContext): Rule = {
    val filter: Mode.RuleFilter = r => {
      val comp: Int = r.compareRank(currentRule)
      if (comp < 0) {
         true
      } else if (comp == 0) {
        val seqComp = java.lang.Integer.compare(r.getSequence, currentRule.getSequence)
        if (seqComp < 0) {
           true
        } else if (seqComp == 0) {
          r.getPartNumber < currentRule.getPartNumber
        } else {
          false
        }
      } else {
        false
      }
    }
    getRule(item, context, filter)
  }

  def exportTemplateRules(out: ExpressionPresenter): Unit

  def explainTemplateRules(out: ExpressionPresenter): Unit

  def applyTemplates(parameters: ParameterSet,
                     tunnelParameters: ParameterSet,
                     output: Outputter,
                     context: XPathContextMajor,
                     locationId: Location): TailCall = {
    val controller: Controller = context.getController
    val tracing: Boolean = modeTracing || controller.isTracing
    val iterator: SequenceIterator = context.getCurrentIterator
    var tc: TailCall = null
    var traceListener: TraceListener = null
    if (tracing) {
      traceListener = controller.getTraceListener
      if (traceListener == null) {
        traceListener = new ModeTraceListener()
        controller.setTraceListener(traceListener)
        traceListener.open(controller)
      }
    }
    val lookahead: Boolean =
      iterator.getProperties.contains(SequenceIterator.Property.LOOKAHEAD)
    var previousTemplate: TemplateRule = null
    breakable {
      while (true) {
        if (tc != null) {
          if (lookahead && !iterator.asInstanceOf[LookaheadIterator].hasNext) {
            break()
          }
          do tc = tc.processLeavingTail() while (tc != null)
        }
        val item: Item = iterator.next()
        if (item == null) {
          break()
        }
        if (mustBeTyped) {
          item match {
            case nodeInfo: NodeInfo =>
              val kind: Int = nodeInfo.getNodeKind
              if (kind == Type.ELEMENT || kind == Type.ATTRIBUTE) {
                val annotation: SchemaType =
                  nodeInfo.getSchemaType
                if (annotation == Untyped.getInstance || annotation == BuiltInAtomicType.UNTYPED_ATOMIC) {
                  throw new XPathException(
                    getModeTitle + " requires typed nodes, but the input is untyped",
                    "XTTE3100")
                }
              }
            case _ =>
          }
        } else if (mustBeUntyped) {
          item match {
            case nodeInfo: NodeInfo =>
              val kind: Int = nodeInfo.getNodeKind
              if (kind == Type.ELEMENT || kind == Type.ATTRIBUTE) {
                val annotation: SchemaType =
                  nodeInfo.getSchemaType
                if (!(annotation == Untyped.getInstance || annotation == BuiltInAtomicType.UNTYPED_ATOMIC)) {
                  throw new XPathException(
                    getModeTitle + " requires untyped nodes, but the input is typed",
                    "XTTE3110")
                }
              }
            case _ =>
          }
        }
        if (tracing) {
          traceListener.startRuleSearch()
        }
        val rule: Rule = getRule(item, context)
        if (tracing) {
          traceListener.endRuleSearch(
            if (rule != null) rule else getBuiltInRuleSet,
            this,
            item)
        }
        if (rule == null) {
          getBuiltInRuleSet.process(item,
            parameters,
            tunnelParameters,
            output,
            context,
            locationId)
        } else {
          val template: TemplateRule = rule.getAction.asInstanceOf[TemplateRule]
          if (template != previousTemplate) {
            previousTemplate = template
            template.initialize()
            context.openStackFrame(template.getStackFrameMap)
            context.setLocalParameters(parameters)
            context.setTunnelParameters(tunnelParameters)
            context.setCurrentMergeGroupIterator(null)
          }
          context.setCurrentTemplateRule(rule)
          if (tracing) {
            traceListener.startCurrentItem(item)
            if (modeTracing) {
              traceListener.enter(template, Collections.emptyMap(), context)
            }
            tc = template.applyLeavingTail(output, context)
            if (tc != null) {
              do tc = tc.processLeavingTail() while (tc != null)
            }
            if (modeTracing) {
              traceListener.leave(template)
            }
            traceListener.endCurrentItem(item)
          } else {
            tc = template.applyLeavingTail(output, context)
          }
        }
      }
    }
    tc
  }

  def getStackFrameSlotsNeeded: Int

  def getCodeForBuiltInRuleSet(builtInRuleSet: BuiltInRuleSet): String =
    builtInRuleSet match {
      case _: ShallowCopyRuleSet =>
        "SC"
      case _: ShallowSkipRuleSet =>
        "SS"
      case _: DeepCopyRuleSet =>
        "DC"
      case _: DeepSkipRuleSet =>
        "DS"
      case _: FailRuleSet =>
        "FF"
      case _: TextOnlyCopyRuleSet =>
        "TC"
      case warnings: RuleSetWithWarnings =>
        getCodeForBuiltInRuleSet(warnings.getBaseRuleSet) + "+W"
      case _ =>
        "???"
    }

  def getBuiltInRuleSetForCode(code: String): BuiltInRuleSet = {
    var base: BuiltInRuleSet = null
    if (code.startsWith("SC")) {
      base = ShallowCopyRuleSet.getInstance
    } else if (code.startsWith("SS")) {
      base = ShallowSkipRuleSet.getInstance
    } else if (code.startsWith("DC")) {
      base = DeepCopyRuleSet.getInstance
    } else if (code.startsWith("DS")) {
      base = DeepSkipRuleSet.getInstance
    } else if (code.startsWith("FF")) {
      base = FailRuleSet.getInstance
    } else if (code.startsWith("TC")) {
      base = TextOnlyCopyRuleSet.getInstance
    } else {
      throw new IllegalArgumentException(code)
    }
    if (code.endsWith("+W")) {
      base = new RuleSetWithWarnings(base)
    }
    base
  }

  def export(presenter: ExpressionPresenter): Unit = {
    val s: Int = presenter.startElement("mode")
    if (!isUnnamedMode) {
      presenter.emitAttribute("name", getModeName)
    }
    presenter.emitAttribute("onNo",
      getCodeForBuiltInRuleSet(getBuiltInRuleSet))
    var flags: String = ""
    if (isDeclaredStreamable) {
      flags += "s"
    }
    if (isUnnamedMode) {
      flags += "d"
    }
    if (mustBeTyped) {
      flags += "t"
    }
    if (mustBeUntyped) {
      flags += "u"
    }
    if (recoveryPolicy == RecoveryPolicy.DO_NOT_RECOVER) {
      flags += "F"
    } else if (recoveryPolicy == RecoveryPolicy.RECOVER_WITH_WARNINGS) {
      flags += "W"
    }
    if (!hasRules) {
      flags += "e"
    }
    if (!flags.isEmpty) {
      presenter.emitAttribute("flags", flags)
    }
    exportUseAccumulators(presenter)
    presenter.emitAttribute("patternSlots", getStackFrameSlotsNeeded.toString)
    exportTemplateRules(presenter)
    val e: Int = presenter.endElement()
    if (s != e) {
      throw new IllegalStateException(
        "Export tree unbalanced for mode " + getModeName)
    }
  }

  def exportUseAccumulators(presenter: ExpressionPresenter): Unit = ()

  def isMustBeTyped: Boolean = mustBeTyped

  def explain(presenter: ExpressionPresenter): Unit = {
    val s: Int = presenter.startElement("mode")
    if (!isUnnamedMode) {
      presenter.emitAttribute("name", getModeName)
    }
    presenter.emitAttribute("onNo",
      getCodeForBuiltInRuleSet(getBuiltInRuleSet))
    var flags: String = ""
    if (isDeclaredStreamable) {
      flags += "s"
    }
    if (isUnnamedMode) {
      flags += "d"
    }
    if (mustBeTyped) {
      flags += "t"
    }
    if (mustBeUntyped) {
      flags += "u"
    }
    if (recoveryPolicy == RecoveryPolicy.DO_NOT_RECOVER) {
      flags += "F"
    } else if (recoveryPolicy == RecoveryPolicy.RECOVER_WITH_WARNINGS) {
      flags += "W"
    }
    if (!flags.isEmpty) {
      presenter.emitAttribute("flags", flags)
    }
    presenter.emitAttribute("patternSlots", getStackFrameSlotsNeeded.toString)
    explainTemplateRules(presenter)
    val e: Int = presenter.endElement()
    if (s != e) {
      throw new IllegalStateException("tree unbalanced")
    }
  }
}
