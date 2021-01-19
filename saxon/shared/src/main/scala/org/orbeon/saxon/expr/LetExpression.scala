package org.orbeon.saxon.expr

import org.orbeon.saxon.event.Outputter
import org.orbeon.saxon.expr.instruct.{DocumentInstr, TailCall, TailCallReturner}
import org.orbeon.saxon.expr.parser._
import org.orbeon.saxon.model.{ItemType, SchemaType, UType}
import org.orbeon.saxon.om._
import org.orbeon.saxon.trace.ExpressionPresenter

//import scala.collection.compat._
import java.util.ArrayList
import java.util.function.BiConsumer

import org.orbeon.saxon.value.{IntegerValue, SequenceType}

import scala.beans.BeanProperty
import scala.jdk.CollectionConverters._
import scala.util.control.Breaks._


class LetExpression extends Assignation with TailCallReturner {

  @BeanProperty
  var evaluator: Evaluator = null
  private var needsEagerEvaluation: Boolean = false
  var needsLazyEvaluation: Boolean = false
  var isInstruct: Boolean = false

  def setInstruction(inst: Boolean): Unit =
    isInstruct = inst

  override def getExpressionName: String = "let"

  def setNeedsEagerEvaluation(req: Boolean): Unit = {
    if (req && needsLazyEvaluation) {
      // No action - see bug #2903, Lazy evaluation wins
    }
    this.needsEagerEvaluation = req
  }

  def setNeedsLazyEvaluation(req: Boolean): Unit = {
    if (req && needsEagerEvaluation)
      this.needsEagerEvaluation = false
    this.needsLazyEvaluation = req
  }

  override def isLiftable(forStreaming: Boolean): Boolean =
    super.isLiftable(forStreaming) && !needsEagerEvaluation

  override def resetLocalStaticProperties(): Unit = {
    super.resetLocalStaticProperties()
    references = new ArrayList[VariableReference]
    if ((evaluator eq Evaluator.VARIABLE) && ! getSequence.isInstanceOf[VariableReference]) {
      evaluator = null
      setEvaluator()
    }
  }

  override def typeCheck(visitor: ExpressionVisitor,
                         contextInfo: ContextItemStaticInfo): Expression = {
    getSequenceOp.typeCheck(visitor, contextInfo)
    val role: RoleDiagnostic = new RoleDiagnostic(
      RoleDiagnostic.VARIABLE,
      getVariableQName.getDisplayName,
      0)
    this.setSequence(TypeChecker.strictTypeCheck(getSequence,
      requiredType,
      role,
      visitor.getStaticContext))
    val actualItemType: ItemType = getSequence.getItemType
    refineTypeInformation(
      actualItemType,
      getSequence.getCardinality,
      getSequence match {
        case literal: Literal => literal.getValue
        case _ => null
      },
      getSequence.getSpecialProperties,
      this
    )
    getActionOp.typeCheck(visitor, contextInfo)
    this
  }

  override def implementsStaticTypeCheck(): Boolean = true

  override def staticTypeCheck(req: SequenceType,
                               backwardsCompatible: Boolean,
                               role: RoleDiagnostic,
                               visitor: ExpressionVisitor): Expression = {
    val tc = visitor.getConfiguration.getTypeChecker(backwardsCompatible)
    this.setAction(tc.staticTypeCheck(getAction, req, role, visitor))
    this
  }

  override def optimize(visitor: ExpressionVisitor,
                        contextItemType: ContextItemStaticInfo): Expression = {
    val opt = visitor.obtainOptimizer()
    getAction match {
      case variableRef: VariableReference if ! ExpressionTool.changesXsltContext(getSequence) && (variableRef.getBinding eq this) =>
        getSequenceOp.optimize(visitor, contextItemType)
        opt.trace("Eliminated trivial variable " + getVariableName, getSequence)
        return getSequence
      case _ =>
    }
    if (getSequence.isInstanceOf[Literal] && opt.isOptionSet(OptimizerOptions.INLINE_VARIABLES)) {
      opt.trace("Inlined constant variable " + getVariableName, getSequence)
      replaceVariable(getSequence)
      return getAction.optimize(visitor, contextItemType)
    }
    getSequence match {
      case instr: DocumentInstr if instr.isTextOnly =>
        verifyReferences()
        if (allReferencesAreFlattened()) {
          var stringValueExpression = instr.getStringValueExpression
          stringValueExpression = stringValueExpression.typeCheck(visitor, contextItemType)
          this.setSequence(stringValueExpression)
          requiredType = SequenceType.SINGLE_UNTYPED_ATOMIC
          adoptChildExpression(getSequence)
          refineTypeInformation(requiredType.getPrimaryType,
            requiredType.getCardinality,
            null,
            0,
            this)
        }
      case _ =>
    }
    if (getSequence.hasSpecialProperty(StaticProperty.HAS_SIDE_EFFECTS))
      needsEagerEvaluation = true
    hasLoopingReference |= removeDeadReferences
    if (!needsEagerEvaluation) {
      var considerRemoval =
        ((references != null && references.size < 2) || getSequence.isInstanceOf[VariableReference]) &&
        !isIndexedVariable &&
        !hasLoopingReference &&
        !needsEagerEvaluation
      if (considerRemoval) {
        verifyReferences()
        considerRemoval = references != null
      }
      if (considerRemoval && references.isEmpty) {
        getActionOp.optimize(visitor, contextItemType)
        opt.trace("Eliminated unused variable " + getVariableName, getAction)
        return getAction
      }
      if (considerRemoval && references.size == 1 && ExpressionTool.dependsOnFocus(getSequence)) {
        if (visitor.isOptimizeForStreaming)
          considerRemoval = false
        var child: Expression = references.get(0)
        var parent = child.getParentExpression
        breakable {
          while (parent != null && (parent ne this)) {
            val operand = ExpressionTool.findOperand(parent, child)
            assert(operand != null)
            if (! operand.hasSameFocus) {
              considerRemoval = false
              break()
            }
            child = parent
            parent = child.getParentExpression
          }
        }
      }
      if (considerRemoval && references.size == 1) {
        if (ExpressionTool.changesXsltContext(getSequence))
          considerRemoval = false
        else if ((getSequence.getDependencies & StaticProperty.DEPENDS_ON_CURRENT_GROUP) != 0)
          considerRemoval = false
        else if (references.get(0).isInLoop)
          considerRemoval = false
      }
      if (considerRemoval &&
        (references.size == 1 || getSequence.isInstanceOf[Literal] ||
          getSequence.isInstanceOf[VariableReference]) &&
        opt.isOptionSet(OptimizerOptions.INLINE_VARIABLES)) {
        inlineReferences()
        opt.trace("Inlined references to $" + getVariableName, getAction)
        references = null
        return getAction.optimize(visitor, contextItemType)
      }
    }
    var tries: Int = 0
    breakable {
      while ( {
        tries += 1
        tries - 1
      } < 5) {
        val seq0: Expression = getSequence
        getSequenceOp.optimize(visitor, contextItemType)
        if (getSequence.isInstanceOf[Literal] && !isIndexedVariable) {
          return optimize(visitor, contextItemType)
        }
        if (seq0 eq getSequence) {
          break()
        }
      }
    }
    tries = 0
    breakable {
      while ( {
        tries += 1
        tries - 1
      } < 5) {
        val act0 = getAction
        getActionOp.optimize(visitor, contextItemType)
        if (act0 eq getAction)
          break()
        if (! isIndexedVariable && ! needsEagerEvaluation) {
          verifyReferences()
          if (references != null && references.size < 2) {
            if (references.isEmpty) {
              hasLoopingReference = false
              return optimize(visitor, contextItemType)
            } else {
              if (!references.get(0).isInLoop) {
                return optimize(visitor, contextItemType)
              }
            }
          }
        }
      }
    }
    setEvaluator()
    this
  }

  def setEvaluator(): Unit =
    if (needsEagerEvaluation)
      this.evaluator = ExpressionTool.eagerEvaluator(getSequence)
    else if (isIndexedVariable)
      this.evaluator = Evaluator.MAKE_INDEXED_VARIABLE
    else if (evaluator == null)
      this.evaluator = ExpressionTool.lazyEvaluator(getSequence, getNominalReferenceCount > 1)

  private def inlineReferences(): Unit =
    for (ref <- references.asScala) {
      val parent = ref.getParentExpression
      if (parent != null) {
        val o = ExpressionTool.findOperand(parent, ref)
        if (o != null)
          o.setChildExpression(getSequence.copy(new RebindingMap()))
        ExpressionTool.resetStaticProperties(parent)
      }
    }

  override def getCost: Double = getSequence.getCost + getAction.getCost

  private def allReferencesAreFlattened(): Boolean =
    references != null && references.asScala.forall(_.isFlattened)

  override def isVacuousExpression: Boolean = getAction.isVacuousExpression

  override def checkPermittedContents(parentType: SchemaType, whole: Boolean): Unit =
    getAction.checkPermittedContents(parentType, whole)

  override def getIntegerBounds: Array[IntegerValue] =
    getAction.getIntegerBounds

  override def getImplementationMethod: Int =
    getAction.getImplementationMethod

  override def gatherProperties(consumer: BiConsumer[String, Any]): Unit =
    consumer.accept("name", getVariableQName)

  override def iterate(context: XPathContext): SequenceIterator = {
    var let = this
    breakable {
      while (true) {
        val `val` = let.eval(context)
        context.setLocalVariable(let.getLocalSlotNumber, `val`)
        let.getAction match {
          case letExpr: LetExpression =>
            let = letExpr
          case _ =>
            break()
        }
      }
    }
    let.getAction.iterate(context)
  }

  def eval(context: XPathContext): Sequence = {

    if (evaluator == null)
      evaluator = ExpressionTool.lazyEvaluator(getSequence, getNominalReferenceCount > 1)

    try {
      val savedOutputState = context.getTemporaryOutputState
      context.setTemporaryOutputState(StandardNames.XSL_VARIABLE)
      val result = evaluator.evaluate(getSequence, context)
      context.setTemporaryOutputState(savedOutputState)
      result
    } catch {
      case _: ClassCastException =>
        // Probably the evaluation mode is wrong, as a result of an expression rewrite. Try again.
        assert(false) // ORBEON: not sure why this is there then.
        val savedOutputState = context.getTemporaryOutputState
        context.setTemporaryOutputState(StandardNames.XSL_VARIABLE)
        val result = Evaluator.EAGER_SEQUENCE.evaluate(getSequence, context)
        context.setTemporaryOutputState(savedOutputState)
        result
    }
  }

  override def evaluateItem(context: XPathContext): Item = {
    var let = this
    breakable {
      while (true) {
        val `val` = let.eval(context)
        context.setLocalVariable(let.getLocalSlotNumber, `val`)
        let.getAction match {
          case letExpr: LetExpression =>
            let = letExpr
          case _ =>
            break()
        }
      }
    }
    let.getAction.evaluateItem(context)
  }

  override def effectiveBooleanValue(context: XPathContext): Boolean = {
    var let = this
    breakable {
      while (true) {
        val `val` = let.eval(context)
        context.setLocalVariable(let.getLocalSlotNumber, `val`)
        let.getAction match {
          case letExpr: LetExpression =>
            let = letExpr
          case _ =>
            break()
        }
      }
    }
    let.getAction.effectiveBooleanValue(context)
  }

  override def process(output: Outputter, context: XPathContext): Unit = {
    var let = this
    breakable {
      while (true) {
        val `val` = let.eval(context)
        context.setLocalVariable(let.getLocalSlotNumber, `val`)
        let.getAction match {
          case letExpr: LetExpression =>
            let = letExpr
          case _ =>
            break()
        }
      }
    }
    let.getAction.process(output, context)
  }

  def getItemType: ItemType = getAction.getItemType

  override def getStaticUType(contextItemType: UType): UType =
    if (isInstruction)
      UType.ANY
    else
      getAction.getStaticUType(contextItemType)

  def computeCardinality(): Int = getAction.getCardinality

  override def computeSpecialProperties(): Int = {
    var props = getAction.getSpecialProperties
    val seqProps = getSequence.getSpecialProperties
    if ((seqProps & StaticProperty.NO_NODES_NEWLY_CREATED) == 0)
      props &= ~StaticProperty.NO_NODES_NEWLY_CREATED
    props
  }

  override def markTailFunctionCalls(qName: StructuredQName, arity: Int): Int =
    ExpressionTool.markTailFunctionCalls(getAction, qName, arity)

  def copy(rebindings: RebindingMap): Expression = {
    val let: LetExpression = new LetExpression()
    rebindings.put(this, let)
    let.isIndexedVariable = isIndexedVariable
    let.hasLoopingReference = hasLoopingReference
    let.setNeedsEagerEvaluation(needsEagerEvaluation)
    let.setNeedsLazyEvaluation(needsLazyEvaluation)
    let.setVariableQName(variableName)
    let.setRequiredType(requiredType)
    let.setSequence(getSequence.copy(rebindings))
    let.setInstruction(isInstruction)
    ExpressionTool.copyLocationInfo(this, let)
    val newAction: Expression = getAction.copy(rebindings)
    let.setAction(newAction)
    let
  }

  def processLeavingTail(output: Outputter, context: XPathContext): TailCall = {
    var let: LetExpression = this
    breakable {
      while (true) {
        val `val`: Sequence = let.eval(context)
        context.setLocalVariable(let.getLocalSlotNumber, `val`)
        let.getAction match {
          case letExpr: LetExpression =>
            let = letExpr
          case _ =>
            break()
        }
      }
    }
    let.getAction match {
      case tc: TailCallReturner =>
        tc.processLeavingTail(output, context)
      case _ =>
        let.getAction.process(output, context)
        null
    }
  }

  override def evaluatePendingUpdates(context: XPathContext,
                                      pul: PendingUpdateList): Unit = {
    var let = this
    breakable {
      while (true) {
        val `val`: Sequence = let.eval(context)
        context.setLocalVariable(let.getLocalSlotNumber, `val`)
        let.getAction match {
          case letExpr: LetExpression =>
            let = letExpr
          case _ =>
            break()
        }
      }
    }
    let.getAction.evaluatePendingUpdates(context, pul)
  }

  override def toString: String =
    "let $" + getVariableEQName + " := " + getSequence + " return " +
      ExpressionTool.parenthesize(getAction)

  override def toShortString: String = "let $" + getVariableName + " := ..."

  def export(out: ExpressionPresenter): Unit = {
    out.startElement("let", this)
    out.emitAttribute("var", variableName)
    if (getRequiredType != SequenceType.ANY_SEQUENCE)
      out.emitAttribute("as", getRequiredType.toAlphaCode)
    if (isIndexedVariable)
      out.emitAttribute("indexable", "true")
    out.emitAttribute("slot", getLocalSlotNumber.toString)
    if (evaluator == null)
      this.evaluator = ExpressionTool.lazyEvaluator(getSequence, getNominalReferenceCount > 1)
    out.emitAttribute("eval", getEvaluator.getEvaluationMode.getCode.toString)
    getSequence.export(out)
    getAction.export(out)
    out.endElement()
  }

  def setEvaluationMode(mode: EvaluationMode.EvaluationMode): Unit =
    this.evaluator = mode.getEvaluator
}
