package net.sf.saxon.expr

import net.sf.saxon.event.Outputter

import net.sf.saxon.expr.instruct.DocumentInstr

import net.sf.saxon.expr.instruct.TailCall

import net.sf.saxon.expr.instruct.TailCallReturner

import net.sf.saxon.expr.parser._

import net.sf.saxon.model.ItemType

import net.sf.saxon.model.SchemaType

import net.sf.saxon.model.UType

import net.sf.saxon.om._

import net.sf.saxon.trace.ExpressionPresenter

import scala.jdk.CollectionConverters._

import net.sf.saxon.value.IntegerValue

import net.sf.saxon.value.SequenceType

import java.util.ArrayList

import java.util.function.BiConsumer

import scala.beans.{BeanProperty, BooleanBeanProperty}
import scala.util.control.Breaks._


class LetExpression extends Assignation with TailCallReturner {

  @BeanProperty
  var evaluator: Evaluator = null

  private var needsEagerEvaluation: Boolean = false

  var needsLazyEvaluation: Boolean = false

  var isInstruct: Boolean = false

  def setInstruction(inst: Boolean): Unit = {
    isInstruct = inst
  }

  override def getExpressionName(): String = "let"

  def setNeedsEagerEvaluation(req: Boolean): Unit = {
    if (req && needsLazyEvaluation) {}
    this.needsEagerEvaluation = req
  }

  def setNeedsLazyEvaluation(req: Boolean): Unit = {
    if (req && needsEagerEvaluation) {
      this.needsEagerEvaluation = false
    }
    this.needsLazyEvaluation = req
  }

  override def isLiftable(forStreaming: Boolean): Boolean =
    super.isLiftable(forStreaming) && !needsEagerEvaluation

  override def resetLocalStaticProperties(): Unit = {
    super.resetLocalStaticProperties()
    references = new ArrayList()
    if (evaluator == Evaluator.VARIABLE && !(getSequence
      .isInstanceOf[VariableReference])) {
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
      if (getSequence.isInstanceOf[Literal])
        getSequence.asInstanceOf[Literal].getValue
      else null,
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
    val tc: TypeChecker =
      visitor.getConfiguration.getTypeChecker(backwardsCompatible)
    this.setAction(tc.staticTypeCheck(getAction, req, role, visitor))
    this
  }

  override def optimize(visitor: ExpressionVisitor,
                        contextItemType: ContextItemStaticInfo): Expression = {
    val opt: Optimizer = visitor.obtainOptimizer()
    if (getAction.isInstanceOf[VariableReference] &&
      (getAction.asInstanceOf[VariableReference].getBinding eq this) &&
      !ExpressionTool.changesXsltContext(getSequence)) {
      getSequenceOp.optimize(visitor, contextItemType)
      opt.trace("Eliminated trivial variable " + getVariableName, getSequence)
      return getSequence
    }
    if (getSequence.isInstanceOf[Literal] && opt.isOptionSet(
      OptimizerOptions.INLINE_VARIABLES)) {
      opt.trace("Inlined constant variable " + getVariableName, getSequence)
      replaceVariable(getSequence)
      return getAction.optimize(visitor, contextItemType)
    }
    if (getSequence.isInstanceOf[DocumentInstr] && getSequence
      .asInstanceOf[DocumentInstr]
      .isTextOnly) {
      verifyReferences()
      if (allReferencesAreFlattened()) {
        var stringValueExpression: Expression =
          getSequence.asInstanceOf[DocumentInstr].getStringValueExpression
        stringValueExpression =
          stringValueExpression.typeCheck(visitor, contextItemType)
        this.setSequence(stringValueExpression)
        requiredType = SequenceType.SINGLE_UNTYPED_ATOMIC
        adoptChildExpression(getSequence)
        refineTypeInformation(requiredType.getPrimaryType,
          requiredType.getCardinality,
          null,
          0,
          this)
      }
    }
    if (getSequence.hasSpecialProperty(StaticProperty.HAS_SIDE_EFFECTS)) {
      needsEagerEvaluation = true
    }
    hasLoopingReference |= removeDeadReferences
    if (!needsEagerEvaluation) {
      var considerRemoval: Boolean = ((references != null && references.size < 2) || getSequence
        .isInstanceOf[VariableReference]) &&
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
        getAction
      }
      if (considerRemoval && references.size == 1 && ExpressionTool
        .dependsOnFocus(getSequence)) {
        if (visitor.isOptimizeForStreaming) {
          considerRemoval = false
        }
        var child: Expression = references.get(0)
        var parent: Expression = child.getParentExpression
        breakable {
          while (parent != null && parent != this) {
            val operand: Operand = ExpressionTool.findOperand(parent, child)
            assert(operand != null)
            if (!operand.hasSameFocus()) {
              considerRemoval = false
              break()
            }
            child = parent
            parent = child.getParentExpression
          }
        }
      }
      if (considerRemoval && references.size == 1) {
        if (ExpressionTool.changesXsltContext(getSequence)) {
          considerRemoval = false
        } else if ((getSequence.getDependencies & StaticProperty.DEPENDS_ON_CURRENT_GROUP) !=
          0) {
          considerRemoval = false
        } else if (references.get(0).isInLoop) {
          considerRemoval = false
        }
      }
      if (considerRemoval &&
        (references.size == 1 || getSequence.isInstanceOf[Literal] ||
          getSequence.isInstanceOf[VariableReference]) &&
        opt.isOptionSet(OptimizerOptions.INLINE_VARIABLES)) {
        inlineReferences()
        opt.trace("Inlined references to $" + getVariableName, getAction)
        references = null
        getAction.optimize(visitor, contextItemType)
      }
    }
    var tries: Int = 0
    breakable {
      while ( {
        tries += 1;
        tries - 1
      } < 5) {
        val seq0: Expression = getSequence
        getSequenceOp.optimize(visitor, contextItemType)
        if (getSequence.isInstanceOf[Literal] && !isIndexedVariable) {
          return optimize(visitor, contextItemType)
        }
        if (seq0 == getSequence) {
          break()
        }
      }
    }
    tries = 0
    breakable {
      while ( {
        tries += 1;
        tries - 1
      } < 5) {
        val act0: Expression = getAction
        getActionOp.optimize(visitor, contextItemType)
        if (act0 == getAction) {
          break()
        }
        if (!isIndexedVariable && !needsEagerEvaluation) {
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

  def setEvaluator(): Unit = {
    if (needsEagerEvaluation) {
      this.evaluator = ExpressionTool.eagerEvaluator(getSequence)
    } else if (isIndexedVariable) {
      this.evaluator = Evaluator.MAKE_INDEXED_VARIABLE
    } else if (evaluator == null) {
      this.evaluator =
        ExpressionTool.lazyEvaluator(getSequence, getNominalReferenceCount > 1)
    }
  }

  private def inlineReferences(): Unit = {
    for (ref <- references.asScala) {
      val parent: Expression = ref.getParentExpression
      if (parent != null) {
        val o: Operand = ExpressionTool.findOperand(parent, ref)
        if (o != null) {
          o.setChildExpression(getSequence.copy(new RebindingMap()))
        }
        ExpressionTool.resetStaticProperties(parent)
      }
    }
  }

  override def getCost(): Double = getSequence.getCost + getAction.getCost

  private def allReferencesAreFlattened(): Boolean =
    references != null &&
      references.stream().allMatch(res => res.isFlattened)

  override def isVacuousExpression(): Boolean = getAction.isVacuousExpression

  override def checkPermittedContents(parentType: SchemaType, whole: Boolean): Unit = {
    getAction.checkPermittedContents(parentType, whole)
  }

  override def getIntegerBounds(): Array[IntegerValue] =
    getAction.getIntegerBounds

  override def getImplementationMethod(): Int =
    getAction.getImplementationMethod

  override def gatherProperties(consumer: BiConsumer[String, Any]): Unit = {
    consumer.accept("name", getVariableQName)
  }

  override def iterate(context: XPathContext): SequenceIterator = {
    var let: LetExpression = this
    breakable {
      while (true) {
        val `val`: Sequence = let.eval(context)
        context.setLocalVariable(let.getLocalSlotNumber, `val`)
        if (let.getAction.isInstanceOf[LetExpression]) {
          let = let.getAction.asInstanceOf[LetExpression]
        } else {
          break()
        }
      }
    }
    let.getAction.iterate(context)
  }

  def eval(context: XPathContext): Sequence = {
    if (evaluator == null) {
      this.evaluator =
        ExpressionTool.lazyEvaluator(getSequence, getNominalReferenceCount > 1)
    }
    try {
      val savedOutputState: Int = context.getTemporaryOutputState
      context.setTemporaryOutputState(StandardNames.XSL_VARIABLE)
      val result: Sequence = evaluator.evaluate(getSequence, context)
      context.setTemporaryOutputState(savedOutputState)
      result
    } catch {
      case e: ClassCastException => {
        assert(false)
        val savedOutputState: Int = context.getTemporaryOutputState
        context.setTemporaryOutputState(StandardNames.XSL_VARIABLE)
        val result: Sequence =
          Evaluator.EAGER_SEQUENCE.evaluate(getSequence, context)
        context.setTemporaryOutputState(savedOutputState)
        result
      }

    }
  }

  override def evaluateItem(context: XPathContext): Item = {
    var let: LetExpression = this
    breakable {
      while (true) {
        val `val`: Sequence = let.eval(context)
        context.setLocalVariable(let.getLocalSlotNumber, `val`)
        if (let.getAction.isInstanceOf[LetExpression]) {
          let = let.getAction.asInstanceOf[LetExpression]
        } else {
          break()
        }
      }
    }
    let.getAction.evaluateItem(context)
  }

  override def effectiveBooleanValue(context: XPathContext): Boolean = {
    var let: LetExpression = this
    breakable {
      while (true) {
        val `val`: Sequence = let.eval(context)
        context.setLocalVariable(let.getLocalSlotNumber, `val`)
        if (let.getAction.isInstanceOf[LetExpression]) {
          let = let.getAction.asInstanceOf[LetExpression]
        } else {
          break()
        }
      }
    }
    let.getAction.effectiveBooleanValue(context)
  }

  override def process(output: Outputter, context: XPathContext): Unit = {
    var let: LetExpression = this
    breakable {
      while (true) {
        val `val`: Sequence = let.eval(context)
        context.setLocalVariable(let.getLocalSlotNumber, `val`)
        if (let.getAction.isInstanceOf[LetExpression]) {
          let = let.getAction.asInstanceOf[LetExpression]
        } else {
          break()
        }
      }
    }
    let.getAction.process(output, context)
  }

  def getItemType(): ItemType = getAction.getItemType

  override def getStaticUType(contextItemType: UType): UType = {
    if (isInstruction) {
      UType.ANY
    }
    getAction.getStaticUType(contextItemType)
  }

  def computeCardinality(): Int = getAction.getCardinality

  override def computeSpecialProperties(): Int = {
    var props: Int = getAction.getSpecialProperties
    val seqProps: Int = getSequence.getSpecialProperties
    if ((seqProps & StaticProperty.NO_NODES_NEWLY_CREATED) == 0) {
      props &= ~StaticProperty.NO_NODES_NEWLY_CREATED
    }
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
        if (let.getAction.isInstanceOf[LetExpression]) {
          let = let.getAction.asInstanceOf[LetExpression]
        } else {
          break()
        }
      }
    }
    if (let.getAction.isInstanceOf[TailCallReturner]) {
      let.getAction
        .asInstanceOf[TailCallReturner]
        .processLeavingTail(output, context)
    } else {
      let.getAction.process(output, context)
      null
    }
  }

  override def evaluatePendingUpdates(context: XPathContext,
                                      pul: PendingUpdateList): Unit = {
    var let: LetExpression = this
    breakable {
      while (true) {
        val `val`: Sequence = let.eval(context)
        context.setLocalVariable(let.getLocalSlotNumber, `val`)
        if (let.getAction.isInstanceOf[LetExpression]) {
          let = let.getAction.asInstanceOf[LetExpression]
        } else {
          break()
        }
      }
    }
    let.getAction.evaluatePendingUpdates(context, pul)
  }

  override def toString(): String =
    "let $" + getVariableEQName + " := " + getSequence + " return " +
      ExpressionTool.parenthesize(getAction)

  override def toShortString(): String = "let $" + getVariableName + " := ..."

  def export(out: ExpressionPresenter): Unit = {
    out.startElement("let", this)
    out.emitAttribute("var", variableName)
    if (getRequiredType != SequenceType.ANY_SEQUENCE) {
      out.emitAttribute("as", getRequiredType.toAlphaCode)
    }
    if (isIndexedVariable) {
      out.emitAttribute("indexable", "true")
    }
    out.emitAttribute("slot", getLocalSlotNumber.toString)
    if (evaluator == null) {
      this.evaluator =
        ExpressionTool.lazyEvaluator(getSequence, getNominalReferenceCount > 1)
    }
    out.emitAttribute("eval", getEvaluator.getEvaluationMode.getCode.toString)
    getSequence.export(out)
    getAction.export(out)
    out.endElement()
  }

  def setEvaluationMode(mode: EvaluationMode.EvaluationMode): Unit = {
    this.evaluator = mode.getEvaluator
  }

}
