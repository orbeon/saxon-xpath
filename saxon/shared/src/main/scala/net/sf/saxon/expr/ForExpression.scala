package net.sf.saxon.expr

import net.sf.saxon.utils.Configuration

import net.sf.saxon.event.Outputter

import net.sf.saxon.expr.flwor.OuterForExpression

import net.sf.saxon.expr.instruct.Choose

import net.sf.saxon.expr.parser._

import net.sf.saxon.lib.Feature

import net.sf.saxon.model._

import net.sf.saxon.om.Item

import net.sf.saxon.om.SequenceIterator

import net.sf.saxon.om.StructuredQName

import net.sf.saxon.trace.ExpressionPresenter

import net.sf.saxon.trans.XPathException

import net.sf.saxon.value.Cardinality

import net.sf.saxon.value.IntegerValue

import net.sf.saxon.value.SequenceType

import java.util.ArrayList

import java.util.List

import ForExpression._

import Expression._

object ForExpression {

  class MappingAction(var context: XPathContext,
                      private var slotNumber: Int,
                      private var action: Expression)
    extends MappingFunction
      with ItemMappingFunction {

    def map(item: Item): SequenceIterator = {
      context.setLocalVariable(slotNumber, item)
      action.iterate(context)
    }

    def mapItem(item: Item): Item = {
      context.setLocalVariable(slotNumber, item)
      action.evaluateItem(context)
    }

  }

}

class ForExpression extends Assignation {

  private var actionCardinality: Int = StaticProperty.ALLOWS_MANY

  override def getExpressionName: String = "for"

  override def typeCheck(visitor: ExpressionVisitor, contextInfo: ContextItemStaticInfo): Expression = {
    getSequenceOp.typeCheck(visitor, contextInfo)
    if (Literal.isEmptySequence(getSequence) && !(this.isInstanceOf[OuterForExpression])) {
      getSequence
    }
    if (requiredType != null) {
      val decl: SequenceType = requiredType
      val sequenceType: SequenceType = SequenceType.makeSequenceType(decl.getPrimaryType, StaticProperty.ALLOWS_ZERO_OR_MORE)
      val role: RoleDiagnostic = new RoleDiagnostic(RoleDiagnostic.VARIABLE, variableName.getDisplayName, 0)
      this.setSequence(TypeChecker.strictTypeCheck(getSequence,
        sequenceType,
        role,
        visitor.getStaticContext))
      val actualItemType: ItemType = getSequence.getItemType
      refineTypeInformation(actualItemType,
        getRangeVariableCardinality,
        null,
        getSequence.getSpecialProperties,
        this)
    }
    if (Literal.isEmptySequence(getAction)) {
      getAction
    }
    getActionOp.typeCheck(visitor, contextInfo)
    actionCardinality = getAction.getCardinality
    this
  }

  def getRangeVariableCardinality: Int = StaticProperty.EXACTLY_ONE

  override def optimize(visitor: ExpressionVisitor,
                        contextItemType: ContextItemStaticInfo): Expression = {
    val config: Configuration = visitor.getConfiguration
    val opt: Optimizer = visitor.obtainOptimizer()
    val debug: Boolean =
      config.getBooleanProperty(Feature.TRACE_OPTIMIZER_DECISIONS)
    if (Choose.isSingleBranchChoice(getAction)) {
      getActionOp.optimize(visitor, contextItemType)
    }
    val p: Expression = promoteWhereClause()
    if (p != null) {
      if (debug) {
        opt.trace("Promoted where clause in for $" + getVariableName, p)
      }
      p.optimize(visitor, contextItemType)
    }
    val seq0: Expression = getSequence
    getSequenceOp.optimize(visitor, contextItemType)
    if (seq0 != getSequence) {
      optimize(visitor, contextItemType)
    }
    if (Literal.isEmptySequence(getSequence) && !(this
      .isInstanceOf[OuterForExpression])) {
      getSequence
    }
    val act0: Expression = getAction
    getActionOp.optimize(visitor, contextItemType)
    if (act0 != getAction) {
      optimize(visitor, contextItemType)
    }
    if (Literal.isEmptySequence(getAction)) {
      getAction
    }
    if (getSequence.isInstanceOf[SlashExpression] && getAction
      .isInstanceOf[SlashExpression]) {
      val path2: SlashExpression = getAction.asInstanceOf[SlashExpression]
      val start2: Expression = path2.getSelectExpression
      val step2: Expression = path2.getActionExpression
      if (start2.isInstanceOf[VariableReference] &&
        start2.asInstanceOf[VariableReference].getBinding == this &&
        ExpressionTool.getReferenceCount(getAction, this, inLoop = false) ==
          1 &&
        ((step2.getDependencies &
          (StaticProperty.DEPENDS_ON_POSITION | StaticProperty.DEPENDS_ON_LAST)) ==
          0)) {
        var newPath: Expression =
          new SlashExpression(getSequence, path2.getActionExpression)
        ExpressionTool.copyLocationInfo(this, newPath)
        newPath = newPath.simplify().typeCheck(visitor, contextItemType)
        if (newPath.isInstanceOf[SlashExpression]) {
          if (debug) {
            opt.trace("Collapsed return clause of for $" + getVariableName +
              " into path expression",
              newPath)
          }
          newPath.optimize(visitor, contextItemType)
        }
      }
    }
    if (getAction.isInstanceOf[VariableReference] &&
      getAction.asInstanceOf[VariableReference].getBinding ==
        this) {
      if (debug) {
        opt.trace("Collapsed redundant for expression $" + getVariableName,
          getSequence)
      }
      getSequence
    }
    if (getSequence.getCardinality == StaticProperty.EXACTLY_ONE) {
      val let: LetExpression = new LetExpression()
      let.setVariableQName(variableName)
      let.setRequiredType(
        SequenceType.makeSequenceType(getSequence.getItemType,
          StaticProperty.EXACTLY_ONE))
      let.setSequence(getSequence)
      let.setAction(getAction)
      let.setSlotNumber(slotNumber)
      let.setRetainedStaticContextLocally(getRetainedStaticContext)
      ExpressionTool.rebindVariableReferences(getAction, this, let)
      let
        .typeCheck(visitor, contextItemType)
        .optimize(visitor, contextItemType)
    }
    this
  }

  override def unordered(retainAllNodes: Boolean, forStreaming: Boolean): Expression = {
    this.setSequence(getSequence.unordered(retainAllNodes, forStreaming))
    this.setAction(getAction.unordered(retainAllNodes, forStreaming))
    this
  }

  override def getIntegerBounds(): Array[IntegerValue] =
    getAction.getIntegerBounds

  private def promoteWhereClause(): Expression = {
    if (Choose.isSingleBranchChoice(getAction)) {
      val condition: Expression =
        getAction.asInstanceOf[Choose].getCondition(0)
      val bindingList: Array[Binding] = Array(this)
      val list: List[Expression] = new ArrayList[Expression](5)
      var promotedCondition: Expression = null
      BooleanExpression.listAndComponents(condition, list)
      var i: Int = list.size - 1
      while (i >= 0) {
        val term: Expression = list.get(i)
        if (!ExpressionTool.dependsOnVariable(term, bindingList)) {
          promotedCondition =
            if (promotedCondition == null) term
            else new AndExpression(term, promotedCondition)
          list.remove(i)
        }
        i -= 1
      }
      if (promotedCondition != null) {
        if (list.isEmpty) {
          val oldThen: Expression = getAction.asInstanceOf[Choose].getAction(0)
          this.setAction(oldThen)
          Choose.makeConditional(condition, this)
        } else {
          var retainedCondition: Expression = list.get(0)
          for (i <- 1 until list.size) {
            retainedCondition =
              new AndExpression(retainedCondition, list.get(i))
          }
          getAction.asInstanceOf[Choose].setCondition(0, retainedCondition)
          val newIf: Expression = Choose.makeConditional(
            promotedCondition,
            this,
            Literal.makeEmptySequence)
          ExpressionTool.copyLocationInfo(this, newIf)
          return newIf
        }
      }
    }
    null
  }

  def copy(rebindings: RebindingMap): Expression = {
    val forExp: ForExpression = new ForExpression()
    ExpressionTool.copyLocationInfo(this, forExp)
    forExp.setRequiredType(requiredType)
    forExp.setVariableQName(variableName)
    forExp.setSequence(getSequence.copy(rebindings))
    rebindings.put(this, forExp)
    val newAction: Expression = getAction.copy(rebindings)
    forExp.setAction(newAction)
    forExp.variableName = variableName
    forExp.slotNumber = slotNumber
    forExp
  }

  override def markTailFunctionCalls(qName: StructuredQName, arity: Int): Int =
    if (!Cardinality.allowsMany(getSequence.getCardinality)) {
      ExpressionTool.markTailFunctionCalls(getAction, qName, arity)
    } else {
      UserFunctionCall.NOT_TAIL_CALL
    }

  override def isVacuousExpression(): Boolean = getAction.isVacuousExpression

  def getImplementationMethod: Int = ITERATE_METHOD | PROCESS_METHOD

  override def checkPermittedContents(parentType: SchemaType, whole: Boolean): Unit = {
    getAction.checkPermittedContents(parentType, whole = false)
  }

  override def iterate(context: XPathContext): SequenceIterator = {
    val base: SequenceIterator = getSequence.iterate(context)
    val map: MappingAction =
      new MappingAction(context, getLocalSlotNumber, getAction)
    actionCardinality match {
      case StaticProperty.EXACTLY_ONE =>
        new ItemMappingIterator(base, map, true)
      case StaticProperty.ALLOWS_ZERO_OR_ONE =>
        new ItemMappingIterator(base, map, false)
      case _ => new MappingIterator(base, map)

    }
  }

  override def process(output: Outputter, context: XPathContext): Unit = {
    val slot: Int = getLocalSlotNumber
    getSequence
      .iterate(context)
      .forEachOrFail((item) => {
        context.setLocalVariable(slot, item)
        getAction.process(output, context)
      })
  }

  override def evaluatePendingUpdates(context: XPathContext,
                                      pul: PendingUpdateList): Unit = {
    val slot: Int = getLocalSlotNumber
    getSequence
      .iterate(context)
      .forEachOrFail((item) => {
        context.setLocalVariable(slot, item)
        getAction.evaluatePendingUpdates(context, pul)
      })
  }

  def getItemType: ItemType = getAction.getItemType

  override def getStaticUType(contextItemType: UType): UType =
    getAction.getStaticUType(contextItemType)

  def computeCardinality(): Int = {
    val c1: Int = getSequence.getCardinality
    val c2: Int = getAction.getCardinality
    Cardinality.multiply(c1, c2)
  }

  override def toString: String =
    "for $" + getVariableEQName + " in " +
      (if (getSequence == null) "(...)" else getSequence.toString) +
      " return " +
      (if (getAction == null) "(...)"
      else ExpressionTool.parenthesize(getAction))

  override def toShortString: String =
    "for $" + getVariableQName.getDisplayName + " in " +
      (if (getSequence == null) "(...)" else getSequence.toShortString) +
      " return " +
      (if (getAction == null) "(...)" else getAction.toShortString)

  def export(out: ExpressionPresenter): Unit = {
    out.startElement("for", this)
    explainSpecializedAttributes(out)
    out.emitAttribute("var", getVariableQName)
    val varType: ItemType = getSequence.getItemType
    if (varType != AnyItemType) {
      out.emitAttribute("as", AlphaCode.fromItemType(varType))
    }
    out.emitAttribute("slot", "" + getLocalSlotNumber)
    out.setChildRole("in")
    getSequence.export(out)
    out.setChildRole("return")
    getAction.export(out)
    out.endElement()
  }

  def explainSpecializedAttributes(out: ExpressionPresenter): Unit = ()

  override def getStreamerName: String = "ForExpression"

}
