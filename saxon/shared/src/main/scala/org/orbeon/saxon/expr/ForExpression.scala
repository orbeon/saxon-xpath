package org.orbeon.saxon.expr

import org.orbeon.saxon.event.Outputter
import org.orbeon.saxon.expr.Expression._
import org.orbeon.saxon.expr.ForExpression._
import org.orbeon.saxon.expr.flwor.OuterForExpression
import org.orbeon.saxon.expr.instruct.Choose
import org.orbeon.saxon.expr.parser._
import org.orbeon.saxon.lib.Feature
import org.orbeon.saxon.model._
import org.orbeon.saxon.om.{Item, SequenceIterator, StructuredQName}
import org.orbeon.saxon.trace.ExpressionPresenter
import org.orbeon.saxon.value.{Cardinality, IntegerValue, SequenceType}

import java.util.ArrayList

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
    if (Literal.isEmptySequence(getSequence) && ! this.isInstanceOf[OuterForExpression])
      return getSequence

    if (requiredType != null) {
      val decl         = requiredType
      val sequenceType = SequenceType.makeSequenceType(decl.getPrimaryType, StaticProperty.ALLOWS_ZERO_OR_MORE)
      val role         = new RoleDiagnostic(RoleDiagnostic.VARIABLE, variableName.getDisplayName, 0)
      this.setSequence(TypeChecker.strictTypeCheck(getSequence,
        sequenceType,
        role,
        visitor.getStaticContext))
      val actualItemType = getSequence.getItemType
      refineTypeInformation(actualItemType,
        getRangeVariableCardinality,
        null,
        getSequence.getSpecialProperties,
        this)
    }
    if (Literal.isEmptySequence(getAction))
      return getAction

    getActionOp.typeCheck(visitor, contextInfo)
    actionCardinality = getAction.getCardinality
    this
  }

  def getRangeVariableCardinality: Int = StaticProperty.EXACTLY_ONE

  override def optimize(visitor: ExpressionVisitor,
                        contextItemType: ContextItemStaticInfo): Expression = {
    val config         = visitor.getConfiguration
    val opt = visitor.obtainOptimizer()
    val debug          = config.getBooleanProperty(Feature.TRACE_OPTIMIZER_DECISIONS)
    if (Choose.isSingleBranchChoice(getAction)) {
      getActionOp.optimize(visitor, contextItemType)
    }
    val p = promoteWhereClause()
    if (p != null) {
      if (debug)
        opt.trace("Promoted where clause in for $" + getVariableName, p)
      return p.optimize(visitor, contextItemType)
    }
    val seq0 = getSequence
    getSequenceOp.optimize(visitor, contextItemType)
    if (seq0 != getSequence)
      return optimize(visitor, contextItemType)

    if (Literal.isEmptySequence(getSequence) && ! this.isInstanceOf[OuterForExpression])
      return getSequence

    val act0 = getAction
    getActionOp.optimize(visitor, contextItemType)
    if (act0 != getAction)
      return optimize(visitor, contextItemType)
    if (Literal.isEmptySequence(getAction)) {
      return getAction
    }
    if (getSequence.isInstanceOf[SlashExpression] && getAction
      .isInstanceOf[SlashExpression]) {
      val path2             = getAction.asInstanceOf[SlashExpression]
      val start2 = path2.getSelectExpression
      val step2 = path2.getActionExpression
      start2 match {
        case varRef: VariableReference if (step2.getDependencies &
          (StaticProperty.DEPENDS_ON_POSITION | StaticProperty.DEPENDS_ON_LAST)) ==
          0 && ExpressionTool.getReferenceCount(getAction, this, inLoop = false) ==
          1 && varRef.getBinding == this =>
          var newPath: Expression =
            new SlashExpression(getSequence, path2.getActionExpression)
          ExpressionTool.copyLocationInfo(this, newPath)
          newPath = newPath.simplify().typeCheck(visitor, contextItemType)
          if (newPath.isInstanceOf[SlashExpression]) {
            if (debug) {
              opt.trace(
                "Collapsed return clause of for $" + getVariableName +
                  " into path expression",
                newPath
              )
            }
            return newPath.optimize(visitor, contextItemType)
          }
        case _                              =>
      }
    }
    getAction match {
      case varRef: VariableReference if varRef.getBinding == this =>
        if (debug)
          opt.trace("Collapsed redundant for expression $" + getVariableName, getSequence)
        return getSequence
      case _                                                            =>
    }
    if (getSequence.getCardinality == StaticProperty.EXACTLY_ONE) {
      val let = new LetExpression
      let.setVariableQName(variableName)
      let.setRequiredType(
        SequenceType.makeSequenceType(getSequence.getItemType,
          StaticProperty.EXACTLY_ONE))
      let.setSequence(getSequence)
      let.setAction(getAction)
      let.setSlotNumber(slotNumber)
      let.setRetainedStaticContextLocally(getRetainedStaticContext)
      ExpressionTool.rebindVariableReferences(getAction, this, let)
      return let
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

  override def getIntegerBounds: Array[IntegerValue] =
    getAction.getIntegerBounds

  private def promoteWhereClause(): Expression = {
    if (Choose.isSingleBranchChoice(getAction)) {
      val condition              = getAction.asInstanceOf[Choose].getCondition(0)
      val bindingList: Array[Binding] = Array(this)
      val list = new ArrayList[Expression](5)
      var promotedCondition: Expression = null
      BooleanExpression.listAndComponents(condition, list)
      var i = list.size - 1
      while (i >= 0) {
        val term = list.get(i)
        if (! ExpressionTool.dependsOnVariable(term, bindingList)) {
          promotedCondition =
            if (promotedCondition == null)
              term
            else
              new AndExpression(term, promotedCondition)
          list.remove(i)
        }
        i -= 1
      }
      if (promotedCondition != null) {
        if (list.isEmpty) {
          val oldThen = getAction.asInstanceOf[Choose].getAction(0)
          this.setAction(oldThen)
          return Choose.makeConditional(condition, this)
        } else {
          var retainedCondition = list.get(0)
          for (i <- 1 until list.size)
            retainedCondition = new AndExpression(retainedCondition, list.get(i))
          getAction.asInstanceOf[Choose].setCondition(0, retainedCondition)
          val newIf = Choose.makeConditional(
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
    val forExp = new ForExpression
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
    if (!Cardinality.allowsMany(getSequence.getCardinality))
      ExpressionTool.markTailFunctionCalls(getAction, qName, arity)
    else
      UserFunctionCall.NOT_TAIL_CALL

  override def isVacuousExpression: Boolean = getAction.isVacuousExpression

  def getImplementationMethod: Int = ITERATE_METHOD | PROCESS_METHOD

  override def checkPermittedContents(parentType: SchemaType, whole: Boolean): Unit =
    getAction.checkPermittedContents(parentType, whole = false)

  override def iterate(context: XPathContext): SequenceIterator = {
    val base = getSequence.iterate(context)
    val map  = new MappingAction(context, getLocalSlotNumber, getAction)
    actionCardinality match {
      case StaticProperty.EXACTLY_ONE =>
        new ItemMappingIterator(base, map, true)
      case StaticProperty.ALLOWS_ZERO_OR_ONE =>
        new ItemMappingIterator(base, map, false)
      case _ =>
        new MappingIterator(base, map)
    }
  }

  override def process(output: Outputter, context: XPathContext): Unit = {
    val slot = getLocalSlotNumber
    getSequence
      .iterate(context)
      .forEachOrFail((item) => {
        context.setLocalVariable(slot, item)
        getAction.process(output, context)
      })
  }

  override def evaluatePendingUpdates(context: XPathContext,
                                      pul: PendingUpdateList): Unit = {
    val slot = getLocalSlotNumber
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
    val c1 = getSequence.getCardinality
    val c2 = getAction.getCardinality
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
    if (varType != AnyItemType)
      out.emitAttribute("as", AlphaCode.fromItemType(varType))
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
