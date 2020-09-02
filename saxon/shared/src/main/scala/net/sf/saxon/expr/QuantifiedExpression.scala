package net.sf.saxon.expr

import net.sf.saxon.expr.parser._

import net.sf.saxon.functions.BooleanFn

import net.sf.saxon.functions.SystemFunction

import net.sf.saxon.model.BuiltInAtomicType

import net.sf.saxon.model.ItemType

import net.sf.saxon.model.UType

import net.sf.saxon.om.GroundedValue

import net.sf.saxon.om.Item

import net.sf.saxon.om.SequenceIterator

import net.sf.saxon.trace.ExpressionPresenter

import net.sf.saxon.trans.XPathException

import net.sf.saxon.value.BooleanValue

import net.sf.saxon.value.SequenceType

import scala.beans.{BeanProperty, BooleanBeanProperty}

class QuantifiedExpression extends Assignation {

  @BeanProperty
  var operator: Int = _

  override def getExpressionName: String = Token.tokens(operator)

  def computeCardinality(): Int = StaticProperty.EXACTLY_ONE

  override def typeCheck(visitor: ExpressionVisitor,
                         contextInfo: ContextItemStaticInfo): Expression = {
    getSequenceOp.typeCheck(visitor, contextInfo)
    if (Literal.isEmptySequence(getSequence)) {
      Literal.makeLiteral(BooleanValue.get(operator != Token.SOME), this)
    }
    this.setSequence(getSequence.unordered(retainAllNodes = false, forStreaming = false))
    val decl: SequenceType = getRequiredType
    if (decl.getCardinality == StaticProperty.ALLOWS_ZERO) {
      val err = new XPathException(
        "Range variable will never satisfy the type empty-sequence()",
        "XPTY0004")
      err.setIsTypeError(true)
      err.setLocation(getLocation)
      throw err
    }
    val sequenceType: SequenceType = SequenceType.makeSequenceType(
      decl.getPrimaryType,
      StaticProperty.ALLOWS_ZERO_OR_MORE)
    val role: RoleDiagnostic = new RoleDiagnostic(
      RoleDiagnostic.VARIABLE,
      getVariableQName.getDisplayName,
      0)
    this.setSequence(TypeChecker.strictTypeCheck(getSequence,
      sequenceType,
      role,
      visitor.getStaticContext))
    val actualItemType: ItemType = getSequence.getItemType
    refineTypeInformation(actualItemType,
      StaticProperty.EXACTLY_ONE,
      null,
      getSequence.getSpecialProperties,
      this)
    getActionOp.typeCheck(visitor, contextInfo)
    val err = TypeChecker.ebvError(
      getAction,
      visitor.getConfiguration.getTypeHierarchy)
    if (err != null) {
      err.setLocation(getLocation)
      throw err
    }
    this
  }

  override def optimize(visitor: ExpressionVisitor,
                        contextItemType: ContextItemStaticInfo): Expression = {
    getSequenceOp.optimize(visitor, contextItemType)
    getActionOp.optimize(visitor, contextItemType)
    val ebv: Expression = BooleanFn.rewriteEffectiveBooleanValue(
      getAction,
      visitor,
      contextItemType)
    if (ebv != null) {
      this.setAction(ebv)
      adoptChildExpression(ebv)
    }
    if (Literal.hasEffectiveBooleanValue(ebv, value = true)) {
      if (getOperator == Token.SOME) {
        SystemFunction.makeCall("exists",
          getRetainedStaticContext,
          getSequence)
      } else {
        val e2: Expression = new Literal(BooleanValue.TRUE)
        ExpressionTool.copyLocationInfo(this, e2)
        return e2
      }
    } else if (Literal.hasEffectiveBooleanValue(ebv, value = false)) {
      if (getOperator == Token.SOME) {
        val e2: Expression = new Literal(BooleanValue.FALSE)
        ExpressionTool.copyLocationInfo(this, e2)
        return e2
      } else {
        SystemFunction.makeCall("empty", getRetainedStaticContext, getSequence)
      }
    }
    if (getSequence.isInstanceOf[Literal]) {
      val seq: GroundedValue = getSequence.asInstanceOf[Literal].getValue
      val len = seq.getLength
      if (len == 0) {
        val e2: Expression = new Literal(
          BooleanValue.get(getOperator == Token.EVERY))
        ExpressionTool.copyLocationInfo(this, e2)
        return e2
      } else if (len == 1) {
        if (getAction.isInstanceOf[VariableReference] &&
          getAction.asInstanceOf[VariableReference].getBinding ==
            this) {
          SystemFunction.makeCall("boolean",
            getRetainedStaticContext,
            getSequence)
        } else {
          replaceVariable(getSequence)
          getAction
        }
      }
    }
    if (visitor.isOptimizeForStreaming) {
      val e3: Expression = visitor
        .obtainOptimizer()
        .optimizeQuantifiedExpressionForStreaming(this)
      if (e3 != null && e3 != this) {
        e3.optimize(visitor, contextItemType)
      }
    }
    this
  }

  override def getImplementationMethod: Int = Expression.EVALUATE_METHOD

  override def checkForUpdatingSubexpressions(): Unit = {
    getSequence.checkForUpdatingSubexpressions()
    getAction.checkForUpdatingSubexpressions()
  }

  override def isUpdatingExpression(): Boolean = false

  def copy(rebindings: RebindingMap): Expression = {
    val qe: QuantifiedExpression = new QuantifiedExpression()
    ExpressionTool.copyLocationInfo(this, qe)
    qe.setOperator(operator)
    qe.setVariableQName(variableName)
    qe.setRequiredType(requiredType)
    qe.setSequence(getSequence.copy(rebindings))
    rebindings.put(this, qe)
    val newAction: Expression = getAction.copy(rebindings)
    qe.setAction(newAction)
    qe.variableName = variableName
    qe.slotNumber = slotNumber
    qe
  }

  override def computeSpecialProperties(): Int = {
    val p: Int = super.computeSpecialProperties()
    p | StaticProperty.NO_NODES_NEWLY_CREATED
  }

  override def evaluateItem(context: XPathContext): BooleanValue =
    BooleanValue.get(effectiveBooleanValue(context))

  override def effectiveBooleanValue(context: XPathContext): Boolean = {
    val base: SequenceIterator = getSequence.iterate(context)
    val some: Boolean = operator == Token.SOME
    val slot: Int = getLocalSlotNumber
    var it: Item = null
    while (({
      it = base.next()
      it
    }) != null) {
      context.setLocalVariable(slot, it)
      if (some == getAction.effectiveBooleanValue(context)) {
        base.close()
        some
      }
    }
    !some
  }

  def getItemType: ItemType = BuiltInAtomicType.BOOLEAN

  override def getStaticUType(contextItemType: UType): UType = UType.BOOLEAN

  override def toString: String =
    (if (operator == Token.SOME) "some" else "every") + " $" +
      getVariableEQName +
      " in " +
      getSequence +
      " satisfies " +
      ExpressionTool.parenthesize(getAction)

  override def toShortString: String =
    (if (operator == Token.SOME) "some" else "every") + " $" +
      getVariableName +
      " in " +
      getSequence.toShortString +
      " satisfies ..."

  def export(out: ExpressionPresenter): Unit = {
    out.startElement(Token.tokens(operator), this)
    out.emitAttribute("var", getVariableQName)
    out.emitAttribute("slot", "" + slotNumber)
    getSequence.export(out)
    getAction.export(out)
    out.endElement()
  }

}
