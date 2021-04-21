package org.orbeon.saxon.expr

import org.orbeon.saxon.expr.parser._
import org.orbeon.saxon.functions.{BooleanFn, SystemFunction}
import org.orbeon.saxon.model.{BuiltInAtomicType, ItemType, UType}
import org.orbeon.saxon.om.Item
import org.orbeon.saxon.trace.ExpressionPresenter
import org.orbeon.saxon.trans.XPathException
import org.orbeon.saxon.value.{BooleanValue, SequenceType}

import scala.beans.BeanProperty


class QuantifiedExpression extends Assignation {

  @BeanProperty
  var operator: Int = _

  override def getExpressionName: String = Token.tokens(operator)

  def computeCardinality(): Int = StaticProperty.EXACTLY_ONE

  override def typeCheck(visitor: ExpressionVisitor,
                         contextInfo: ContextItemStaticInfo): Expression = {
    getSequenceOp.typeCheck(visitor, contextInfo)
    if (Literal.isEmptySequence(getSequence))
      return Literal.makeLiteral(BooleanValue.get(operator != Token.SOME), this)
    this.setSequence(getSequence.unordered(retainAllNodes = false, forStreaming = false))
    val decl = getRequiredType
    if (decl.getCardinality == StaticProperty.ALLOWS_ZERO) {
      val err = new XPathException("Range variable will never satisfy the type empty-sequence()", "XPTY0004")
      err.setIsTypeError(true)
      err.setLocation(getLocation)
      throw err
    }
    val sequenceType = SequenceType.makeSequenceType(
      decl.getPrimaryType,
      StaticProperty.ALLOWS_ZERO_OR_MORE)
    val role         = new RoleDiagnostic(
      RoleDiagnostic.VARIABLE,
      getVariableQName.getDisplayName,
      0)
    this.setSequence(TypeChecker.strictTypeCheck(getSequence,
      sequenceType,
      role,
      visitor.getStaticContext))
    val actualItemType = getSequence.getItemType
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
    val ebv = BooleanFn.rewriteEffectiveBooleanValue(
      getAction,
      visitor,
      contextItemType)
    if (ebv != null) {
      this.setAction(ebv)
      adoptChildExpression(ebv)
    }
    if (Literal.hasEffectiveBooleanValue(ebv, value = true)) {
      if (getOperator == Token.SOME) {
        return SystemFunction.makeCall("exists",
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
        return SystemFunction.makeCall("empty", getRetainedStaticContext, getSequence)
      }
    }
    getSequence match {
      case literal: Literal =>
        val seq = literal.getValue
        val len = seq.getLength
        if (len == 0) {
          val e2: Expression = new Literal(
            BooleanValue.get(getOperator == Token.EVERY)
          )
          ExpressionTool.copyLocationInfo(this, e2)
          return e2
        } else if (len == 1) {
          getAction match {
            case varRef: VariableReference if varRef.getBinding ==
              this =>
              return SystemFunction.makeCall(
                "boolean",
                getRetainedStaticContext,
                getSequence
              )
            case _ =>
              replaceVariable(getSequence)
              return getAction
          }
        }
      case _                =>
    }
    if (visitor.isOptimizeForStreaming) {
      val e3 = visitor
        .obtainOptimizer()
        .optimizeQuantifiedExpressionForStreaming(this)
      if (e3 != null && e3 != this)
        return e3.optimize(visitor, contextItemType)
    }
    this
  }

  override def getImplementationMethod: Int = Expression.EVALUATE_METHOD

  override def checkForUpdatingSubexpressions(): Unit = {
    getSequence.checkForUpdatingSubexpressions()
    getAction.checkForUpdatingSubexpressions()
  }

  override def isUpdatingExpression: Boolean = false

  def copy(rebindings: RebindingMap): Expression = {
    val qe = new QuantifiedExpression
    ExpressionTool.copyLocationInfo(this, qe)
    qe.setOperator(operator)
    qe.setVariableQName(variableName)
    qe.setRequiredType(requiredType)
    qe.setSequence(getSequence.copy(rebindings))
    rebindings.put(this, qe)
    val newAction = getAction.copy(rebindings)
    qe.setAction(newAction)
    qe.variableName = variableName
    qe.slotNumber = slotNumber
    qe
  }

  override def computeSpecialProperties(): Int = {
    val p = super.computeSpecialProperties()
    p | StaticProperty.NO_NODES_NEWLY_CREATED
  }

  override def evaluateItem(context: XPathContext): BooleanValue =
    BooleanValue.get(effectiveBooleanValue(context))

  override def effectiveBooleanValue(context: XPathContext): Boolean = {
    val base = getSequence.iterate(context)
    val some = operator == Token.SOME
    val slot = getLocalSlotNumber
    var it: Item = null
    while ({
      it = base.next()
      it
    } != null) {
      context.setLocalVariable(slot, it)
      if (some == getAction.effectiveBooleanValue(context)) {
        base.close()
        return some
      }
    }
    ! some
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
