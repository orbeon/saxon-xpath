package org.orbeon.saxon.expr

import org.orbeon.saxon.expr.parser.ContextItemStaticInfo

import org.orbeon.saxon.expr.parser.ExpressionTool

import org.orbeon.saxon.expr.parser.ExpressionVisitor

import org.orbeon.saxon.expr.parser.RebindingMap

import org.orbeon.saxon.model._

import org.orbeon.saxon.om._

import org.orbeon.saxon.trace.ExpressionPresenter

import org.orbeon.saxon.trans.XPathException

import org.orbeon.saxon.value.AtomicValue

import org.orbeon.saxon.value.BooleanValue

import org.orbeon.saxon.value.Cardinality


class CastableExpression(source: Expression,
                         target: AtomicType,
                         allowEmpty: Boolean)
  extends CastingExpression(source, target, allowEmpty) {

  override def typeCheck(visitor: ExpressionVisitor,
                         contextInfo: ContextItemStaticInfo): Expression = {
    getOperand.typeCheck(visitor, contextInfo)
    val operand: Expression = getBaseExpression
    val sourceItemType: ItemType = operand.getItemType
    val atomizedType: AtomicType =
      sourceItemType.getAtomizedItemType.getPrimitiveItemType
        .asInstanceOf[AtomicType]
    if (!(atomizedType == BuiltInAtomicType.ANY_ATOMIC)) {
      converter = visitor.getConfiguration.getConversionRules
        .getConverter(atomizedType, getTargetType)
      if (converter == null) {
        if (!allowsEmpty() || !Cardinality.allowsZero(operand.getCardinality)) {

          return Literal.makeLiteral(BooleanValue.FALSE, this)
        }
      } else {
        if (getTargetPrimitiveType.isNamespaceSensitive) {
          converter = converter.setNamespaceResolver(getRetainedStaticContext)
        }
        if (converter.isAlwaysSuccessful && !allowsEmpty() &&
          operand.getCardinality == StaticProperty.ALLOWS_ONE) {
          return Literal.makeLiteral(BooleanValue.TRUE, this)
        }
      }
    }
    this.setBaseExpression(operand)
    if (operand.isInstanceOf[Literal]) {
      return preEvaluate()
    }
    this
  }

   def preEvaluate(): Expression = {
    val literalOperand: GroundedValue =
      getBaseExpression.asInstanceOf[Literal].getValue
    if (literalOperand.isInstanceOf[AtomicValue] && converter != null) {
      val result: ConversionResult =
        converter.convert(literalOperand.asInstanceOf[AtomicValue])
      return Literal.makeLiteral(
        BooleanValue.get(!(result.isInstanceOf[ValidationFailure])),
        this)
    }
    val length: Int = literalOperand.getLength
    if (length == 0) {
      return Literal.makeLiteral(BooleanValue.get(allowsEmpty()), this)
    }
    if (length > 1) {
      return Literal.makeLiteral(BooleanValue.FALSE, this)
    }
    this
  }



  override def optimize(visitor: ExpressionVisitor,
                        contextInfo: ContextItemStaticInfo): Expression = {
    optimizeChildren(visitor, contextInfo)
    if (getBaseExpression.isInstanceOf[Literal]) {
      return preEvaluate()
    }
    this
  }

  override def getImplementationMethod: Int = Expression.EVALUATE_METHOD

  override def equals(other: Any): Boolean =
    other.isInstanceOf[CastableExpression] &&
      getBaseExpression.isEqual(
        other.asInstanceOf[CastableExpression].getBaseExpression) &&
      getTargetType ==
        other.asInstanceOf[CastableExpression].getTargetType &&
      allowsEmpty() ==
        other.asInstanceOf[CastableExpression].allowsEmpty()

  override def computeHashCode(): Int = super.computeHashCode() ^ 0x5555

  override def getItemType: ItemType = BuiltInAtomicType.BOOLEAN


  override def getStaticUType(contextItemType: UType): UType = UType.BOOLEAN

  override def computeCardinality(): Int = StaticProperty.EXACTLY_ONE



  def copy(rebindings: RebindingMap): Expression = {
    val ce: CastableExpression = new CastableExpression(
      getBaseExpression.copy(rebindings),
      getTargetType,
      allowsEmpty())
    ExpressionTool.copyLocationInfo(this, ce)
    ce.setRetainedStaticContext(getRetainedStaticContext)
    ce.converter = converter
    ce
  }

  override def evaluateItem(context: XPathContext): BooleanValue =
    BooleanValue.get(effectiveBooleanValue(context))

  override def effectiveBooleanValue(context: XPathContext): Boolean = {

    var count: Int = 0
    val iter: SequenceIterator = getBaseExpression.iterate(context)
    var item: Item = null
    while (({
      item = iter.next()
      item
    }) != null) if (item.isInstanceOf[NodeInfo]) {
      val atomizedValue: AtomicSequence = item.atomize()
      val length: Int = SequenceTool.getLength(atomizedValue)
      count += length
      if (count > 1) {
        return false
      }
      if (length != 0) {
        val av: AtomicValue = atomizedValue.head
        if (!isCastable(av, getTargetType, context)) {
          return false
        }
      }
    } else if (item.isInstanceOf[AtomicValue]) {
      val av: AtomicValue = item.asInstanceOf[AtomicValue]
      count += 1
      if (count > 1) {
        return false
      }
      if (!isCastable(av, getTargetType, context)) {
        return false
      }
    } else {
      throw new XPathException("Input to cast cannot be atomized", "XPTY0004")
    }
    count != 0 || allowsEmpty()
  }




  private def isCastable(value: AtomicValue,
                         targetType: AtomicType,
                         context: XPathContext): Boolean = {
    var converter: Converter = this.converter
    if (converter == null) {
      converter = context.getConfiguration.getConversionRules
        .getConverter(value.getPrimitiveType, targetType)
      if (converter == null) {
        return false
      }
      if (converter.isAlwaysSuccessful) {
        return  true
      }
      if (getTargetType.isNamespaceSensitive) {
        converter = converter.setNamespaceResolver(getRetainedStaticContext)
      }
    }
    !(converter.convert(value).isInstanceOf[ValidationFailure])
  }

  override def getExpressionName: String = "castable"

  override def toString: String =
    getBaseExpression.toString + " castable as " + getTargetType.getEQName

  override def export(out: ExpressionPresenter): Unit = {
    export(out, "castable")
  }














}







