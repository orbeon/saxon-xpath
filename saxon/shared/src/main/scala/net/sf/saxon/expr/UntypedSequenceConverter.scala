package net.sf.saxon.expr

import net.sf.saxon.utils.Configuration

import net.sf.saxon.expr.parser.ContextItemStaticInfo

import net.sf.saxon.expr.parser.ExpressionTool

import net.sf.saxon.expr.parser.ExpressionVisitor

import net.sf.saxon.expr.parser.RebindingMap

import net.sf.saxon.lib.ConversionRules

import net.sf.saxon.model._

import net.sf.saxon.trace.ExpressionPresenter

import net.sf.saxon.trans.XPathException

import net.sf.saxon.value.AtomicValue

import net.sf.saxon.value.UntypedAtomicValue

object UntypedSequenceConverter {

  def makeUntypedSequenceConverter(
                                    config: Configuration,
                                    operand: Expression,
                                    requiredItemType: PlainType): UntypedSequenceConverter = {
    val atomicSeqConverter: UntypedSequenceConverter =
      new UntypedSequenceConverter(operand, requiredItemType)
    val rules: ConversionRules = config.getConversionRules
    var untypedConverter: Converter = null
    if (requiredItemType.isNamespaceSensitive) {
      throw new XPathException(
        "Cannot convert untyped atomic values to a namespace-sensitive type",
        "XPTY0117")
    }
    if (requiredItemType.isAtomicType) {
      untypedConverter = rules.getConverter(
        BuiltInAtomicType.UNTYPED_ATOMIC,
        requiredItemType.asInstanceOf[AtomicType])
    } else if (requiredItemType == NumericType.getInstance) {
      untypedConverter = rules.getConverter(BuiltInAtomicType.UNTYPED_ATOMIC,
        BuiltInAtomicType.DOUBLE)
      atomicSeqConverter.requiredItemType = BuiltInAtomicType.DOUBLE
    } else {
      untypedConverter =
        new StringConverter.StringToUnionConverter(requiredItemType, rules)
    }
    val converter: Converter = new UntypedConverter(rules, untypedConverter)
    atomicSeqConverter.setConverter(converter)
    atomicSeqConverter
  }

  class UntypedConverter(rules: ConversionRules, var untypedConverter: Converter) extends Converter {

    this.setConversionRules(rules)

    override def convert(input: AtomicValue): ConversionResult =
      if (input.isInstanceOf[UntypedAtomicValue]) {
        untypedConverter.convert(input)
      } else {
        input
      }

  }

  def makeUntypedSequenceRejector(
                                   config: Configuration,
                                   operand: Expression,
                                   requiredItemType: PlainType): UntypedSequenceConverter = {
    val atomicSeqConverter: UntypedSequenceConverter =
      new UntypedSequenceConverter(operand, requiredItemType)
    val rules: ConversionRules = config.getConversionRules
    val untypedConverter: Converter = new Converter() {
      def convert(input: AtomicValue): ConversionResult = {
        var vf: ValidationFailure = new ValidationFailure(
          "Implicit conversion of untypedAtomic value to " + requiredItemType.toString +
            " is not allowed")
        vf.setErrorCode("XPTY0117")
        vf.setLocator(operand.getLocation)
        vf
      }
    }
    val converter: Converter = new UntypedConverter(rules, untypedConverter)
    atomicSeqConverter.setConverter(converter)
    atomicSeqConverter
  }

}

class UntypedSequenceConverter(sequence: Expression,
                               requiredItemType: PlainType)
  extends AtomicSequenceConverter(sequence, requiredItemType) {

  override def typeCheck(visitor: ExpressionVisitor,
                         contextInfo: ContextItemStaticInfo): Expression = {
    val e2: Expression = super.typeCheck(visitor, contextInfo)
    if (e2 != this) {
      return e2
    }
    val th: TypeHierarchy = visitor.getConfiguration.getTypeHierarchy
    val base: Expression = getBaseExpression
    if (th.relationship(base.getItemType, BuiltInAtomicType.UNTYPED_ATOMIC) ==
      Affinity.DISJOINT ||
      base.hasSpecialProperty(StaticProperty.NOT_UNTYPED_ATOMIC)) {
      getBaseExpression
    }
    this
  }

  override def computeSpecialProperties(): Int = {
    val p: Int = super.computeSpecialProperties()
    p | StaticProperty.NO_NODES_NEWLY_CREATED | StaticProperty.NOT_UNTYPED_ATOMIC
  }

  override def copy(rebindings: RebindingMap): Expression = {
    val atomicConverter: UntypedSequenceConverter =
      new UntypedSequenceConverter(getBaseExpression.copy(rebindings),
        getRequiredItemType)
    ExpressionTool.copyLocationInfo(this, atomicConverter)
    atomicConverter.setConverter(converter)
    atomicConverter.setRoleDiagnostic(getRoleDiagnostic)
    atomicConverter
  }

  override def getItemType(): ItemType =
    if (getBaseExpression.getItemType == BuiltInAtomicType.UNTYPED_ATOMIC) {
      getRequiredItemType
    } else {
      val th: TypeHierarchy = getConfiguration.getTypeHierarchy
      Type.getCommonSuperType(getRequiredItemType,
        getBaseExpression.getItemType,
        th)
    }

  override def computeCardinality(): Int = getBaseExpression.getCardinality

  override def equals(other: Any): Boolean = other match {
    case other: UntypedSequenceConverter =>
      getBaseExpression.isEqual(other.getBaseExpression)
    case _ => false

  }

  override def computeHashCode(): Int = super.computeHashCode()

   override def displayOperator(config: Configuration): String =
    "convertUntyped"

  override def getExpressionName(): String = "convertUntyped"

  override def toShortString(): String = getBaseExpression.toShortString()

  override def export(destination: ExpressionPresenter): Unit = {
    destination.startElement("cvUntyped", this)
    destination.emitAttribute("to",
      AlphaCode.fromItemType(getRequiredItemType))
    if (getRoleDiagnostic != null) {
      destination.emitAttribute("diag", getRoleDiagnostic.save())
    }
    getBaseExpression.export(destination)
    destination.endElement()
  }

}
