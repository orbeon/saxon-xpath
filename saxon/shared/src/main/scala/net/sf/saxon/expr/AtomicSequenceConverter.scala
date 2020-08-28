package net.sf.saxon.expr

import net.sf.saxon.utils.Configuration

import net.sf.saxon.expr.parser._

import net.sf.saxon.lib.ConversionRules

import net.sf.saxon.model._

import net.sf.saxon.om.GroundedValue

import net.sf.saxon.om.Item

import net.sf.saxon.om.SequenceIterator

import net.sf.saxon.trace.ExpressionPresenter

import net.sf.saxon.trans.XPathException

import net.sf.saxon.value.AtomicValue

import net.sf.saxon.value.StringValue

import AtomicSequenceConverter._

import scala.beans.{BeanProperty, BooleanBeanProperty}


object AtomicSequenceConverter {

  var TO_STRING_MAPPER: ToStringMappingFunction = new ToStringMappingFunction()

  class AtomicSequenceMappingFunction extends ItemMappingFunction {

    private var converter: Converter = _

    private var errorCode: String = _

    def setConverter(converter: Converter): Unit = {
      this.converter = converter
    }

    def setErrorCode(code: String): Unit = {
      this.errorCode = code
    }

    def mapItem(item: Item): AtomicValue = {
      val result: ConversionResult =
        converter.convert(item.asInstanceOf[AtomicValue])
      if (errorCode != null && result.isInstanceOf[ValidationFailure]) {
        result.asInstanceOf[ValidationFailure].setErrorCode(errorCode)
      }
      result.asAtomic()
    }

  }

  class ToStringMappingFunction extends ItemMappingFunction {

    def mapItem(item: Item): StringValue =
      StringValue.makeStringValue(item.getStringValueCS)

  }

}

class AtomicSequenceConverter(sequence: Expression,
                              var requiredItemType: PlainType)
  extends UnaryExpression(sequence) {

  var converter: Converter = _

  var roleDiagnostic: RoleDiagnostic = _

  def allocateConverterStatically(config: Configuration,
                                  allowNull: Boolean): Unit = {
    converter = allocateConverter(config, allowNull, getBaseExpression.getItemType)
  }

  def allocateConverter(config: Configuration, allowNull: Boolean): Converter =
    allocateConverter(config, allowNull, getBaseExpression.getItemType)

  def getConverterDynamically(context: XPathContext): Converter = {
    if (converter != null) {
      return converter
    }
    allocateConverter(context.getConfiguration, allowNull = false)
  }

  def allocateConverter(config: Configuration,
                        allowNull: Boolean,
                        sourceType: ItemType): Converter = {
    val rules: ConversionRules = config.getConversionRules
    var converter: Converter = null
    if (sourceType.isInstanceOf[ErrorType]) {
      converter = Converter.IdentityConverter.INSTANCE
    } else if (!(sourceType.isInstanceOf[AtomicType])) {
      converter = null
    } else if (requiredItemType.isInstanceOf[AtomicType]) {
      converter = rules.getConverter(sourceType.asInstanceOf[AtomicType],
        requiredItemType.asInstanceOf[AtomicType])
    } else if (requiredItemType.asInstanceOf[SimpleType].isUnionType) {
      converter =
        new StringConverter.StringToUnionConverter(requiredItemType, rules)
    }
    if (converter == null && !allowNull) {
      converter = new Converter(rules) {
        override def convert(input: AtomicValue): ConversionResult = {
          var converter: Converter = rules.getConverter(
            input.getPrimitiveType,
            requiredItemType.asInstanceOf[AtomicType])
          if (converter == null) {
            new ValidationFailure(
              "Cannot convert value from " + input.getPrimitiveType +
                " to " +
                requiredItemType)
          } else {
            converter.convert(input)
          }
        }
      }
    }
    converter
  }

  def getOperandRole(): OperandRole = OperandRole.ATOMIC_SEQUENCE

  def getRequiredItemType(): PlainType = requiredItemType

  def getConverter(): Converter = converter

  def setConverter(converter: Converter): Unit = {
    this.converter = converter
  }

  def setRoleDiagnostic(role: RoleDiagnostic): Unit = {
    if (role != null && "XPTY0004" != role.getErrorCode) {
      this.roleDiagnostic = role
    }
  }

  def getRoleDiagnostic: RoleDiagnostic = roleDiagnostic

  override def simplify(): Expression = {
    val operand: Expression = getBaseExpression.simplify()
    this.setBaseExpression(operand)
    if (operand.isInstanceOf[Literal] && requiredItemType
      .isInstanceOf[AtomicType]) {
      if (Literal.isEmptySequence(operand)) {
        return operand
      }
      val config: Configuration = getConfiguration
      allocateConverterStatically(config, allowNull = true)
      if (converter != null) {
        val `val`: GroundedValue =
          iterate(new EarlyEvaluationContext(config)).materialize()
        Literal.makeLiteral(`val`, operand)
      }
    }
    this
  }

  override def typeCheck(visitor: ExpressionVisitor,
                         contextInfo: ContextItemStaticInfo): Expression = {
    typeCheckChildren(visitor, contextInfo)
    val config: Configuration = visitor.getConfiguration
    val th: TypeHierarchy = config.getTypeHierarchy
    val operand: Expression = getBaseExpression
    if (th.isSubType(operand.getItemType, requiredItemType)) {
      operand
    } else {
      if (converter == null) {
        allocateConverterStatically(config, allowNull = true)
      }
      this
    }
  }

  override def optimize(visitor: ExpressionVisitor,
                        contextInfo: ContextItemStaticInfo): Expression = {
    val e: Expression = super.optimize(visitor, contextInfo)
    if (e != this) {
      return e
    }
    if (getBaseExpression.isInstanceOf[UntypedSequenceConverter]) {
      val asc: UntypedSequenceConverter =
        getBaseExpression.asInstanceOf[UntypedSequenceConverter]
      val ascType: ItemType = asc.getItemType
      if (ascType == requiredItemType) {
        getBaseExpression
      } else if ((requiredItemType == BuiltInAtomicType.STRING ||
        requiredItemType == BuiltInAtomicType.UNTYPED_ATOMIC) &&
        (ascType == BuiltInAtomicType.STRING || ascType == BuiltInAtomicType.UNTYPED_ATOMIC)) {
        val old: UntypedSequenceConverter =
          getBaseExpression.asInstanceOf[UntypedSequenceConverter]
        val asc2: UntypedSequenceConverter =
          new UntypedSequenceConverter(old.getBaseExpression, requiredItemType)
        asc2.typeCheck(visitor, contextInfo).optimize(visitor, contextInfo)
      }
    } else if (getBaseExpression.isInstanceOf[AtomicSequenceConverter]) {
      val asc: AtomicSequenceConverter =
        getBaseExpression.asInstanceOf[AtomicSequenceConverter]
      val ascType: ItemType = asc.getItemType
      if (ascType == requiredItemType) {
        getBaseExpression
      } else if ((requiredItemType == BuiltInAtomicType.STRING ||
        requiredItemType == BuiltInAtomicType.UNTYPED_ATOMIC) &&
        (ascType == BuiltInAtomicType.STRING || ascType == BuiltInAtomicType.UNTYPED_ATOMIC)) {
        val old: AtomicSequenceConverter =
          getBaseExpression.asInstanceOf[AtomicSequenceConverter]
        val asc2: AtomicSequenceConverter =
          new AtomicSequenceConverter(old.getBaseExpression, requiredItemType)
        asc2.typeCheck(visitor, contextInfo).optimize(visitor, contextInfo)
      }
    }
    this
  }

  override def getImplementationMethod(): Int = Expression.ITERATE_METHOD

  override def computeSpecialProperties(): Int = {
    var p: Int = super
      .computeSpecialProperties() | StaticProperty.NO_NODES_NEWLY_CREATED
    if (requiredItemType == BuiltInAtomicType.UNTYPED_ATOMIC) {
      p &= ~StaticProperty.NOT_UNTYPED_ATOMIC
    } else {
      p |= StaticProperty.NOT_UNTYPED_ATOMIC
    }
    p
  }

  override def getStreamerName(): String = "AtomicSequenceConverter"

  def copy(rebindings: RebindingMap): Expression = {
    val atomicConverter: AtomicSequenceConverter = new AtomicSequenceConverter(
      getBaseExpression.copy(rebindings),
      requiredItemType)
    ExpressionTool.copyLocationInfo(this, atomicConverter)
    atomicConverter.setConverter(converter)
    atomicConverter.setRoleDiagnostic(getRoleDiagnostic)
    atomicConverter
  }

  override def iterate(context: XPathContext): SequenceIterator = {
    val base: SequenceIterator = getBaseExpression.iterate(context)
    val conv: Converter = getConverterDynamically(context)
    if (conv == Converter.ToStringConverter.INSTANCE) {
      new ItemMappingIterator(base, TO_STRING_MAPPER, true)
    } else {
      val mapper: AtomicSequenceMappingFunction =
        new AtomicSequenceMappingFunction()
      mapper.setConverter(conv)
      if (roleDiagnostic != null) {
        mapper.setErrorCode(roleDiagnostic.getErrorCode)
      }
      new ItemMappingIterator(base, mapper, true)
    }
  }

  override def evaluateItem(context: XPathContext): AtomicValue = {
    val conv: Converter = getConverterDynamically(context)
    val item: AtomicValue =
      getBaseExpression.evaluateItem(context).asInstanceOf[AtomicValue]
    if (item == null) {
      return null
    }
    val result: ConversionResult = conv.convert(item)
    if (roleDiagnostic != null && result.isInstanceOf[ValidationFailure]) {
      result
        .asInstanceOf[ValidationFailure]
        .setErrorCode(roleDiagnostic.getErrorCode)
    }
    result.asAtomic()
  }

  override def getItemType(): ItemType = requiredItemType

  override def computeCardinality(): Int = getBaseExpression.getCardinality

  override def computeHashCode(): Int =
    super.computeHashCode() ^ requiredItemType.hashCode

  override def getExpressionName(): String = "convert"

  override def displayOperator(config: Configuration): String =
    "convert"

  override def export(destination: ExpressionPresenter): Unit = {
    destination.startElement("convert", this)
    destination.emitAttribute(
      "from",
      AlphaCode.fromItemType(getBaseExpression.getItemType))
    destination.emitAttribute("to", AlphaCode.fromItemType(requiredItemType))
    if (converter.isInstanceOf[Converter.PromoterToDouble] || converter
      .isInstanceOf[Converter.PromoterToFloat]) {
      destination.emitAttribute("flags", "p")
    }
    if (getRoleDiagnostic != null) {
      destination.emitAttribute("diag", getRoleDiagnostic.save())
    }
    getBaseExpression.export(destination)
    destination.endElement()
  }

}
