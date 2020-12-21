package org.orbeon.saxon.model

import org.orbeon.saxon.expr.Expression
import org.orbeon.saxon.lib.ConversionRules
import org.orbeon.saxon.lib.NamespaceConstant
import org.orbeon.saxon.om._
import org.orbeon.saxon.value._
import java.util.Arrays
//import scala.collection.compat._
import scala.jdk.CollectionConverters._
import org.orbeon.saxon.model.SchemaComponent.ValidationStatus.{VALIDATED, ValidationStatus}

object NumericType {

  private var THE_INSTANCE: NumericType = _

  def getInstance: NumericType = classOf[NumericType].synchronized {
    if (THE_INSTANCE == null) {
      THE_INSTANCE = new NumericType()
      BuiltInType.register(StandardNames.XS_NUMERIC, THE_INSTANCE)
    }
    THE_INSTANCE
  }

  def isNumericType(`type`: ItemType): Boolean =
    `type`
      .isInstanceOf[AtomicType] && `type` != BuiltInAtomicType.ANY_ATOMIC &&
      `type`.getUType.overlaps(getInstance.getUType)

}

class NumericType private()
  extends LocalUnionType(
    Arrays.asList(BuiltInAtomicType.DOUBLE,
      BuiltInAtomicType.FLOAT,
      BuiltInAtomicType.DECIMAL))
    with SimpleType {

  override def getTypeName(): StructuredQName =
    new StructuredQName("xs", NamespaceConstant.SCHEMA, "numeric")

  override def getGenre: Genre.Genre = Genre.ATOMIC

  override def getBasicAlphaCode: String = "A"

  override def containsListType(): Boolean = false

  override def getPlainMemberTypes(): Iterable[AtomicType] = synchronized {
    getMemberTypes.asScala
  }

  override def getResultTypeOfCast(): SequenceType = SequenceType.ATOMIC_SEQUENCE

  override def isPlainType: Boolean = true

  override def getDefaultPriority: Double = 0.125

  override def matches(item: Item, th: TypeHierarchy): Boolean =
    item.isInstanceOf[NumericValue]

  override def getPrimitiveItemType: AtomicType = BuiltInAtomicType.ANY_ATOMIC

  override def getPrimitiveType: Int = BuiltInAtomicType.ANY_ATOMIC.getFingerprint

  override def getUType: UType = UType.NUMERIC

  override def getAtomizedItemType: PlainType = this

  override def isAtomizable(th: TypeHierarchy): Boolean = true

  override def isAtomicType: Boolean = false

  override def isListType(): Boolean = false

  override def isUnionType(): Boolean = true

  override def isBuiltInType(): Boolean = true

  def getBuiltInBaseType(): SchemaType = AnySimpleType

  override def getTypedValue(value: CharSequence,
                             resolver: NamespaceResolver,
                             rules: ConversionRules): DoubleValue = {
    val d: Double = StringToDouble.getInstance.stringToNumber(value)
    new DoubleValue(d)
  }

  override def validateContent(value: CharSequence,
                               nsResolver: NamespaceResolver,
                               rules: ConversionRules): ValidationFailure =
    try {
      StringToDouble.getInstance.stringToNumber(value)
      null
    } catch {
      case e: NumberFormatException => new ValidationFailure(e.getMessage)

    }

  override def checkAgainstFacets(value: AtomicValue,
                                  rules: ConversionRules): ValidationFailure =
    null

  override def isNamespaceSensitive(): Boolean = false

  def getWhitespaceAction(): Int = Whitespace.COLLAPSE

  def preprocess(input: CharSequence): CharSequence = input

  def postprocess(input: CharSequence): CharSequence = input

  def getName: String = "numeric"

  def getTargetNamespace(): String = NamespaceConstant.SCHEMA

  def getFingerprint: Int = StandardNames.XS_NUMERIC

  def getDisplayName: String = "xs:numeric"

  def getEQName(): String = "Q(" + NamespaceConstant.SCHEMA + "}numeric"

  override def getStructuredQName: StructuredQName =
    new StructuredQName("xs", NamespaceConstant.SCHEMA, "numeric")

  def isComplexType(): Boolean = false

  def isSimpleType(): Boolean = true

  def isAnonymousType(): Boolean = false

  def getBlock(): Int = 0

  def getBaseType(): SchemaType = AnySimpleType

  def getDerivationMethod(): Int = DERIVATION_RESTRICTION

  def getFinalProhibitions(): Int = 0

  def allowsDerivation(derivation: Int): Boolean = true

  def analyzeContentExpression(expression: Expression, kind: Int): Unit = {
    BuiltInAtomicType.analyzeContentExpression(this, expression, kind)
  }

  def atomize(node: NodeInfo): AtomicSequence =
    throw new UnsupportedOperationException()

  def isSameType(other: SchemaType): Boolean = other.isInstanceOf[NumericType]

  override def getDescription: String = "xs:numeric"

  def getSystemId: String = null

  override def isIdType(): Boolean = false

  override def isIdRefType(): Boolean = false

  def getValidationStatus(): ValidationStatus = VALIDATED

  def getRedefinitionLevel(): Int = 0

  override def toExportString: String = toString

  override def toString: String = "xs:numeric"

  def checkTypeDerivationIsOK(base: SchemaType, block: Int): Unit = ()

}
