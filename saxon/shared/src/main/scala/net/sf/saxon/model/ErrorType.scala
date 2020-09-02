package net.sf.saxon.model

import java.util.{Collections, Optional}

import net.sf.saxon.expr.Expression
import net.sf.saxon.lib.{ConversionRules, NamespaceConstant}
import net.sf.saxon.model.SchemaComponent.ValidationStatus.{VALIDATED, ValidationStatus}
import net.sf.saxon.om._
import net.sf.saxon.pattern.NodeTest
import net.sf.saxon.trans.XPathException
import net.sf.saxon.value.{AtomicValue, SequenceType, UntypedAtomicValue, Whitespace}

import scala.jdk.CollectionConverters._


object ErrorType
  extends NodeTest
    with AtomicType
    with UnionType
    with PlainType {

  def getUType: UType = UType.VOID

  def getName: String = "error"

  def getTargetNamespace(): String = NamespaceConstant.SCHEMA

  def getEQName(): String = "Q{" + NamespaceConstant.SCHEMA + "}error"

  def containsListType(): Boolean = false

  def getPlainMemberTypes(): Iterable[_ <: PlainType] =
    Collections.emptySet().asScala

  def isBuiltInType(): Boolean = true

  def getRedefinitionLevel(): Int = 0

  def getSystemId: String = null

  def getValidationStatus(): ValidationStatus = VALIDATED

  def getBaseType(): SchemaType = AnySimpleType

  def getKnownBaseType: SchemaType = getBaseType

  def isComplexType(): Boolean = false

  def isSimpleType(): Boolean = true

  override def getFingerprint(): Int = StandardNames.XS_ERROR

  override def getMatchingNodeName(): StructuredQName =
    StandardNames.getStructuredQName(StandardNames.XS_ERROR)

  def getTypeName(): StructuredQName =
    new StructuredQName("xs", NamespaceConstant.SCHEMA, "error")

  override def getDescription: String = "xs:error"

  def getDisplayName: String = "xs:error"

  def isSameType(other: SchemaType): Boolean = other eq ErrorType

  def atomize(node: NodeInfo): AtomicSequence =
    new UntypedAtomicValue(node.getStringValueCS)

  def checkTypeDerivationIsOK(`type`: SchemaType, block: Int): Unit = {
    if (`type` == this || `type` == AnySimpleType) {
      return
    }
    throw new SchemaException(
      "Type xs:error is not validly derived from " + `type`.getDescription)
  }

  override def isAtomicType: Boolean = false

  def isIdType(): Boolean = false

  def isIdRefType(): Boolean = false

  def isAnonymousType(): Boolean = false

  def isListType(): Boolean = false

  def isUnionType(): Boolean = true

  def getBuiltInBaseType(): SchemaType = this

  def getTypedValue(value: CharSequence,
                    resolver: NamespaceResolver,
                    rules: ConversionRules): AtomicSequence =
    throw new ValidationFailure("Cast to xs:error always fails")
      .makeException()

  def getStringConverter(rules: ConversionRules): StringConverter = null

  def validateContent(value: CharSequence,
                      nsResolver: NamespaceResolver,
                      rules: ConversionRules): ValidationFailure =
    new ValidationFailure("No content is ever valid against the type xs:error")

  def isNamespaceSensitive(): Boolean = false

  def getBlock(): Int = 0

  def getDerivationMethod(): Int = DERIVATION_RESTRICTION

  def allowsDerivation(derivation: Int): Boolean = false

  def getFinalProhibitions(): Int = DERIVATION_EXTENSION | DERIVATION_RESTRICTION |
    DERIVATION_LIST |
    DERIVATION_UNION

  def getWhitespaceAction(): Int = Whitespace.COLLAPSE

  def analyzeContentExpression(expression: Expression, kind: Int): Unit = {
    throw new XPathException(
      "No expression can ever return a value of type xs:error")
  }

  def preprocess(input: CharSequence): CharSequence = input

  def postprocess(input: CharSequence): CharSequence = input

  override def isPlainType: Boolean = true

  override def matches(item: Item, th: TypeHierarchy): Boolean = false

  override def matches(nodeKind: Int,
                       name: NodeName,
                       annotation: SchemaType): Boolean = false

  override def getPrimitiveItemType: AtomicType = this

  override def getPrimitiveType: Int = Type.ITEM

  override def getDefaultPriority: Double = -1000

  override def getAtomizedItemType: AtomicType = BuiltInAtomicType.UNTYPED_ATOMIC

  override def isAtomizable(th: TypeHierarchy): Boolean = false

  def getResultTypeOfCast(): SequenceType = SequenceType.OPTIONAL_ITEM

  override def toExportString: String = toString

  override def toString: String = "xs:error"

  def validate(primValue: AtomicValue,
               lexicalValue: CharSequence,
               rules: ConversionRules): ValidationFailure =
    new ValidationFailure("No value is valid against type xs:error")

  def isOrdered(optimistic: Boolean): Boolean = false

  def isAbstract(): Boolean = true

  def isPrimitiveType(): Boolean = false

  override def getStructuredQName: StructuredQName =
    new StructuredQName("xs", NamespaceConstant.SCHEMA, "error")

  override def checkAgainstFacets(value: AtomicValue,
                                  rules: ConversionRules): ValidationFailure =
    null

  override def explainMismatch(item: Item,
                               th: TypeHierarchy): Optional[String] =
    Optional.of("Evaluation of the supplied expression will always fail")

}
