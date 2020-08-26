package net.sf.saxon.model

import net.sf.saxon.expr.Expression
import net.sf.saxon.expr.MappingFunction
import net.sf.saxon.expr.MappingIterator
import net.sf.saxon.lib.ConversionRules
import net.sf.saxon.lib.NamespaceConstant
import net.sf.saxon.om._
import net.sf.saxon.trans.XPathException
import net.sf.saxon.tree.iter.AtomicIterator
import net.sf.saxon.value.AtomicValue
import net.sf.saxon.value.StringValue
import net.sf.saxon.value.Whitespace
import net.sf.saxon.model.SchemaComponent.ValidationStatus.{VALIDATED, ValidationStatus}
import BuiltInListType._

import scala.beans.BeanProperty

object BuiltInListType {

  var ENTITIES: BuiltInListType =
    makeListType(NamespaceConstant.SCHEMA, "ENTITIES")

  var IDREFS: BuiltInListType =
    makeListType(NamespaceConstant.SCHEMA, "IDREFS")

  var NMTOKENS: BuiltInListType =
    makeListType(NamespaceConstant.SCHEMA, "NMTOKENS")

  var ANY_URIS: BuiltInListType = makeListType(
    NamespaceConstant.SCHEMA_INSTANCE,
    "anonymous_schemaLocationType")

  private def makeListType(namespace: String, lname: String): BuiltInListType = {
    val t: BuiltInListType = new BuiltInListType(
      StandardNames.getFingerprint(namespace, lname))
    BuiltInType.register(t.getFingerprint, t)
    t
  }

  private class ListTypeMappingFunction extends MappingFunction {

    var resolver: NamespaceResolver = _

    var atomicType: AtomicType = _

    var rules: ConversionRules = _

    def map(item: Item): AtomicIterator[AtomicValue] =
      atomicType
        .getTypedValue(item.getStringValueCS, resolver, rules)
        .iterate().asInstanceOf[AtomicIterator[AtomicValue]]

  }

}

class BuiltInListType(@BeanProperty var fingerprint: Int) extends ListType {

  def isBuiltInType(): Boolean = true

  def getSystemId(): String = null

  def getRedefinitionLevel(): Int = 0

  def getWhitespaceAction(): Int = Whitespace.COLLAPSE

  @BeanProperty
  var itemType: BuiltInAtomicType = _

  fingerprint match {
    case StandardNames.XS_ENTITIES => itemType = BuiltInAtomicType.ENTITY
    case StandardNames.XS_IDREFS => itemType = BuiltInAtomicType.IDREF
    case StandardNames.XS_NMTOKENS => itemType = BuiltInAtomicType.NMTOKEN
    case StandardNames.XSI_SCHEMA_LOCATION_TYPE =>
      itemType = BuiltInAtomicType.ANY_URI

  }

  def getValidationStatus(): ValidationStatus = VALIDATED

  def getBaseType(): SchemaType = AnySimpleType

  def isAtomicType(): Boolean = false

  def isIdType(): Boolean = false

  def isIdRefType(): Boolean = fingerprint == StandardNames.XS_IDREFS

  def isListType(): Boolean = true

  def isUnionType(): Boolean = false

  def isAnonymousType(): Boolean = false

  def getBuiltInBaseType(): SchemaType = this

  def isNamespaceSensitive(): Boolean = false

  def getName(): String = StandardNames.getLocalName(fingerprint)

  def getTargetNamespace(): String = NamespaceConstant.SCHEMA

  def getEQName(): String = "Q{" + NamespaceConstant.SCHEMA + "}" + getName

  def getDisplayName(): String = StandardNames.getDisplayName(fingerprint)

  def isComplexType(): Boolean = false

  def isSimpleType(): Boolean = true

  def getBlock(): Int = 0

  def getKnownBaseType(): SchemaType = AnySimpleType

  def getDerivationMethod(): Int = DERIVATION_LIST

  def allowsDerivation(derivation: Int): Boolean = true

  def getFinalProhibitions(): Int = 0

  def atomize(node: NodeInfo): AtomicSequence =
    getTypedValue(node.getStringValue,
      node.getAllNamespaces,
      node.getConfiguration.getConversionRules)

  def isSameType(other: SchemaType): Boolean =
    other.getFingerprint == getFingerprint

  def getDescription(): String = getDisplayName

  def checkTypeDerivationIsOK(`type`: SchemaType, block: Int): Unit = ()

  def getLocalName(): String = StandardNames.getLocalName(fingerprint)

  def getStructuredQName(): StructuredQName =
    new StructuredQName("xs", NamespaceConstant.SCHEMA, getLocalName)

  def applyWhitespaceNormalization(value: String): String =
    Whitespace.collapseWhitespace(value).toString

  def analyzeContentExpression(expression: Expression, kind: Int): Unit = {
    BuiltInAtomicType.analyzeContentExpression(this, expression, kind)
  }

  def validateContent(value: CharSequence,
                      nsResolver: NamespaceResolver,
                      rules: ConversionRules): ValidationFailure = {
    val base: SimpleType = getItemType
    val iter: Whitespace.Tokenizer = new Whitespace.Tokenizer(value)
    var found: Boolean = false
    var `val`: StringValue = null
    while ((`val` = iter.next()) != null) {
      found = true
      val v: ValidationFailure =
        base.validateContent(`val`.getStringValue, nsResolver, rules)
      if (v != null) {
        v
      }
    }
    if (!found) {
      new ValidationFailure(
        "The built-in list type " + StandardNames.getDisplayName(fingerprint) +
          " does not allow a zero-length list")
    }
    null
  }

  def getTypedValue(value: CharSequence,
                    resolver: NamespaceResolver,
                    rules: ConversionRules): AtomicSequence = {
    val iter: Whitespace.Tokenizer = new Whitespace.Tokenizer(value)
    val map: ListTypeMappingFunction = new ListTypeMappingFunction()
    map.resolver = resolver
    map.atomicType = getItemType.asInstanceOf[AtomicType]
    map.rules = rules
    new AtomicArray(new MappingIterator(iter, map))
  }

  def preprocess(input: CharSequence): CharSequence = input

  def postprocess(input: CharSequence): CharSequence = input

}
