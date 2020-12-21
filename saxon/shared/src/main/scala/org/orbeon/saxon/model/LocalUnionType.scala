package org.orbeon.saxon.model

import org.orbeon.saxon.expr.StaticProperty

import org.orbeon.saxon.lib.ConversionRules

import org.orbeon.saxon.lib.NamespaceConstant

import org.orbeon.saxon.om._

import org.orbeon.saxon.trans.Err

import org.orbeon.saxon.tree.util.FastStringBuffer

import org.orbeon.saxon.value.AtomicValue

import org.orbeon.saxon.value.SequenceType

import java.util.List

import scala.beans.{BeanProperty}

//import scala.collection.compat._
import scala.jdk.CollectionConverters._

class LocalUnionType(@BeanProperty var memberTypes: List[AtomicType])
  extends PlainType
    with UnionType {

  override def getGenre: Genre.Genre = Genre.ATOMIC

  def getTypeName(): StructuredQName =
    new StructuredQName("", NamespaceConstant.ANONYMOUS, "U" + hashCode)

  def isAtomicType: Boolean = false

  def containsListType(): Boolean = false

  def isPlainType: Boolean = true

  override def isTrueItemType(): Boolean = true

  def getResultTypeOfCast(): SequenceType =
    SequenceType.makeSequenceType(this, StaticProperty.ALLOWS_ZERO_OR_ONE)

  def isIdType: Boolean = memberTypes.stream().anyMatch((atmType: AtomicType) => atmType.isIdType)

  def isIdRefType: Boolean = memberTypes.stream().anyMatch((atmType: AtomicType) => atmType.isIdType)

  def isBuiltInType: Boolean = false

  def isListType: Boolean = false

  def isUnionType: Boolean = true

  def getUType: UType = {
    var u: UType = UType.VOID
    for (at <- memberTypes.asScala) {
      u = u.union(at.getUType)
    }
    u
  }

  override def getBasicAlphaCode: String = "A"

  def isNamespaceSensitive(): Boolean =
    memberTypes.stream().anyMatch((atmType: AtomicType) => atmType.isNamespaceSensitive)

  def validateContent(value: CharSequence,
                      nsResolver: NamespaceResolver,
                      rules: ConversionRules): ValidationFailure = {
    for (at <- memberTypes.asScala) {
      val err: ValidationFailure = at.validateContent(value, nsResolver, rules)
      if (err == null) {
        null
      }
    }
    new ValidationFailure(
      "Value " + Err
        .wrap(value, Err.VALUE) + " does not match any member of union type " +
        toString)
  }

  override def checkAgainstFacets(value: AtomicValue,
                                  rules: ConversionRules): ValidationFailure =
    null

  def getTypedValue(value: CharSequence,
                    resolver: NamespaceResolver,
                    rules: ConversionRules): AtomicValue = {
    for (memType <- memberTypes.asScala) {
      val converter: StringConverter = rules.makeStringConverter(memType)
      converter.setNamespaceResolver(resolver)
      val outcome: ConversionResult = converter.convertString(value)
      if (outcome.isInstanceOf[AtomicValue]) {
        outcome.asInstanceOf[AtomicValue]
      }
    }
    val ve: ValidationFailure = new ValidationFailure(
      "Value " + Err
        .wrap(value, Err.VALUE) + " does not match any member of union type " +
        toString)
    throw ve.makeException()
  }

  def matches(item: Item, th: TypeHierarchy): Boolean =
    if (item.isInstanceOf[AtomicValue]) {
      memberTypes.stream().anyMatch((at) => at.matches(item, th))
    } else {
      false
    }

  def getPrimitiveItemType: AtomicType = BuiltInAtomicType.ANY_ATOMIC

  def getPrimitiveType: Int = StandardNames.XS_ANY_ATOMIC_TYPE

  def getAtomizedItemType: PlainType = this

  def isAtomizable(th: TypeHierarchy): Boolean = true

  def getPlainMemberTypes(): Iterable[AtomicType] = memberTypes.asScala

  override def getDefaultPriority: Double = {
    var result: Double = 1
    for (t <- memberTypes.asScala) {
      result *= t.getDefaultPriority
    }
    result
  }

  override def toString: String = {
    val fsb = new FastStringBuffer(FastStringBuffer.C256)
    fsb.append("union(")
    for (at <- memberTypes.asScala) {
      val member: String = at.getDisplayName
      fsb.append(member)
      fsb.append(", ")
    }
    fsb.setLength(fsb.length - 2)
    fsb.append(")")
    fsb.toString
  }

  override def toExportString: String = {
    val fsb = new FastStringBuffer(FastStringBuffer.C256)
    fsb.append("union(")
    for (at <- memberTypes.asScala) {
      fsb.append(at.toExportString)
      fsb.append(", ")
    }
    fsb.setLength(fsb.length - 2)
    fsb.append(")")
    fsb.toString
  }

}
