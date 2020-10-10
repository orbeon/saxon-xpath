package org.orbeon.saxon.ma.arrays

import java.util.Optional

import org.orbeon.saxon.expr.{Expression, StaticProperty}
import org.orbeon.saxon.expr.parser.RoleDiagnostic
import org.orbeon.saxon.ma.arrays.ArrayItemType._
import org.orbeon.saxon.model.Affinity.Affinity
import org.orbeon.saxon.model._
import org.orbeon.saxon.om.Genre.Genre
import org.orbeon.saxon.om.{Genre, GroundedValue, Item}
import org.orbeon.saxon.trans.{Err, XPathException}
import org.orbeon.saxon.tree.util.FastStringBuffer
import org.orbeon.saxon.value.SequenceType

import scala.beans.BeanProperty

object ArrayItemType {

  val ANY_ARRAY_TYPE: ArrayItemType = new ArrayItemType(
    SequenceType.ANY_SEQUENCE)

  val SINGLE_ARRAY: SequenceType = SequenceType.makeSequenceType(
    ArrayItemType.ANY_ARRAY_TYPE,
    StaticProperty.EXACTLY_ONE)

}

class ArrayItemType(@BeanProperty var memberType: SequenceType)
  extends AnyFunctionType {

  override def getGenre: Genre = Genre.ARRAY

  override def isMapType: Boolean = false

  override def isArrayType: Boolean = true

  override def getBasicAlphaCode: String = "FA"

  override def isAtomizable(th: TypeHierarchy): Boolean = true

  override def getAtomizedItemType: PlainType =
    memberType.getPrimaryType.getAtomizedItemType

  def getArity: Int = 1

  override def getArgumentTypes: Array[SequenceType] =
    Array(BuiltInAtomicType.INTEGER.one)

  override def getDefaultPriority: Double =
    memberType.getPrimaryType.getNormalizedDefaultPriority

  override def matches(item: Item, th: TypeHierarchy): Boolean =
    if (! item.isInstanceOf[ArrayItem])
      false
    else if (this == ANY_ARRAY_TYPE)
      true
    else
      item.asInstanceOf[ArrayItem].members().forall(memberType.matches(_, th))

  override def getResultType: SequenceType = memberType

  override def toString: String = makeString((seqType: SequenceType) => SequenceType.toString)

  private def makeString(show: Function1[SequenceType, String]): String =
    if (this == ANY_ARRAY_TYPE) {
      "array(*)"
    } else {
      val sb = new FastStringBuffer(100)
      sb.append("array(")
      sb.append(show.apply(memberType))
      sb.append(")")
      sb.toString
    }

  override def toExportString: String =
    makeString((seqType: SequenceType) => toExportString)

  override def equals(other: Any): Boolean =
    other match {
      case o: ArrayItemType if this eq o => true
      case o: ArrayItemType => memberType == o.memberType
      case _ => false
    }

  override def hashCode: Int = memberType.hashCode

  override def relationship(other: FunctionItemType, th: TypeHierarchy): Affinity =
    if (other == AnyFunctionType) {
      Affinity.SUBSUMED_BY
    } else if (equals(other)) {
      Affinity.SAME_TYPE
    } else if (other == ArrayItemType.ANY_ARRAY_TYPE) {
      Affinity.SUBSUMED_BY
    } else if (other.isMapType) {
      Affinity.DISJOINT
    } else if (other.isInstanceOf[ArrayItemType]) {
      val f2: ArrayItemType = other.asInstanceOf[ArrayItemType]
      val rel: Affinity =
        th.sequenceTypeRelationship(memberType, f2.memberType)
      if (rel == Affinity.DISJOINT) Affinity.OVERLAPS else rel
    } else {
      var rel: Affinity =
        new SpecificFunctionType(getArgumentTypes, getResultType)
          .relationship(other, th)
      if (rel == Affinity.SUBSUMES || rel == Affinity.SAME_TYPE) {
        rel = Affinity.OVERLAPS
      }
      rel
    }

  override def makeFunctionSequenceCoercer(exp: Expression,
                                           role: RoleDiagnostic): Expression =
    new SpecificFunctionType(getArgumentTypes, getResultType)
      .makeFunctionSequenceCoercer(exp, role)

  override def explainMismatch(item: Item,
                               th: TypeHierarchy): Option[String] = {
    if (item.isInstanceOf[ArrayItem]) {
      for (i <- 0 until item.asInstanceOf[ArrayItem].arrayLength()) {
        try {
          val member: GroundedValue = item.asInstanceOf[ArrayItem].get(i)
          if (!memberType.matches(member, th)) {
            var s: String = "The " + RoleDiagnostic.ordinal(i + 1) + " member of the supplied array {" +
              Err.depictSequence(member) +
              "} does not match the required member type " +
              memberType
            val more = memberType.explainMismatch(member, th)
            if (more.isDefined) {
              s = s + ". " + more.get
            }
            Optional.of(s)
          }
        } catch {
          case e: XPathException => None

        }
      }
    }
    None
  }

}
