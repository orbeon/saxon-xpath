package org.orbeon.saxon.pattern

import java.util.function.IntPredicate

import org.orbeon.saxon.functions.Nilled_1
import org.orbeon.saxon.model._
import org.orbeon.saxon.om.{Item, NodeInfo, NodeName}
import org.orbeon.saxon.tree.tiny.{NodeVectorTree, TinyTree}
import org.orbeon.saxon.utils.Configuration

import scala.beans.{BeanProperty, BooleanBeanProperty}

class ContentTypeTest(nodeKind: Int,
                      @BeanProperty var schemaType: SchemaType,
                      private var config: Configuration,
                      @BooleanBeanProperty  var nillableBool: Boolean)
  extends NodeTest {

  private val kind: Int = nodeKind

  def getUType: UType =
    if (kind == Type.ELEMENT) UType.ELEMENT else UType.ATTRIBUTE

  def getNodeKind: Int = kind

  override def matches(nodeKind: Int,
                       name: NodeName,
                       annotation: SchemaType): Boolean =
    kind == nodeKind && matchesAnnotation(annotation)

  override def getMatcher(tree: NodeVectorTree): IntPredicate = {
    val nodeKindArray: Array[Byte] = tree.getNodeKindArray
    nodeNr =>
      (nodeKindArray(nodeNr) & 0x0f) == kind &&
        matchesAnnotation(tree.asInstanceOf[TinyTree].getSchemaType(nodeNr)) &&
        (nillableBool || !tree.asInstanceOf[TinyTree].isNilled(nodeNr))
  }

  override def test(node: NodeInfo): Boolean =
    node.getNodeKind == kind && matchesAnnotation(node.getSchemaType) &&
      (nillableBool || !Nilled_1.isNilled(node))

  private def matchesAnnotation(annotation: SchemaType): Boolean = {
    if (annotation == null) {
      return false
    }
    if (schemaType == AnyType.getInstance) {
      return true
    }
    if (annotation == schemaType) {
      return true
    }
    val r: Affinity.Affinity =
      config.getTypeHierarchy.schemaTypeRelationship(annotation, schemaType)
    r == Affinity.SAME_TYPE || r == Affinity.SUBSUMED_BY
  }

  def getDefaultPriority: Double = 0

  override def getPrimitiveType: Int = kind

  override def getContentType: SchemaType = schemaType

  override def getAtomizedItemType: AtomicType = {
    val `type`: SchemaType = schemaType
    try
      if (`type`.isAtomicType) {
        return `type`.asInstanceOf[AtomicType]
      } else `type` match {
        case listType: ListType =>
          val mem = listType.getItemType
          if (mem.isAtomicType)
            return mem.asInstanceOf[AtomicType]
        case complexType: ComplexType if complexType
          .isSimpleContent =>
          val ctype = complexType.getSimpleContentType
          assert(ctype != null)
          if (ctype.isAtomicType) {
            return ctype.asInstanceOf[AtomicType]
          } else ctype match {
            case listType: ListType =>
              val mem = listType.getItemType
              if (mem.isAtomicType)
                return mem.asInstanceOf[AtomicType]
            case _ =>
          }
        case _ =>
      }
    catch {
      case _: MissingComponentException =>
        return BuiltInAtomicType.ANY_ATOMIC
    }
    BuiltInAtomicType.ANY_ATOMIC
  }

  override def isAtomizable(th: TypeHierarchy): Boolean =
    !(schemaType.isComplexType &&
      schemaType
        .asInstanceOf[ComplexType]
        .getVariety == ComplexType.VARIETY_ELEMENT_ONLY)

  override def toString: String =
    (if (kind == Type.ELEMENT) "element(*, " else "attribute(*, ") +
      schemaType.getEQName +
      ')'

  override def toExportString: String =
    (if (kind == Type.ELEMENT) "element(*, " else "attribute(*, ") +
      schemaType.getNearestNamedType.getEQName +
      ')'

  override def hashCode: Int = kind << 20 ^ schemaType.hashCode

  override def equals(other: Any): Boolean =
    other.isInstanceOf[ContentTypeTest] && other
      .asInstanceOf[ContentTypeTest]
      .kind == kind &&
      other.asInstanceOf[ContentTypeTest].schemaType == schemaType &&
      other.asInstanceOf[ContentTypeTest].nillableBool == nillableBool

  override def explainMismatch(item: Item,
                               th: TypeHierarchy): Option[String] = {

    val explanation = super.explainMismatch(item, th)
    if (explanation.isDefined)
      return explanation

    val node = item.asInstanceOf[NodeInfo]
    if (!matchesAnnotation(item.asInstanceOf[NodeInfo].getSchemaType)) {
      if (node.getSchemaType == Untyped.getInstance)
        return Some("The supplied node has not been schema-validated")
      if (node.getSchemaType == BuiltInAtomicType.UNTYPED_ATOMIC)
        return Some("The supplied node has not been schema-validated")
      return Some(
        "The supplied node has the wrong type annotation (" +
          node.getSchemaType.getDescription +
          ")")
    }
    if (Nilled_1.isNilled(node) && !nillableBool)
      return Some("The supplied node has xsi:nil='true', which the required type does not allow")
    None
  }

}