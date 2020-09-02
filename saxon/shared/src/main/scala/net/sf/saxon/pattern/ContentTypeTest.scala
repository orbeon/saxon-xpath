package net.sf.saxon.pattern

import net.sf.saxon.functions.Nilled_1
import net.sf.saxon.model._
import net.sf.saxon.om.Item
import net.sf.saxon.om.NodeInfo
import net.sf.saxon.om.NodeName
import net.sf.saxon.tree.tiny.NodeVectorTree
import net.sf.saxon.tree.tiny.TinyTree
import java.util.Optional
import java.util.function.IntPredicate

import net.sf.saxon.utils.Configuration

import scala.beans.{BeanProperty, BooleanBeanProperty}

class ContentTypeTest(nodeKind: Int,
                      @BeanProperty var schemaType: SchemaType,
                      private var config: Configuration,
                      @BooleanBeanProperty  var nillableBool: Boolean)
  extends NodeTest {

  private var kind: Int = nodeKind

  def getUType: UType =
    if (kind == Type.ELEMENT) UType.ELEMENT else UType.ATTRIBUTE

  def getNodeKind: Int = kind

  override def matches(nodeKind: Int,
                       name: NodeName,
                       annotation: SchemaType): Boolean =
    kind == nodeKind && matchesAnnotation(annotation)

  override def getMatcher(tree: NodeVectorTree): IntPredicate = {
    val nodeKindArray: Array[Byte] = tree.getNodeKindArray
    (nodeNr) =>
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

  override def getContentType(): SchemaType = schemaType

  override def getAtomizedItemType: AtomicType = {
    val `type`: SchemaType = schemaType
    try if (`type`.isAtomicType) {
      `type`.asInstanceOf[AtomicType]
    } else if (`type`.isInstanceOf[ListType]) {
      val mem: SimpleType = `type`.asInstanceOf[ListType].getItemType
      if (mem.isAtomicType) {
        mem.asInstanceOf[AtomicType]
      }
    } else if (`type`.isInstanceOf[ComplexType] && `type`
      .asInstanceOf[ComplexType]
      .isSimpleContent) {
      val ctype: SimpleType =
        `type`.asInstanceOf[ComplexType].getSimpleContentType
      assert(ctype != null)
      if (ctype.isAtomicType) {
        ctype.asInstanceOf[AtomicType]
      } else if (ctype.isInstanceOf[ListType]) {
        val mem: SimpleType = ctype.asInstanceOf[ListType].getItemType
        if (mem.isAtomicType) {
          mem.asInstanceOf[AtomicType]
        }
      }
    } catch {
      case e: MissingComponentException => BuiltInAtomicType.ANY_ATOMIC

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
                               th: TypeHierarchy): Optional[String] = {
    val explanation: Optional[String] = super.explainMismatch(item, th)
    if (explanation.isPresent) {
      return explanation
    }
    val node: NodeInfo = item.asInstanceOf[NodeInfo]
    if (!matchesAnnotation(item.asInstanceOf[NodeInfo].getSchemaType)) {
      if (node.getSchemaType == Untyped.getInstance) {
        Optional.of("The supplied node has not been schema-validated")
      }
      if (node.getSchemaType == BuiltInAtomicType.UNTYPED_ATOMIC) {
        Optional.of("The supplied node has not been schema-validated")
      }
      Optional.of(
        "The supplied node has the wrong type annotation (" +
          node.getSchemaType.getDescription +
          ")")
    }
    if (Nilled_1.isNilled(node) && !nillableBool) {
      Optional.of(
        "The supplied node has xsi:nil='true', which the required type does not allow")
    }
    Optional.empty()
  }

}