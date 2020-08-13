package net.sf.saxon.pattern

import net.sf.saxon.expr.parser.Token

import net.sf.saxon.ma.map.DictionaryMap

import net.sf.saxon.model._

import net.sf.saxon.om.Item

import net.sf.saxon.om.NodeInfo

import net.sf.saxon.om.NodeName

import net.sf.saxon.om.StructuredQName

import net.sf.saxon.tree.tiny.NodeVectorTree

import net.sf.saxon.value.StringValue

import net.sf.saxon.z.IntExceptPredicate

import net.sf.saxon.z.IntSet

import java.util.Optional

import java.util.function.IntPredicate

import AnyType._
import AnySimpleType._

import scala.beans.{BeanProperty}

object CombinedNodeTest {

  def getContentTypeForAlphaCode(nodetest1: NameTest,
                                 nodetest2: ContentTypeTest): String =
    if (nodetest1.getNodeKind == Type.ELEMENT) {
      if (nodetest2.getContentType == Untyped.getInstance && nodetest2.isNillable) {
        null
      } else {
        val contentType: SchemaType = nodetest2.getContentType
        contentType.getEQName
      }
    } else if (nodetest1.getNodeKind == Type.ATTRIBUTE) {
      if (nodetest2.getContentType == BuiltInAtomicType.UNTYPED_ATOMIC) {
        null
      } else {
        val contentType: SchemaType = nodetest2.getContentType
        contentType.getEQName
      }
    } else {
      throw new IllegalStateException()
    }

}

class CombinedNodeTest(private var nodetest1: NodeTest,
                       @BeanProperty var operator: Int,
                       private var nodetest2: NodeTest)
  extends NodeTest {

  def getUType(): UType = {
    val u1: UType = nodetest1.getUType
    val u2: UType = nodetest2.getUType
    operator match {
      case Token.UNION => u1.union(u2)
      case Token.INTERSECT => u1.intersection(u2)
      case Token.EXCEPT => u1
      case _ =>
        throw new IllegalArgumentException(
          "Unknown operator in Combined Node Test")

    }
  }

  override def matches(nodeKind: Int,
                       name: NodeName,
                       annotation: SchemaType): Boolean = operator match {
    case Token.UNION =>
      nodetest1 == null || nodetest2 == null || nodetest1
        .matches(nodeKind, name, annotation) ||
        nodetest2.matches(nodeKind, name, annotation)
    case Token.INTERSECT =>
      (nodetest1 == null || nodetest1.matches(nodeKind, name, annotation)) &&
        (nodetest2 == null || nodetest2.matches(nodeKind, name, annotation))
    case Token.EXCEPT =>
      (nodetest1 == null || nodetest1.matches(nodeKind, name, annotation)) &&
        !(nodetest2 == null || nodetest2.matches(nodeKind, name, annotation))
    case _ =>
      throw new IllegalArgumentException(
        "Unknown operator in Combined Node Test")

  }

  override def getMatcher(tree: NodeVectorTree): IntPredicate =
    operator match {
      case Token.UNION =>
        nodetest1.getMatcher(tree).or(nodetest2.getMatcher(tree))
      case Token.INTERSECT =>
        nodetest1.getMatcher(tree).and(nodetest2.getMatcher(tree))
      case Token.EXCEPT =>
        new IntExceptPredicate(nodetest1.getMatcher(tree),
          nodetest2.getMatcher(tree))
      case _ =>
        throw new IllegalArgumentException(
          "Unknown operator in Combined Node Test")

    }

  override def test(node: NodeInfo): Boolean = operator match {
    case Token.UNION =>
      nodetest1 == null || nodetest2 == null || nodetest1.test(node) ||
        nodetest2.test(node)
    case Token.INTERSECT =>
      (nodetest1 == null || nodetest1
        .test(node)) && (nodetest2 == null || nodetest2.test(node))
    case Token.EXCEPT =>
      (nodetest1 == null || nodetest1
        .test(node)) && !(nodetest2 == null || nodetest2.test(node))
    case _ =>
      throw new IllegalArgumentException(
        "Unknown operator in Combined Node Test")

  }

  override def toString(): String = makeString(false)

  private def makeString(forExport: Boolean): String =
    if (nodetest1.isInstanceOf[NameTest] && operator == Token.INTERSECT) {
      val kind: Int = nodetest1.getPrimitiveType
      val skind: String =
        if (kind == Type.ELEMENT) "element(" else "attribute("
      var content: String = ""
      if (nodetest2.isInstanceOf[ContentTypeTest]) {
        var schemaType: SchemaType =
          nodetest2.asInstanceOf[ContentTypeTest].getSchemaType
        if (forExport) {
          schemaType = schemaType.getNearestNamedType
        }
        content = ", " + schemaType.getEQName
        if (nodetest2.isNillable) {
          content += "?"
        }
      }
      val name: String = nodetest1.getMatchingNodeName.getEQName
      skind + name + content + ')'
    } else {
      val nt1: String = if (nodetest1 == null) "item()" else nodetest1.toString
      val nt2: String = if (nodetest2 == null) "item()" else nodetest2.toString
      '(' + nt1 + ' ' + Token.tokens(operator) + ' ' + nt2 +
        ')'
    }

  override def toExportString(): String = makeString(true)

  def getContentTypeForAlphaCode(): String =
    if (nodetest1.isInstanceOf[NameTest] && operator == Token.INTERSECT &&
      nodetest2.isInstanceOf[ContentTypeTest]) {
      CombinedNodeTest.getContentTypeForAlphaCode(nodetest1.asInstanceOf[NameTest],
        nodetest2.asInstanceOf[ContentTypeTest])
    } else if (nodetest2
      .isInstanceOf[NameTest] && operator == Token.INTERSECT &&
      nodetest1.isInstanceOf[ContentTypeTest]) {
      CombinedNodeTest.getContentTypeForAlphaCode(nodetest2.asInstanceOf[NameTest],
        nodetest1.asInstanceOf[ContentTypeTest])
    } else {
      null
    }

  def addTypeDetails(map: DictionaryMap): Unit = {
    if (nodetest1.isInstanceOf[NameTest] && operator == Token.INTERSECT) {
      map.initialPut("n",
        new StringValue(nodetest1.getMatchingNodeName.getEQName))
      if (nodetest2.isInstanceOf[ContentTypeTest]) {
        val schemaType: SchemaType =
          nodetest2.asInstanceOf[ContentTypeTest].getSchemaType
        if (schemaType != Untyped.getInstance && schemaType != BuiltInAtomicType.UNTYPED_ATOMIC) {
          map.initialPut(
            "c",
            new StringValue(
              schemaType.getEQName + (if (nodetest2.isNillable) "?" else "")))
        }
      }
    }
  }

  override def getPrimitiveType(): Int = {
    val mask: UType = getUType
    if (mask == UType.ELEMENT) {
      Type.ELEMENT
    }
    if (mask == UType.ATTRIBUTE) {
      Type.ATTRIBUTE
    }
    if (mask == UType.DOCUMENT) {
      Type.DOCUMENT
    }
    Type.NODE
  }

  override def getRequiredNodeNames(): Optional[IntSet] = {
    val os1: Optional[IntSet] = nodetest1.getRequiredNodeNames
    val os2: Optional[IntSet] = nodetest2.getRequiredNodeNames
    if (os1.isPresent && os2.isPresent) {
      val s1: IntSet = os1.get
      val s2: IntSet = os2.get
      operator match {
        case Token.UNION => Optional.of(s1.union(s2))
        case Token.INTERSECT => Optional.of(s1.intersect(s2))
        case Token.EXCEPT => Optional.of(s1.except(s2))
        case _ => throw new IllegalStateException()

      }
    } else {
      Optional.empty()
    }
  }

  override def getContentType(): SchemaType = {
    val type1: SchemaType = nodetest1.getContentType
    val type2: SchemaType = nodetest2.getContentType
    if (type1.isSameType(type2)) {
      return type1
    }
    if (operator == Token.INTERSECT) {
      if (type2.isInstanceOf[AnyType] ||
        ((type2 eq AnySimpleType) && type1.isSimpleType)) {
        return type1
      }
      if (type1.isInstanceOf[AnyType] ||
        ((type1 eq AnySimpleType) && type2.isSimpleType)) {
        return type2
      }
    }
    AnyType.getInstance
  }

  override def getAtomizedItemType(): AtomicType = {
    val type1: AtomicType = nodetest1.getAtomizedItemType
    val type2: AtomicType = nodetest2.getAtomizedItemType
    if (type1.isSameType(type2)) {
      return type1
    }
    if (operator == Token.INTERSECT) {
      if (type2 == BuiltInAtomicType.ANY_ATOMIC) {
        return type1
      }
      if (type1 == BuiltInAtomicType.ANY_ATOMIC) {
        return type2
      }
    }
    BuiltInAtomicType.ANY_ATOMIC
  }

  override def isAtomizable(th: TypeHierarchy): Boolean = operator match {
    case Token.UNION =>
      nodetest1.isAtomizable(th) || nodetest2.isAtomizable(th)
    case Token.INTERSECT =>
      nodetest1.isAtomizable(th) && nodetest2.isAtomizable(th)
    case Token.EXCEPT => nodetest1.isAtomizable(th)
    case _ => true

  }

  override def getFingerprint(): Int = {
    val fp1: Int = nodetest1.getFingerprint
    val fp2: Int = nodetest2.getFingerprint
    if (fp1 == fp2) {
      return fp1
    }
    if (fp2 == -1 && operator == Token.INTERSECT) {
      return fp1
    }
    if (fp1 == -1 && operator == Token.INTERSECT) {
      return fp2
    }
    -1
  }

  override def getMatchingNodeName(): StructuredQName = {
    val n1: StructuredQName = nodetest1.getMatchingNodeName
    val n2: StructuredQName = nodetest2.getMatchingNodeName
    if (n1 != null && n1 == n2) {
      return n1
    }
    if (n1 == null && operator == Token.INTERSECT) {
      return n2
    }
    if (n2 == null && operator == Token.INTERSECT) {
      return n1
    }
    null
  }

  override def isNillable(): Boolean = nodetest1.isNillable && nodetest2.isNillable

  override def hashCode(): Int = nodetest1.hashCode ^ nodetest2.hashCode

  override def equals(other: Any): Boolean =
    other.isInstanceOf[CombinedNodeTest] &&
      other.asInstanceOf[CombinedNodeTest].nodetest1 == nodetest1 &&
      other.asInstanceOf[CombinedNodeTest].nodetest2 == nodetest2 &&
      other.asInstanceOf[CombinedNodeTest].operator == operator

  def getDefaultPriority(): Double =
    if (operator == Token.UNION) {
      nodetest1.getDefaultPriority
    } else {
      if (nodetest1.isInstanceOf[NameTest]) 0.25 else 0.125
    }

  def getComponentNodeTests(): Array[NodeTest] = Array(nodetest1, nodetest2)

  def getOperand(which: Int): NodeTest =
    if (which == 0) nodetest1 else nodetest2

  override def explainMismatch(item: Item,
                               th: TypeHierarchy): Optional[String] = {
    val explanation: Optional[String] = super.explainMismatch(item, th)
    if (explanation.isPresent) {
      return explanation
    }
    if (operator == Token.INTERSECT) {
      if (!nodetest1.test(item.asInstanceOf[NodeInfo])) {
        nodetest1.explainMismatch(item, th)
      } else if (!nodetest2.test(item.asInstanceOf[NodeInfo])) {
        nodetest2.explainMismatch(item, th)
      }
    }
    Optional.empty()
  }

}
