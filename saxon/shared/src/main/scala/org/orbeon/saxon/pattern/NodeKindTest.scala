package org.orbeon.saxon.pattern

import org.orbeon.saxon.model._

import org.orbeon.saxon.om.Item

import org.orbeon.saxon.om.NodeInfo

import org.orbeon.saxon.om.NodeName

import org.orbeon.saxon.tree.tiny.NodeVectorTree

import java.util.Optional

import java.util.function.IntPredicate


object NodeKindTest {

  val DOCUMENT: NodeKindTest = new NodeKindTest(Type.DOCUMENT)

  val ELEMENT: NodeKindTest = new NodeKindTest(Type.ELEMENT)

  val ATTRIBUTE: NodeKindTest = new NodeKindTest(Type.ATTRIBUTE)

  val TEXT: NodeKindTest = new NodeKindTest(Type.TEXT)

  val COMMENT: NodeKindTest = new NodeKindTest(Type.COMMENT)

  val PROCESSING_INSTRUCTION: NodeKindTest = new NodeKindTest(
    Type.PROCESSING_INSTRUCTION)

  val NAMESPACE: NodeKindTest = new NodeKindTest(Type.NAMESPACE)

  def makeNodeKindTest(kind: Int): NodeTest = kind match {
    case Type.DOCUMENT => DOCUMENT
    case Type.ELEMENT => ELEMENT
    case Type.ATTRIBUTE => ATTRIBUTE
    case Type.COMMENT => COMMENT
    case Type.TEXT => TEXT
    case Type.PROCESSING_INSTRUCTION => PROCESSING_INSTRUCTION
    case Type.NAMESPACE => NAMESPACE
    case Type.NODE => AnyNodeTest.getInstance
    case _ =>
      throw new IllegalArgumentException(
        "Unknown node kind " + kind + " in NodeKindTest")

  }

  def toString(kind: Int): String = kind match {
    case Type.DOCUMENT => "document-node()"
    case Type.ELEMENT => "element()"
    case Type.ATTRIBUTE => "attribute()"
    case Type.COMMENT => "comment()"
    case Type.TEXT => "text()"
    case Type.PROCESSING_INSTRUCTION => "processing-instruction()"
    case Type.NAMESPACE => "namespace-node()"
    case _ => "** error **"

  }

  def nodeKindName(kind: Int): String = kind match {
    case Type.DOCUMENT => "document"
    case Type.ELEMENT => "element"
    case Type.ATTRIBUTE => "attribute"
    case Type.COMMENT => "comment"
    case Type.TEXT => "text"
    case Type.PROCESSING_INSTRUCTION => "processing-instruction"
    case Type.NAMESPACE => "namespace"
    case _ => "** error **"

  }

}

class NodeKindTest(var kind: Int) extends NodeTest {

  var uType: UType = UType.fromTypeCode(kind)

  def getNodeKind: Int = kind

  def getUType: UType = uType

  override def matches(item: Item, th: TypeHierarchy): Boolean =
    item.isInstanceOf[NodeInfo] && kind == item
      .asInstanceOf[NodeInfo]
      .getNodeKind

  override def matches(nodeKind: Int,
                       name: NodeName,
                       annotation: SchemaType): Boolean = kind == nodeKind

  override def getMatcher(tree: NodeVectorTree): IntPredicate = {
    val nodeKindArray: Array[Byte] = tree.getNodeKindArray
    if (kind == Type.TEXT) { (nodeNr) => {
      val k: Int = nodeKindArray(nodeNr)
      k == Type.TEXT || k == Type.WHITESPACE_TEXT
    }
    } else { (nodeNr) =>
      (nodeKindArray(nodeNr) & 0x0f) == kind
    }
  }

  override def test(node: NodeInfo): Boolean = node.getNodeKind == kind

  def getDefaultPriority: Double = -0.5

  override def getPrimitiveType: Int = kind

  override def getContentType(): SchemaType = kind match {
    case Type.DOCUMENT => AnyType.getInstance
    case Type.ELEMENT => AnyType.getInstance
    case Type.ATTRIBUTE => AnySimpleType
    case Type.COMMENT => BuiltInAtomicType.STRING
    case Type.TEXT => BuiltInAtomicType.UNTYPED_ATOMIC
    case Type.PROCESSING_INSTRUCTION => BuiltInAtomicType.STRING
    case Type.NAMESPACE => BuiltInAtomicType.STRING
    case _ => throw new AssertionError("Unknown node kind")

  }

  override def getAtomizedItemType: AtomicType = kind match {
    case Type.DOCUMENT => BuiltInAtomicType.UNTYPED_ATOMIC
    case Type.ELEMENT => BuiltInAtomicType.ANY_ATOMIC
    case Type.ATTRIBUTE => BuiltInAtomicType.ANY_ATOMIC
    case Type.COMMENT => BuiltInAtomicType.STRING
    case Type.TEXT => BuiltInAtomicType.UNTYPED_ATOMIC
    case Type.PROCESSING_INSTRUCTION => BuiltInAtomicType.STRING
    case Type.NAMESPACE => BuiltInAtomicType.STRING
    case _ => throw new AssertionError("Unknown node kind")

  }

  override def toString: String = NodeKindTest.toString(kind)

  override def hashCode: Int = kind

  override def equals(other: Any): Boolean = other match {
    case other: NodeKindTest => other.kind == kind
    case _ => false

  }

  override def explainMismatch(item: Item,
                               th: TypeHierarchy): Optional[String] = {
    val explanation: Optional[String] = super.explainMismatch(item, th)
    if (explanation.isPresent) {
      return explanation
    }
    if (item.isInstanceOf[NodeInfo]) {
      val actualKind: UType = UType.getUType(item)
      if (!getUType.overlaps(actualKind)) {
        Optional.of(
          "The supplied value is " + actualKind
            .toStringWithIndefiniteArticle)
      } else {
        Optional.empty()
      }
    } else {
      Optional.of("The supplied value is " + item.getGenre.getDescription)
    }
  }

  override def toShortString: String = getNodeKind match {
    case Type.ELEMENT => "*"
    case Type.ATTRIBUTE => "@*"
    case Type.DOCUMENT => "/"
    case _ => toString

  }

}
