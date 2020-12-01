package org.orbeon.saxon.pattern

import java.util.function.IntPredicate

import org.orbeon.saxon.model.{SchemaType, Type, TypeHierarchy, UType}
import org.orbeon.saxon.om._
import org.orbeon.saxon.trace.ExpressionPresenter
import org.orbeon.saxon.tree.tiny.NodeVectorTree
import org.orbeon.saxon.z.{IntSet, IntSingletonSet}

import scala.beans.BeanProperty


class NameTest private (
  @BeanProperty val nodeKind       : Int,
  private var uri                  : String,
  private var localName            : String,
  @BeanProperty val namePool       : NamePool,
  @BeanProperty val fingerPrintInt : Int
) extends NodeTest
     with QNameTest {

  private val uType: UType = UType.fromTypeCode(nodeKind)

  def this(
    nodeKind  : Int,
    uri       : String,
    localName : String,
    namePool  : NamePool
  ) =
    this(
      nodeKind,
      uri,
      localName,
      namePool,
      namePool.allocateFingerprint(uri, localName) & NamePool.FP_MASK
    )

  def this(nodeKind: Int, nameCode: Int, namePool: NamePool) =
    this(nodeKind, null, null, namePool, nameCode & NamePool.FP_MASK)

  def this(nodeKind: Int, name: NodeName, pool: NamePool) =
    this(nodeKind, name.getURI, name.getLocalPart, pool, name.obtainFingerprint(pool))

  def getUType: UType = uType

  override def matches(nodeKind: Int,
                       name: NodeName,
                       annotation: SchemaType): Boolean = {

    println(s"xxx matches $nodeKind / $name")

    if (nodeKind != this.nodeKind)
      return false
    if (name.hasFingerprint) {
      name.getFingerprint == this.fingerPrintInt
    } else {
      computeUriAndLocal()
      name.hasURI(uri) && name.getLocalPart == localName
    }
  }

  override def getMatcher(tree: NodeVectorTree): IntPredicate = {
    val nodeKindArray = tree.getNodeKindArray
    val nameCodeArray = tree.getNameCodeArray
    nodeNr => (nameCodeArray(nodeNr) & 0xfffff) == fingerPrintInt && (nodeKindArray(nodeNr) & 0x0f) == nodeKind
  }

  override def test(node: NodeInfo): Boolean = {
    println(s"xxx test $node")

    if (node.getNodeKind != nodeKind)
      return false
    if (node.hasFingerprint) {
      node.getFingerprint == fingerPrintInt
    } else {
      computeUriAndLocal()
      localName == node.getLocalPart && uri == node.getURI
    }
  }

  private def computeUriAndLocal(): Unit =
    if (uri == null || localName == null) {
      val name = namePool.getUnprefixedQName(fingerPrintInt)
      uri = name.getURI
      localName = name.getLocalPart
    }

  def matches(qname: StructuredQName): Boolean = {
    computeUriAndLocal()
    qname.getLocalPart == localName && qname.hasURI(uri)
  }

  def getDefaultPriority: Double = 0.0

  override def getMatchingNodeName: StructuredQName = {
    computeUriAndLocal()
    new StructuredQName("", uri, localName)
  }

  override def getPrimitiveType: Int = nodeKind

  override def getRequiredNodeNames: Option[IntSet] =
    Some(new IntSingletonSet(fingerPrintInt))

  def getNamespaceURI: String = {
    computeUriAndLocal()
    uri
  }

  def getLocalPart: String = {
    computeUriAndLocal()
    localName
  }

  override def toString: String =
    nodeKind match {
      case Type.ELEMENT                => "element(" + namePool.getEQName(fingerPrintInt) + ")"
      case Type.ATTRIBUTE              => "attribute(" + namePool.getEQName(fingerPrintInt) + ")"
      case Type.PROCESSING_INSTRUCTION => "processing-instruction(" + namePool.getLocalName(fingerPrintInt) + ')'
      case Type.NAMESPACE              => "namespace-node(" + namePool.getLocalName(fingerPrintInt) + ')'
      case _                           => namePool.getEQName(fingerPrintInt)
    }

  override def hashCode: Int = nodeKind << 20 ^ fingerPrintInt

  override def equals(other: Any): Boolean =
    other.isInstanceOf[NameTest] && other
      .asInstanceOf[NameTest]
      .namePool == namePool &&
      other.asInstanceOf[NameTest].nodeKind == nodeKind &&
      other.asInstanceOf[NameTest].fingerPrintInt == fingerPrintInt

  override def getFullAlphaCode: String =
    getBasicAlphaCode + " n" + getMatchingNodeName.getEQName

  override def exportQNameTest: String = getMatchingNodeName.getEQName

  def generateJavaScriptNameTest(targetVersion: Int): String = {
    computeUriAndLocal()
    "q.uri==='" + ExpressionPresenter.jsEscape(uri) + "'&&q.local==='" +
      localName +
      "'"
  }

  override def explainMismatch(item: Item, th: TypeHierarchy): Option[String] = {
    val explanation = super.explainMismatch(item, th)
    if (explanation.isDefined)
      return explanation
    Some("The node has the wrong name")
  }

  override def toShortString: String = nodeKind match {
    case Type.ELEMENT =>
      if (getNamespaceURI.isEmpty)
        namePool.getLocalName(getFingerprint)
      else
        toString
    case Type.ATTRIBUTE =>
      "@" +
        (
          if (getNamespaceURI.isEmpty)
            namePool.getLocalName(getFingerprint)
          else
            toString
        )
    case _ => toString
  }
}
