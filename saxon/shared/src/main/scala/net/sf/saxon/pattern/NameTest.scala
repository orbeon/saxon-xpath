package net.sf.saxon.pattern

import net.sf.saxon.model.SchemaType

import net.sf.saxon.model.Type

import net.sf.saxon.model.TypeHierarchy

import net.sf.saxon.model.UType

import net.sf.saxon.om._

import net.sf.saxon.trace.ExpressionPresenter

import net.sf.saxon.tree.tiny.NodeVectorTree

import net.sf.saxon.z.IntSet

import net.sf.saxon.z.IntSingletonSet

import java.util.Optional

import java.util.function.IntPredicate

import scala.beans.{BeanProperty}

class NameTest(@BeanProperty var nodeKind: Int,
               private var uri: String,
               private var localName: String,
               @BeanProperty var namePool: NamePool)
  extends NodeTest
    with QNameTest {

  @BeanProperty
  var fingerPrintInt: Int = namePool.allocateFingerprint(uri, localName) & NamePool.FP_MASK

  private var uType: UType = UType.fromTypeCode(nodeKind)

  def this(nodeKind: Int, nameCode: Int, namePool: NamePool) = {
    this(nodeKind, "", "", namePool)
    this.nodeKind = nodeKind
    this.fingerPrintInt = nameCode & NamePool.FP_MASK
    this.namePool = namePool
    this.uType = UType.fromTypeCode(nodeKind)
  }

  def this(nodeKind: Int, name: NodeName, pool: NamePool) = {
    this(nodeKind, "", "", pool)
    this.uri = name.getURI
    this.localName = name.getLocalPart
    this.nodeKind = nodeKind
    this.fingerPrintInt = name.obtainFingerprint(pool)
    this.namePool = pool
    this.uType = UType.fromTypeCode(nodeKind)
  }

  def getUType(): UType = uType

  override def matches(nodeKind: Int,
                       name: NodeName,
                       annotation: SchemaType): Boolean = {
    if (nodeKind != this.nodeKind) {
      return false
    }
    if (name.hasFingerprint) {
      name.getFingerprint == this.fingerPrintInt
    } else {
      computeUriAndLocal()
      name.hasURI(uri) && name.getLocalPart == localName
    }
  }

  override def getMatcher(tree: NodeVectorTree): IntPredicate = {
    val nodeKindArray: Array[Byte] = tree.getNodeKindArray
    val nameCodeArray: Array[Int] = tree.getNameCodeArray
    (nodeNr) =>
      (nameCodeArray(nodeNr) & 0xfffff) == fingerPrintInt && (nodeKindArray(
        nodeNr) & 0x0f) == nodeKind
  }

  override def test(node: NodeInfo): Boolean = {
    if (node.getNodeKind != nodeKind) {
      return false
    }
    if (node.hasFingerprint) {
      node.getFingerprint == fingerPrintInt
    } else {
      computeUriAndLocal()
      localName == node.getLocalPart && uri == node.getURI
    }
  }

  private def computeUriAndLocal(): Unit = {
    if (uri == null || localName == null) {
      val name: StructuredQName = namePool.getUnprefixedQName(fingerPrintInt)
      uri = name.getURI
      localName = name.getLocalPart
    }
  }

  def matches(qname: StructuredQName): Boolean = {
    computeUriAndLocal()
    qname.getLocalPart == localName && qname.hasURI(uri)
  }

  def getDefaultPriority: Double = 0.0

  override def getMatchingNodeName(): StructuredQName = {
    computeUriAndLocal()
    new StructuredQName("", uri, localName)
  }

  override def getPrimitiveType: Int = nodeKind

  override def getRequiredNodeNames(): Optional[IntSet] =
    Optional.of(new IntSingletonSet(fingerPrintInt))

  def getNamespaceURI: String = {
    computeUriAndLocal()
    uri
  }

  def getLocalPart: String = {
    computeUriAndLocal()
    localName
  }

  override def toString: String = {
    nodeKind match {
      case Type.ELEMENT => "element(" + namePool.getEQName(fingerPrintInt) + ")"
      case Type.ATTRIBUTE =>
        "attribute(" + namePool.getEQName(fingerPrintInt) + ")"
      case Type.PROCESSING_INSTRUCTION =>
        "processing-instruction(" + namePool.getLocalName(fingerPrintInt) +
          ')'
      case Type.NAMESPACE =>
        "namespace-node(" + namePool.getLocalName(fingerPrintInt) +
          ')'

    }
    namePool.getEQName(fingerPrintInt)
  }

  override def hashCode(): Int = nodeKind << 20 ^ fingerPrintInt

  override def equals(other: Any): Boolean =
    other.isInstanceOf[NameTest] && other
      .asInstanceOf[NameTest]
      .namePool == namePool &&
      other.asInstanceOf[NameTest].nodeKind == nodeKind &&
      other.asInstanceOf[NameTest].fingerPrintInt == fingerPrintInt

  override def getFullAlphaCode(): String =
    getBasicAlphaCode + " n" + getMatchingNodeName.getEQName

  override def exportQNameTest: String = getMatchingNodeName.getEQName

  def generateJavaScriptNameTest(targetVersion: Int): String = {
    computeUriAndLocal()
    "q.uri==='" + ExpressionPresenter.jsEscape(uri) + "'&&q.local==='" +
      localName +
      "'"
  }

  override def explainMismatch(item: Item,
                               th: TypeHierarchy): Optional[String] = {
    val explanation: Optional[String] = super.explainMismatch(item, th)
    if (explanation.isPresent) {
      return explanation
    }
    Optional.of("The node has the wrong name")
  }

  override def toShortString: String = nodeKind match {
    case Type.ELEMENT =>
      if (getNamespaceURI.isEmpty) namePool.getLocalName(getFingerprint)
      else toString
    case Type.ATTRIBUTE =>
      "@" +
        (if (getNamespaceURI.isEmpty) namePool.getLocalName(getFingerprint)
        else toString)
    case _ => toString

  }

}
