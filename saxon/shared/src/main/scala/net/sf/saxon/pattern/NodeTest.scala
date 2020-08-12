package net.sf.saxon.pattern

import net.sf.saxon.expr.StaticProperty

import net.sf.saxon.model._

import net.sf.saxon.om._

import net.sf.saxon.tree.tiny.NodeVectorTree

import net.sf.saxon.value.SequenceType

import net.sf.saxon.z.IntSet

import net.sf.saxon.z.IntUniversalSet

import java.util.Iterator

import java.util.Optional

import java.util.Set

import java.util.function.IntPredicate

import java.util.function.Predicate

import PrimitiveUType._

abstract class NodeTest
  extends Predicate[NodeInfo]
    with ItemType.WithSequenceTypeCache {

  private var _one: SequenceType = _

  private var _oneOrMore: SequenceType = _

  private var _zeroOrOne: SequenceType = _

  private var _zeroOrMore: SequenceType = _

  override def getGenre(): Genre.Genre = Genre.NODE

  def getDefaultPriority(): Double

  def matches(item: Item, th: TypeHierarchy): Boolean =
    item.isInstanceOf[NodeInfo] && test(item.asInstanceOf[NodeInfo])

  def getPrimitiveItemType(): ItemType = {
    val p: Int = getPrimitiveType
    if (p == Type.NODE) {
      AnyNodeTest.getInstance
    } else {
      NodeKindTest.makeNodeKindTest(p)
    }
  }

  def getPrimitiveType(): Int = Type.NODE

  def getFingerprint(): Int = -1

  def getMatchingNodeName(): StructuredQName = null

  override def getBasicAlphaCode(): String = getPrimitiveType match {
    case Type.NODE => "N"
    case Type.ELEMENT => "NE"
    case Type.ATTRIBUTE => "NA"
    case Type.TEXT => "NT"
    case Type.COMMENT => "NC"
    case Type.PROCESSING_INSTRUCTION => "NP"
    case Type.DOCUMENT => "ND"
    case Type.NAMESPACE => "NN"
    case _ => "*"

  }

  def isAtomicType(): Boolean = false

  def isPlainType(): Boolean = false

  def getAtomizedItemType(): AtomicType = BuiltInAtomicType.ANY_ATOMIC

  def isAtomizable(th: TypeHierarchy): Boolean = true

  def getMatcher(tree: NodeVectorTree): IntPredicate =
    (nodeNr) => test(tree.getNode(nodeNr))

  def matches(nodeKind: Int, name: NodeName, annotation: SchemaType): Boolean

  def test(node: NodeInfo): Boolean =
    matches(node.getNodeKind, NameOfNode.makeName(node), node.getSchemaType)

  def getContentType(): SchemaType = {
    val m: Set[PrimitiveUType.PrimitiveUType] = getUType.decompose()
    val it: Iterator[PrimitiveUType.PrimitiveUType] = m.iterator()
    if (m.size == 1 && it.hasNext) {
      val p: PrimitiveUType.PrimitiveUType = it.next()
      p match {
        case DOCUMENT => AnyType.getInstance
        case ELEMENT => AnyType.getInstance
        case ATTRIBUTE => AnySimpleType.getInstance
        case COMMENT => BuiltInAtomicType.STRING
        case TEXT => BuiltInAtomicType.UNTYPED_ATOMIC
        case PI => BuiltInAtomicType.STRING
        case NAMESPACE => BuiltInAtomicType.STRING

      }
    }
    AnyType.getInstance
  }

  def getRequiredNodeNames(): Optional[IntSet] =
    Optional.of(IntUniversalSet.getInstance)

  def isNillable(): Boolean = true

  def copy(): NodeTest = this

  def one(): SequenceType = {
    if (_one == null) {
      _one = new SequenceType(this, StaticProperty.EXACTLY_ONE)
    }
    _one
  }

  def zeroOrOne(): SequenceType = {
    if (_zeroOrOne == null) {
      _zeroOrOne = new SequenceType(this, StaticProperty.ALLOWS_ZERO_OR_ONE)
    }
    _zeroOrOne
  }

  def oneOrMore(): SequenceType = {
    if (_oneOrMore == null) {
      _oneOrMore = new SequenceType(this, StaticProperty.ALLOWS_ONE_OR_MORE)
    }
    _oneOrMore
  }

  def zeroOrMore(): SequenceType = {
    if (_zeroOrMore == null) {
      _zeroOrMore = new SequenceType(this, StaticProperty.ALLOWS_ZERO_OR_MORE)
    }
    _zeroOrMore
  }

  override def explainMismatch(item: Item,
                               th: TypeHierarchy): Optional[String] =
    if (item.isInstanceOf[NodeInfo]) {
      val actualKind: UType = UType.getUType(item)
      if (!getUType.overlaps(actualKind)) {
        Optional.of(
          "The supplied value is " + actualKind
            .toStringWithIndefiniteArticle())
      }
      Optional.empty()
    } else {
      Optional.of("The supplied value is " + item.getGenre.getDescription)
    }

  def toShortString(): String = toString

}
