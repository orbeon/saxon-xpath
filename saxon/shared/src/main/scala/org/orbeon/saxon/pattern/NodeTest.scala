package org.orbeon.saxon.pattern

import java.util.function.{IntPredicate, Predicate}

import org.orbeon.saxon.expr.StaticProperty
import org.orbeon.saxon.model.PrimitiveUType._
import org.orbeon.saxon.model._
import org.orbeon.saxon.om._
import org.orbeon.saxon.tree.tiny.NodeVectorTree
import org.orbeon.saxon.value.SequenceType
import org.orbeon.saxon.z.{IntSet, IntUniversalSet}


abstract class NodeTest
  extends Predicate[NodeInfo]
    with ItemType.WithSequenceTypeCache {

  private var _one: SequenceType = _
  private var _oneOrMore: SequenceType = _
  private var _zeroOrOne: SequenceType = _
  private var _zeroOrMore: SequenceType = _

  def getGenre: Genre.Genre = Genre.NODE

  def getDefaultPriority: Double

  def matches(item: Item, th: TypeHierarchy): Boolean =
    item.isInstanceOf[NodeInfo] && test(item.asInstanceOf[NodeInfo])

  def getPrimitiveItemType: ItemType = {
    val p = getPrimitiveType
    if (p == Type.NODE)
      AnyNodeTest.getInstance
    else
      NodeKindTest.makeNodeKindTest(p)
  }

  def getPrimitiveType: Int = Type.NODE
  def getFingerprint: Int = -1
  def getMatchingNodeName: StructuredQName = null

  override def getBasicAlphaCode: String = getPrimitiveType match {
    case Type.NODE                   => "N"
    case Type.ELEMENT                => "NE"
    case Type.ATTRIBUTE              => "NA"
    case Type.TEXT                   => "NT"
    case Type.COMMENT                => "NC"
    case Type.PROCESSING_INSTRUCTION => "NP"
    case Type.DOCUMENT               => "ND"
    case Type.NAMESPACE              => "NN"
    case _                           => "*"
  }

  def isAtomicType: Boolean = false
  def isPlainType: Boolean = false
  def getAtomizedItemType: AtomicType = BuiltInAtomicType.ANY_ATOMIC
  def isAtomizable(th: TypeHierarchy): Boolean = true

  def getMatcher(tree: NodeVectorTree): IntPredicate =
    nodeNr => test(tree.getNode(nodeNr))

  def matches(nodeKind: Int, name: NodeName, annotation: SchemaType): Boolean

  def test(node: NodeInfo): Boolean =
    matches(node.getNodeKind, NameOfNode.makeName(node), node.getSchemaType)

  def getContentType: SchemaType = {
    val m = getUType.decompose
    val it = m.iterator
    if (m.size == 1 && it.hasNext) {
      val p = it.next()
      p match {
        case DOCUMENT  => AnyType.getInstance
        case ELEMENT   => AnyType.getInstance
        case ATTRIBUTE => AnySimpleType
        case COMMENT   => BuiltInAtomicType.STRING
        case TEXT      => BuiltInAtomicType.UNTYPED_ATOMIC
        case PI        => BuiltInAtomicType.STRING
        case NAMESPACE => BuiltInAtomicType.STRING
      }
    }
    AnyType.getInstance
  }

  def getRequiredNodeNames: Option[IntSet] =
    Some(IntUniversalSet.getInstance)

  def isNillable: Boolean = true

  def copy(): NodeTest = this

  def one: SequenceType = {
    if (_one == null)
      _one = new SequenceType(this, StaticProperty.EXACTLY_ONE)
    _one
  }

  def zeroOrOne: SequenceType = {
    if (_zeroOrOne == null)
      _zeroOrOne = new SequenceType(this, StaticProperty.ALLOWS_ZERO_OR_ONE)
    _zeroOrOne
  }

  def oneOrMore: SequenceType = {
    if (_oneOrMore == null)
      _oneOrMore = new SequenceType(this, StaticProperty.ALLOWS_ONE_OR_MORE)
    _oneOrMore
  }

  def zeroOrMore: SequenceType = {
    if (_zeroOrMore == null)
      _zeroOrMore = new SequenceType(this, StaticProperty.ALLOWS_ZERO_OR_MORE)
    _zeroOrMore
  }

  override def explainMismatch(item: Item, th: TypeHierarchy): Option[String] =
    if (item.isInstanceOf[NodeInfo]) {
      val actualKind = UType.getUType(item)
      if (! getUType.overlaps(actualKind))
        Some("The supplied value is " + actualKind.toStringWithIndefiniteArticle)
      else
        None
    } else {
      Some("The supplied value is " + item.getGenre.getDescription)
    }

  def toShortString: String = toString
}
