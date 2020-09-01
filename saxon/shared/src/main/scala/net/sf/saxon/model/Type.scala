////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
package net.sf.saxon.model

import net.sf.saxon.utils.Configuration
import net.sf.saxon.expr.parser.Token
import net.sf.saxon.ma.arrays.ArrayItem
import net.sf.saxon.ma.arrays.ArrayItemType
import net.sf.saxon.ma.map.MapItem
import net.sf.saxon.ma.map.MapType
import net.sf.saxon.model.Affinity.Affinity
import net.sf.saxon.model.BuiltInAtomicType.{ANY_ATOMIC, ANY_URI, DAY_TIME_DURATION, DECIMAL, DURATION, INTEGER, STRING, UNTYPED_ATOMIC, YEAR_MONTH_DURATION}
import net.sf.saxon.om.Function
import net.sf.saxon.om.Item
import net.sf.saxon.om.NodeInfo
import net.sf.saxon.om.StandardNames
import net.sf.saxon.pattern._
import net.sf.saxon.value._
import scala.util.control.Breaks._


object Type {

  val ELEMENT: Short = 1

  /**
   * Item type representing an attribute node - attribute()
   */
  /**
   * Item type representing an attribute node - attribute()
   */
  val ATTRIBUTE: Short = 2

  /**
   * Item type representing a text node - text()
   */
  /**
   * Item type representing a text node - text()
   */
  val TEXT: Short = 3

  /**
   * Item type representing a text node stored in the tiny tree as compressed whitespace
   */
  /**
   * Item type representing a text node stored in the tiny tree as compressed whitespace
   */
  val WHITESPACE_TEXT: Short = 4

  /**
   * Item type representing a processing-instruction node
   */
  /**
   * Item type representing a processing-instruction node
   */
  val PROCESSING_INSTRUCTION: Short = 7

  /**
   * Item type representing a comment node
   */
  /**
   * Item type representing a comment node
   */
  val COMMENT: Short = 8

  /**
   * Item type representing a document node
   */
  /**
   * Item type representing a document node
   */
  val DOCUMENT: Short = 9

  /**
   * Item type representing a namespace node
   */
  /**
   * Item type representing a namespace node
   */
  val NAMESPACE: Short = 13

  /**
   * Dummy node kind used in the tiny tree to mark the end of the tree
   */
  /**
   * Dummy node kind used in the tiny tree to mark the end of the tree
   */
  val STOPPER: Short = 11

  /**
   * Dummy node kind used in the tiny tree to contain a parent pointer
   */
  /**
   * Dummy node kind used in the tiny tree to contain a parent pointer
   */
  val PARENT_POINTER: Short = 12

  // Chosen so kind & 0x0f = Type.ELEMENT
  val TEXTUAL_ELEMENT: Short = 17

  val NODE: Short = 0

  val NODE_TYPE: ItemType = AnyNodeTest.getInstance

  val ITEM: Short = 88

  /*@NotNull*/

  val ITEM_TYPE: ItemType = AnyItemType

  val FUNCTION: Short = 99

  def isNodeType(`type`: ItemType): Boolean = `type`.isInstanceOf[NodeTest]

  /*@NotNull*/

  def getItemType(item: Item, thVar: TypeHierarchy): ItemType = {
    var th: TypeHierarchy = thVar;
    if (item == null) {
      AnyItemType
    } else if (item.isInstanceOf[AtomicValue]) {
      item.asInstanceOf[AtomicValue].getItemType
    } else if (item.isInstanceOf[NodeInfo]) {
      val node: NodeInfo = item.asInstanceOf[NodeInfo]
      if (th == null) {
        th = node.getConfiguration.getTypeHierarchy
      }
      node.getNodeKind match {
        case Type.DOCUMENT =>
          // Need to know whether the document is well-formed and if so what the element type is
          var elementType: ItemType = null
          breakable {
            for (n <- node.children()) {
              val kind: Int = n.getNodeKind
              if (kind == Type.TEXT) {
                elementType = null
                break()
              } else if (kind == Type.ELEMENT) {
                if (elementType != null) {
                  elementType = null
                  break()
                }
                elementType = Type.getItemType(n, th)
              }
            }
          }
          if (elementType == null) {
            NodeKindTest.DOCUMENT
          } else {
            new DocumentNodeTest(elementType.asInstanceOf[NodeTest])
          }
        case Type.ELEMENT =>
          var eltype: SchemaType = node.getSchemaType
          if (eltype == Untyped.getInstance || eltype == AnyType.getInstance) {
            new SameNameTest(node)
          } else {
            new CombinedNodeTest(new SameNameTest(node),
              Token.INTERSECT,
              new ContentTypeTest(Type.ELEMENT,
                eltype,
                node.getConfiguration,
                false))
          }
        case Type.ATTRIBUTE =>
          var attype: SchemaType = node.getSchemaType
          if (attype == UNTYPED_ATOMIC) {
            new SameNameTest(node)
          } else {
            new CombinedNodeTest(new SameNameTest(node),
              Token.INTERSECT,
              new ContentTypeTest(Type.ATTRIBUTE,
                attype,
                node.getConfiguration,
                false))
          }
        case Type.TEXT => NodeKindTest.TEXT
        case Type.COMMENT => NodeKindTest.COMMENT
        case Type.PROCESSING_INSTRUCTION => NodeKindTest.PROCESSING_INSTRUCTION
        case Type.NAMESPACE => NodeKindTest.NAMESPACE
        case _ =>
          throw new IllegalArgumentException(
            "Unknown node kind " + node.getNodeKind)

      }
    } else if (item.isInstanceOf[ExternalObject[_]]) {
      if (th == null) {
        throw new IllegalArgumentException(
          "typeHierarchy is required for an external object")
      }
      item.asInstanceOf[ExternalObject[_]].getItemType(th)
    } else if (item.isInstanceOf[MapItem]) {
      if (th == null) MapType.ANY_MAP_TYPE
      else item.asInstanceOf[MapItem].getItemType(th)
    } else if (item.isInstanceOf[ArrayItem]) {
      if (th == null) ArrayItemType.ANY_ARRAY_TYPE
      else new ArrayItemType(item.asInstanceOf[ArrayItem].getMemberType(th))
    } else {
      //if (item instanceof FunctionItem) {
      item.asInstanceOf[Function].getFunctionItemType
    }
  }

  def displayTypeName(item: Item): String =
    if (item.isInstanceOf[NodeInfo]) {
      val node: NodeInfo = item.asInstanceOf[NodeInfo]
      node.getNodeKind match {
        case DOCUMENT => "document-node()"
        case ELEMENT =>
          var annotation: SchemaType = node.getSchemaType
          "element(" + item.asInstanceOf[NodeInfo].getDisplayName +
            ", " +
            annotation.getDisplayName +
            ')'
        case ATTRIBUTE =>
          var annotation2: SchemaType = node.getSchemaType
          "attribute(" + item.asInstanceOf[NodeInfo].getDisplayName +
            ", " +
            annotation2.getDisplayName +
            ')'
        case TEXT => "text()"
        case COMMENT => "comment()"
        case PROCESSING_INSTRUCTION => "processing-instruction()"
        case NAMESPACE => "namespace()"
        case _ => ""

      }
    } else if (item.isInstanceOf[ExternalObject[_]]) {
      ObjectValue.displayTypeName(item.asInstanceOf[ExternalObject[AnyRef]].getObject)
    } else if (item.isInstanceOf[AtomicValue]) {
      item.asInstanceOf[AtomicValue].getItemType.toString
    } else if (item.isInstanceOf[Function]) {
      "function(*)"
    } else {
      item.getClass.toString
    }

  /*@Nullable*/

  def getBuiltInItemType(namespace: String, localName: String): ItemType = {
    val t: SchemaType = BuiltInType.getSchemaType(
      StandardNames.getFingerprint(namespace, localName))
    if (t.isInstanceOf[ItemType]) {
      t.asInstanceOf[ItemType]
    } else {
      null
    }
  }

  /*@Nullable*/

  def getBuiltInSimpleType(namespace: String, localName: String): SimpleType = {
    val t: SchemaType = BuiltInType.getSchemaType(
      StandardNames.getFingerprint(namespace, localName))
    if (t.isInstanceOf[SimpleType] && t
      .asInstanceOf[SimpleType]
      .isBuiltInType) {
      t.asInstanceOf[SimpleType]
    } else {
      null
    }
  }

  def isSubType(oneVar: AtomicType, two: AtomicType): Boolean = {
    var one = oneVar;
    while (true) {
      if (one.getFingerprint == two.getFingerprint) {
        return true
      }
      val s: SchemaType = one.getBaseType
      if (s.isInstanceOf[AtomicType]) {
        one = s.asInstanceOf[AtomicType]
      } else {
        return false
      }
    }
    return false
  }

  /*@NotNull*/

  def getCommonSuperType(t1: ItemType,
                         t2: ItemType,
                         th: TypeHierarchy): ItemType = {
    if (t1 == t2)
      return t1

    if (t1 eq ErrorType)
      return t2

    if (t2 eq ErrorType)
      return t1

    t1 match {
      case objectType: JavaExternalObjectType if t2.isInstanceOf[JavaExternalObjectType] =>
        val config = objectType.getConfiguration
        val c1 = objectType.getJavaClass
        val c2 = t2.asInstanceOf[JavaExternalObjectType].getJavaClass
        config.getJavaExternalObjectType(leastCommonSuperClass(c1, c2))
      case _ =>
    }
    t1 match {
      case mapType: MapType if t2.isInstanceOf[MapType] =>
        if (t1 == MapType.EMPTY_MAP_TYPE)
          return t2

        if (t2 == MapType.EMPTY_MAP_TYPE)
          return t1

        val keyType = getCommonSuperType(mapType.getKeyType, t2.asInstanceOf[MapType].getKeyType)
        val k =
          keyType match {
            case atomicType: AtomicType => atomicType
            case _ => keyType.getAtomizedItemType.getPrimitiveItemType
          }
        val v = SequenceType.makeSequenceType(
          getCommonSuperType(
            mapType.getValueType.getPrimaryType,
            t2.asInstanceOf[MapType].getValueType.getPrimaryType),
          Cardinality.union(mapType.getValueType.getCardinality,
            t2.asInstanceOf[MapType].getValueType.getCardinality)
        )
        new MapType(k, v)
      case _ =>
    }
    val r = th.relationship(t1, t2)
    if (r == Affinity.SAME_TYPE) {
      t1
    } else if (r == Affinity.SUBSUMED_BY) {
      t2
    } else if (r == Affinity.SUBSUMES) {
      t1
    } else {
      t1.getUType.union(t2.getUType).toItemType()
    }
  }

  /*@NotNull*/

  def getCommonSuperType(t1: ItemType, t2: ItemType): ItemType = {
    if (t1 == t2)
      return t1

    if (t1 eq ErrorType)
      return t2

    if (t2 eq ErrorType)
      return t1

    if (t1 == AnyItemType || t2 == AnyItemType)
      return AnyItemType

    val p1 = t1.getPrimitiveItemType
    val p2 = t2.getPrimitiveItemType
    if (p1 == p2) {
      return p1
    }

    if ((p1 == DECIMAL && p2 == INTEGER) ||
      (p2 == DECIMAL && p1 == INTEGER)) {
      return DECIMAL
    }

    p1 match {
      case atomicType: BuiltInAtomicType if p2.asInstanceOf[BuiltInAtomicType].isNumericType && p2.isInstanceOf[BuiltInAtomicType] && atomicType.isNumericType =>
        return NumericType.getInstance
      case _ =>
    }

    if (t1.isAtomicType && t2.isAtomicType)
      return ANY_ATOMIC

    if (t1.isInstanceOf[NodeTest] && t2.isInstanceOf[NodeTest])
      return AnyNodeTest.getInstance

    t1 match {
      case objectType: JavaExternalObjectType if t2.isInstanceOf[JavaExternalObjectType] =>
        val config = objectType.getConfiguration
        val c1 = objectType.getJavaClass
        val c2 = t2.asInstanceOf[JavaExternalObjectType].getJavaClass
        return config.getJavaExternalObjectType(leastCommonSuperClass(c1, c2))
      case _ =>
    }
    AnyItemType
  }

  def getCommonSuperType(t1: SequenceType, t2: SequenceType): SequenceType =
    if (t1 == t2) {
      t1
    } else {
      SequenceType.makeSequenceType(
        getCommonSuperType(t1.getPrimaryType, t2.getPrimaryType),
        Cardinality.union(t1.getCardinality, t2.getCardinality))
    }

  private def leastCommonSuperClass(class1: Class[_],
                                    class2: Class[_]): Class[_] = {
    if (class1 == class2)
      return class1

    if (class1 == null || class2 == null)
      return null

    if (!class1.isArray && class1.isAssignableFrom(class2))
      return class1

    if (!class2.isArray && class2.isAssignableFrom(class1))
      return class2

    if (class1.isInterface || class2.isInterface)
      return classOf[AnyRef]

    leastCommonSuperClass(class1.getSuperclass, class2.getSuperclass)
  }

  /**
   * Determine whether a given atomic type is a primitive type. The primitive types are
   * the 19 primitive types of XML Schema, plus xs:integer, xs:dayTimeDuration and xs:yearMonthDuration;
   * xs:untypedAtomic; the 7 node kinds; and all supertypes of these (item(), node(), xs:anyAtomicType,
   * xs:numeric, ...)
   *
   * @param fingerprint the item type code to be tested
   * @return true if the type is considered primitive under the above rules
   */
  def isPrimitiveAtomicType(fingerprint: Int): Boolean =
    fingerprint >= 0 &&
      (fingerprint <= StandardNames.XS_INTEGER || fingerprint == StandardNames.XS_NUMERIC ||
        fingerprint == StandardNames.XS_UNTYPED_ATOMIC ||
        fingerprint == StandardNames.XS_ANY_ATOMIC_TYPE ||
        fingerprint == StandardNames.XS_DAY_TIME_DURATION ||
        fingerprint == StandardNames.XS_YEAR_MONTH_DURATION ||
        fingerprint == StandardNames.XS_ANY_SIMPLE_TYPE)

  /**
   * Determine whether this type is a type that corresponds exactly to a UType
   *
   * @param fingerprint the item type code to be tested
   * @return true if the type is considered primitive under the above rules
   */
  def isPrimitiveAtomicUType(fingerprint: Int): Boolean =
    fingerprint >= 0 && fingerprint <= StandardNames.XS_INTEGER

  def isGuaranteedComparable(t1Var: BuiltInAtomicType,
                             t2Var: BuiltInAtomicType,
                             ordered: Boolean): Boolean = {
    var t1 = t1Var
    var t2 = t2Var
    if (t1 == t2) {
      // short cut
      return true
    }
    if (t1.isPrimitiveNumeric) {
      t2.isPrimitiveNumeric
    }
    if (t1 == UNTYPED_ATOMIC || t1 == ANY_URI) {
      t1 = STRING
    }
    if (t2 == UNTYPED_ATOMIC || t2 == ANY_URI) {
      t2 = STRING
    }
    if (!ordered) {
      if (t1 == DAY_TIME_DURATION) {
        t1 = DURATION
      }
      if (t2 == DAY_TIME_DURATION) {
        t2 = DURATION
      }
      if (t1 == YEAR_MONTH_DURATION) {
        t1 = DURATION
      }
      if (t2 == YEAR_MONTH_DURATION) {
        t2 = DURATION
      }
    }
    t1 == t2
  }

  def isPossiblyComparable(t1Var: BuiltInAtomicType,
                           t2Var: BuiltInAtomicType,
                           ordered: Boolean): Boolean = {
    var t1 = t1Var
    var t2 = t2Var
    if (t1 == t2) {
      // short cut
      return true
    }
    if (t1 == ANY_ATOMIC || t2 == ANY_ATOMIC) {
      // meaning we don't actually know at this stage
      return true
    }
    if (t1.isPrimitiveNumeric) {
      t2.isPrimitiveNumeric
    }
    if (t1 == UNTYPED_ATOMIC || t1 == ANY_URI) {
      t1 = STRING
    }
    if (t2 == UNTYPED_ATOMIC || t2 == ANY_URI) {
      t2 = STRING
    }
    if (t1 == DAY_TIME_DURATION) {
      t1 = DURATION
    }
    if (t2 == DAY_TIME_DURATION) {
      t2 = DURATION
    }
    if (t1 == YEAR_MONTH_DURATION) {
      t1 = DURATION
    }
    if (t2 == YEAR_MONTH_DURATION) {
      t2 = DURATION
    }
    t1 == t2
  }

  def isGenerallyComparable(t1: BuiltInAtomicType,
                            t2: BuiltInAtomicType,
                            ordered: Boolean): Boolean =
    t1 == ANY_ATOMIC || t2 == ANY_ATOMIC ||
      t1 == UNTYPED_ATOMIC ||
      t2 == UNTYPED_ATOMIC ||
      isGuaranteedComparable(t1, t2, ordered)

  def isGuaranteedGenerallyComparable(t1: BuiltInAtomicType,
                                      t2: BuiltInAtomicType,
                                      ordered: Boolean): Boolean =
    !(t1 == ANY_ATOMIC || t2 == ANY_ATOMIC) &&
      isGenerallyComparable(t1, t2, ordered)

}
