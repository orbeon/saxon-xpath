package net.sf.saxon.model

import java.util.{HashSet, Iterator, Set}

import net.sf.saxon.model.PrimitiveUType.PrimitiveUType
import net.sf.saxon.model.UType._
import net.sf.saxon.om._
import net.sf.saxon.pattern.AnyNodeTest
import net.sf.saxon.tree.iter.UnfailingIterator
import net.sf.saxon.tree.util.FastStringBuffer
import net.sf.saxon.value.{AtomicValue, ObjectValue}

object UType {

  val VOID: UType = new UType(0)
  val DOCUMENT: UType = PrimitiveUType.DOCUMENT.toUType
  val ELEMENT: UType = PrimitiveUType.ELEMENT.toUType
  val ATTRIBUTE: UType = PrimitiveUType.ATTRIBUTE.toUType
  val TEXT: UType = PrimitiveUType.TEXT.toUType
  val COMMENT: UType = PrimitiveUType.COMMENT.toUType
  val PI: UType = PrimitiveUType.PI.toUType
  val NAMESPACE: UType = PrimitiveUType.NAMESPACE.toUType
  val FUNCTION: UType = PrimitiveUType.FUNCTION.toUType
  val STRING: UType = PrimitiveUType.STRING.toUType
  val BOOLEAN: UType = PrimitiveUType.BOOLEAN.toUType
  val DECIMAL: UType = PrimitiveUType.DECIMAL.toUType
  val FLOAT: UType = PrimitiveUType.FLOAT.toUType
  val DOUBLE: UType = PrimitiveUType.DOUBLE.toUType
  val DURATION: UType = PrimitiveUType.DURATION.toUType
  val DATE_TIME: UType = PrimitiveUType.DATE_TIME.toUType
  val TIME: UType = PrimitiveUType.TIME.toUType
  val DATE: UType = PrimitiveUType.DATE.toUType
  val G_YEAR_MONTH: UType = PrimitiveUType.G_YEAR_MONTH.toUType
  val G_YEAR: UType = PrimitiveUType.G_YEAR.toUType
  val G_MONTH_DAY: UType = PrimitiveUType.G_MONTH_DAY.toUType
  val G_DAY: UType = PrimitiveUType.G_DAY.toUType
  val G_MONTH: UType = PrimitiveUType.G_MONTH.toUType
  val HEX_BINARY: UType = PrimitiveUType.HEX_BINARY.toUType
  val BASE64_BINARY: UType = PrimitiveUType.BASE64_BINARY.toUType
  val ANY_URI: UType = PrimitiveUType.ANY_URI.toUType
  val QNAME: UType = PrimitiveUType.QNAME.toUType
  val NOTATION: UType = PrimitiveUType.NOTATION.toUType
  val UNTYPED_ATOMIC: UType = PrimitiveUType.UNTYPED_ATOMIC.toUType
  val EXTENSION: UType = PrimitiveUType.EXTENSION.toUType
  val NUMERIC: UType = DOUBLE.union(FLOAT).union(DECIMAL)
  val STRING_LIKE: UType = STRING.union(ANY_URI).union(UNTYPED_ATOMIC)
  val CHILD_NODE_KINDS: UType = ELEMENT.union(TEXT).union(COMMENT).union(PI)
  val PARENT_NODE_KINDS: UType = DOCUMENT.union(ELEMENT)
  val ELEMENT_OR_ATTRIBUTE: UType = ELEMENT.union(ATTRIBUTE)
  val ANY_NODE: UType = CHILD_NODE_KINDS.union(DOCUMENT).union(ATTRIBUTE).union(NAMESPACE)
  val ANY_ATOMIC: UType = new UType(0x0FFFFF00)
  val ANY: UType = ANY_NODE.union(ANY_ATOMIC).union(FUNCTION).union(EXTENSION)

  def fromTypeCode(code: Int): UType = code match {
    case Type.NODE => ANY_NODE
    case Type.ELEMENT => ELEMENT
    case Type.ATTRIBUTE => ATTRIBUTE
    case Type.TEXT | Type.WHITESPACE_TEXT => TEXT
    case Type.DOCUMENT => DOCUMENT
    case Type.COMMENT => COMMENT
    case Type.PROCESSING_INSTRUCTION => PI
    case Type.NAMESPACE => NAMESPACE
    case Type.FUNCTION => FUNCTION
    case Type.ITEM => ANY
    case StandardNames.XS_ANY_ATOMIC_TYPE => ANY_ATOMIC
    case StandardNames.XS_NUMERIC => NUMERIC
    case StandardNames.XS_STRING => STRING
    case StandardNames.XS_BOOLEAN => BOOLEAN
    case StandardNames.XS_DURATION => DURATION
    case StandardNames.XS_DATE_TIME => DATE_TIME
    case StandardNames.XS_DATE => DATE
    case StandardNames.XS_TIME => TIME
    case StandardNames.XS_G_YEAR_MONTH => G_YEAR_MONTH
    case StandardNames.XS_G_MONTH => G_MONTH
    case StandardNames.XS_G_MONTH_DAY => G_MONTH_DAY
    case StandardNames.XS_G_YEAR => G_YEAR
    case StandardNames.XS_G_DAY => G_DAY
    case StandardNames.XS_HEX_BINARY => HEX_BINARY
    case StandardNames.XS_BASE64_BINARY => BASE64_BINARY
    case StandardNames.XS_ANY_URI => ANY_URI
    case StandardNames.XS_QNAME => QNAME
    case StandardNames.XS_NOTATION => NOTATION
    case StandardNames.XS_UNTYPED_ATOMIC => UNTYPED_ATOMIC
    case StandardNames.XS_DECIMAL => DECIMAL
    case StandardNames.XS_FLOAT => FLOAT
    case StandardNames.XS_DOUBLE => DOUBLE
    case StandardNames.XS_INTEGER => DECIMAL
    case StandardNames.XS_NON_POSITIVE_INTEGER |
        StandardNames.XS_NEGATIVE_INTEGER | StandardNames.XS_LONG |
        StandardNames.XS_INT | StandardNames.XS_SHORT | StandardNames.XS_BYTE |
        StandardNames.XS_NON_NEGATIVE_INTEGER |
        StandardNames.XS_POSITIVE_INTEGER | StandardNames.XS_UNSIGNED_LONG |
        StandardNames.XS_UNSIGNED_INT | StandardNames.XS_UNSIGNED_SHORT |
        StandardNames.XS_UNSIGNED_BYTE =>
      DECIMAL
    case StandardNames.XS_YEAR_MONTH_DURATION |
        StandardNames.XS_DAY_TIME_DURATION =>
      DURATION
    case StandardNames.XS_DATE_TIME_STAMP => DATE_TIME
    case StandardNames.XS_NORMALIZED_STRING | StandardNames.XS_TOKEN |
        StandardNames.XS_LANGUAGE | StandardNames.XS_NAME |
        StandardNames.XS_NMTOKEN | StandardNames.XS_NCNAME |
        StandardNames.XS_ID | StandardNames.XS_IDREF |
        StandardNames.XS_ENTITY =>
      STRING
    case _ => throw new IllegalArgumentException("" + code)

  }

  def getUType(item: Item): UType =
    if (item.isInstanceOf[NodeInfo]) {
      fromTypeCode(item.asInstanceOf[NodeInfo].getNodeKind)
    } else if (item.isInstanceOf[AtomicValue]) {
      item.asInstanceOf[AtomicValue].getUType
    } else if (item.isInstanceOf[Function]) {
      UType.FUNCTION
    } else if (item.isInstanceOf[ObjectValue[_]]) {
      UType.EXTENSION
    } else {
      UType.VOID
    }

  def getUType(sequence: GroundedValue): UType = {
    val iter: UnfailingIterator = sequence.iterate()
    var item: Item = null
    var u: UType = UType.VOID
    while (({
      item = iter.next()
      item
    }) != null) u = u.union(getUType(item))
    u
  }

  def isPossiblyComparable(t1: UType, t2: UType, ordered: Boolean): Boolean = {
    var t1Var = t1
    var t2Var = t2
    if (t1Var == t2Var) {
      return true
    }
    if (t1Var == UType.ANY_ATOMIC || t2Var == UType.ANY_ATOMIC) {
      return true
    }
    if (t1Var == UType.UNTYPED_ATOMIC || t1Var == UType.ANY_URI) {
      t1Var = UType.STRING
    }
    if (t2Var == UType.UNTYPED_ATOMIC || t2Var == UType.ANY_URI) {
      t2Var = UType.STRING
    }
    if (NUMERIC.subsumes(t1Var)) {
      t1Var = NUMERIC
    }
    if (NUMERIC.subsumes(t2Var)) {
      t2Var = NUMERIC
    }
    t1Var == t2Var
  }

  def isGuaranteedComparable(t1: UType, t2: UType): Boolean = {
    var t1Var = t1
    var t2Var = t2
    if (t1 == t2) {
      return true
    }
    if (t1Var == UType.UNTYPED_ATOMIC || t1Var == UType.ANY_URI) {
      t1Var = UType.STRING
    }
    if (t2Var == UType.UNTYPED_ATOMIC || t2Var == UType.ANY_URI) {
      t2Var = UType.STRING
    }
    if (NUMERIC.subsumes(t1Var)) {
      t1Var = NUMERIC
    }
    if (NUMERIC.subsumes(t2Var)) {
      t2Var = NUMERIC
    }
    return t1Var == t2Var
  }

  def isGenerallyComparable(t1: UType, t2: UType): Boolean =
    t1 == UType.UNTYPED_ATOMIC || t2 == UType.UNTYPED_ATOMIC ||
      isGuaranteedComparable(t1, t2)

}

class UType(private var bits: Int) {

  /**
    * Returns a hash code value for the object.
    *
    * @return a hash code value for this object.
    * @see Object#equals(Object)
    * @see java.util.Hashtable
    */
  override def hashCode: Int = bits

  /**
    * Indicates whether some other object is "equal to" this one.
    *
    * @param obj the reference object with which to compare.
    * @return <code>true</code> if this object is the same as the obj
    *         argument; <code>false</code> otherwise.
    * @see #hashCode
    * @see java.util.Hashtable
    */
  override def equals(obj: Any): Boolean = obj match {
    case obj: UType => bits == obj.bits
    case _ => false

  }

  def union(other: UType): UType = {
    if (other == null) {
      new NullPointerException().printStackTrace()
    }
    new UType(bits | other.bits)
  }

  def intersection(other: UType): UType = new UType(bits & other.bits)

  def except(other: UType): UType = new UType(bits & ~other.bits)

  def decompose(): Set[PrimitiveUType] = {
    val result: Set[PrimitiveUType] = new HashSet[PrimitiveUType]()
    for (p <- PrimitiveUType.values if (bits & (1 << p.getBit)) != 0) {
      result.add(p)
    }
    result
  }

  override def toString: String = {
    val components: Set[PrimitiveUType] = decompose()
    if (components.isEmpty) {
      return "U{}"
    }
    val sb: FastStringBuffer = new FastStringBuffer(FastStringBuffer.C256)
    val iter: Iterator[PrimitiveUType] = components.iterator
    var started: Boolean = false
    while (iter.hasNext) {
      if (started) {
        sb.append("|")
      }
      started = true
      sb.append(iter.next().toString)
    }
    sb.toString
  }

  def toStringWithIndefiniteArticle: String = {
    val s: String = toString
    if ("aeiouxy".indexOf(s.charAt(0)) >= 0) {
      "an " + s + " node"
    } else {
      "a " + s + " node"
    }
  }

  def overlaps(other: UType): Boolean = (bits & other.bits) != 0

  def subsumes(other: UType): Boolean = (bits & other.bits) == other.bits

  def toItemType: ItemType = {
    val p: Set[PrimitiveUType] = decompose()
    if (p.isEmpty) {
      ErrorType
    } else if (p.size == 1) {
      p.toArray(Array.ofDim[PrimitiveUType](1))(0).toItemType
    } else if (ANY_NODE.subsumes(this)) {
      AnyNodeTest.getInstance
    } else if (equals(NUMERIC)) {
      NumericType.getInstance
    } else if (ANY_ATOMIC.subsumes(this)) {
      BuiltInAtomicType.ANY_ATOMIC
    } else {
      AnyItemType
    }
  }

  def matches(item: Item): Boolean = subsumes(getUType(item))
}
