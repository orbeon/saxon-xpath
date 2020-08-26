package net.sf.saxon.expr

import java.io.StringWriter
import java.util.{ArrayList, List, Properties}

import javax.xml.transform.stream.StreamResult
import net.sf.saxon.event.{Outputter, ReceiverOption}
import net.sf.saxon.expr.Literal._
import net.sf.saxon.expr.parser._
import net.sf.saxon.functions.hof.FunctionLiteral
import net.sf.saxon.ma.arrays.ArrayItem
import net.sf.saxon.ma.map.MapItem
import net.sf.saxon.model._
import net.sf.saxon.om._
import net.sf.saxon.pattern.{NodeTestPattern, Pattern}
import net.sf.saxon.query.QueryResult
import net.sf.saxon.trace.ExpressionPresenter
import net.sf.saxon.trans.XPathException
import net.sf.saxon.utils.Configuration
import net.sf.saxon.value._

import scala.jdk.CollectionConverters._

object Literal {

  def makeStringsLiteral(strings: List[String]): Literal = {
    val values: List[StringValue] = new ArrayList[StringValue]()
    for (s: String <- strings.asScala) {
      values.add(new StringValue(s))
    }
    val gv: GroundedValue = SequenceExtent.makeSequenceExtent(values)
    makeLiteral(gv)
  }

  def exportValue(value: Sequence, out: ExpressionPresenter): Unit = {
    if (value.head() == null) {
      out.startElement("empty")
      out.endElement()
    } else if (value.isInstanceOf[AtomicValue]) {
      exportAtomicValue(value.asInstanceOf[AtomicValue], out)
    } else if (value.isInstanceOf[IntegerRange]) {
      out.startElement("range")
      out.emitAttribute("from", "" + value.asInstanceOf[IntegerRange].getStart)
      out.emitAttribute("to", "" + value.asInstanceOf[IntegerRange].getEnd)
      out.endElement()
    } else if (value.isInstanceOf[NodeInfo]) {
      out.startElement("node")
      val nodeKind: Int = value.asInstanceOf[NodeInfo].getNodeKind
      out.emitAttribute("kind", nodeKind.toString + "")
      if (out.getOptions
        .asInstanceOf[ExpressionPresenter.ExportOptions]
        .explaining) {
        val name: String = value.asInstanceOf[NodeInfo].getDisplayName
        if (!name.isEmpty) {
          out.emitAttribute("name", name)
        }
      } else {
        nodeKind match {
          case Type.DOCUMENT | Type.ELEMENT =>
            var sw: StringWriter = new StringWriter()
            var props: Properties = new Properties()
            props.setProperty("method", "xml")
            props.setProperty("indent", "no")
            props.setProperty("omit-xml-declaration", "yes")
            QueryResult.serialize(value.asInstanceOf[NodeInfo],
              new StreamResult(sw),
              props)
            out.emitAttribute("content", sw.toString)
            out.emitAttribute("baseUri",
              value.asInstanceOf[NodeInfo].getBaseURI)
          case Type.TEXT | Type.COMMENT =>
            out.emitAttribute("content",
              value.asInstanceOf[NodeInfo].getStringValue)
          case Type.ATTRIBUTE | Type.NAMESPACE | Type.PROCESSING_INSTRUCTION =>
            val name: StructuredQName = NameOfNode
              .makeName(value.asInstanceOf[NodeInfo])
              .getStructuredQName
            if (!name.getLocalPart.isEmpty) {
              out.emitAttribute("localName", name.getLocalPart)
            }
            if (!name.getPrefix.isEmpty) {
              out.emitAttribute("prefix", name.getPrefix)
            }
            if (!name.getURI.isEmpty) {
              out.emitAttribute("ns", name.getURI)
            }
            out.emitAttribute("content",
              value.asInstanceOf[NodeInfo].getStringValue)
          case _ => assert(false)

        }
      }
      out.endElement()
    } else if (value.isInstanceOf[MapItem]) {
      out.startElement("map")
      out.emitAttribute("size", "" + value.asInstanceOf[MapItem].size)
      for (kvp <- value.asInstanceOf[MapItem].keyValuePairs().asScala) {
        exportAtomicValue(kvp.key, out)
        exportValue(kvp.value, out)
      }
      out.endElement()
    } else if (value.isInstanceOf[Function]) {
      value.asInstanceOf[Function].export(out)
    } else if (value.isInstanceOf[ExternalObject[_]]) {
      if (out.getOptions
        .asInstanceOf[ExpressionPresenter.ExportOptions]
        .explaining) {
        out.startElement("externalObject")
        out.emitAttribute(
          "class",
          value.asInstanceOf[ExternalObject[_]].getObject.getClass.getName)
        out.endElement()
      } else {
        throw new XPathException(
          "Cannot export a stylesheet containing literal values bound to external Java objects")
      }
    } else {
      out.startElement("literal")
      if (value.isInstanceOf[GroundedValue]) {
        out.emitAttribute("count",
          value.asInstanceOf[GroundedValue].getLength.toString + "")
      }
      value.iterate().forEachOrFail((it) => exportValue(it, out))
      out.endElement()
    }
  }

  def exportAtomicValue(value: AtomicValue, out: ExpressionPresenter): Unit = {
    if ("JS" == out.getOptions
      .asInstanceOf[ExpressionPresenter.ExportOptions]
      .target) {
      value.checkValidInJavascript()
    }
    val `type`: AtomicType = value.getItemType
    val `val`: String = value.getStringValue
    if (`type` == BuiltInAtomicType.STRING) {
      out.startElement("str")
      out.emitAttribute("val", `val`)
      out.endElement()
    } else if (`type` == BuiltInAtomicType.INTEGER) {
      out.startElement("int")
      out.emitAttribute("val", `val`)
      out.endElement()
    } else if (`type` == BuiltInAtomicType.DECIMAL) {
      out.startElement("dec")
      out.emitAttribute("val", `val`)
      out.endElement()
    } else if (`type` == BuiltInAtomicType.DOUBLE) {
      out.startElement("dbl")
      out.emitAttribute("val", `val`)
      out.endElement()
    } else if (`type` == BuiltInAtomicType.BOOLEAN) {
      out.startElement(
        if (value.asInstanceOf[BooleanValue].effectiveBooleanValue()) "true"
        else "false")
      out.endElement()
    } else if (value.isInstanceOf[QualifiedNameValue]) {
      out.startElement("qName")
      out.emitAttribute("pre",
        value.asInstanceOf[QualifiedNameValue].getPrefix)
      out.emitAttribute("uri",
        value.asInstanceOf[QualifiedNameValue].getNamespaceURI)
      out.emitAttribute("loc",
        value.asInstanceOf[QualifiedNameValue].getLocalName)
      if (`type` != BuiltInAtomicType.QNAME) {
        out.emitAttribute("type", `type`.getEQName)
      }
      out.endElement()
    } else {
      out.startElement("atomic")
      out.emitAttribute("val", `val`)
      out.emitAttribute("type", AlphaCode.fromItemType(`type`))
      out.endElement()
    }
  }

  def isAtomic(exp: Expression): Boolean =
    exp.isInstanceOf[Literal] &&
      exp.asInstanceOf[Literal].value.isInstanceOf[AtomicValue]

  def isEmptySequence(exp: Expression): Boolean =
    exp.isInstanceOf[Literal] && exp
      .asInstanceOf[Literal]
      .value
      .getLength == 0

  def isConstantBoolean(exp: Expression, value: Boolean): Boolean = {
    if (exp.isInstanceOf[Literal]) {
      val b: GroundedValue = exp.asInstanceOf[Literal].value
      b.isInstanceOf[BooleanValue] &&
        b.asInstanceOf[BooleanValue].getBooleanValue == value
    }
    false
  }

  def hasEffectiveBooleanValue(exp: Expression, value: Boolean): Boolean = {
    if (exp.isInstanceOf[Literal]) {
      try value ==
        exp.asInstanceOf[Literal].value.effectiveBooleanValue()
      catch {
        case err: XPathException => return false

      }
    }
    false
  }

  def isConstantOne(exp: Expression): Boolean = {
    if (exp.isInstanceOf[Literal]) {
      val v: GroundedValue = exp.asInstanceOf[Literal].value
      v.isInstanceOf[Int64Value] && v.asInstanceOf[Int64Value].longValue() == 1
    }
    false
  }

  def makeEmptySequence(): Literal = new Literal(EmptySequence.getInstance)

  def makeLiteral[T <: Item](value: GroundedValue): Literal = {
    var valueVar = value
    valueVar = valueVar.reduce()
    if (valueVar.isInstanceOf[StringValue]) {
      new StringLiteral(valueVar.asInstanceOf[StringValue])
    } else if (valueVar.isInstanceOf[Function] &&
      !(valueVar.isInstanceOf[MapItem] || valueVar
        .isInstanceOf[ArrayItem])) {
      new FunctionLiteral(valueVar.asInstanceOf[Function])
    } else {
      new Literal(valueVar)
    }
  }

  def makeLiteral(value: GroundedValue, origin: Expression): Literal = {
    val lit: Literal = makeLiteral(value)
    ExpressionTool.copyLocationInfo(origin, lit)
    lit
  }

}

class Literal extends Expression {

  var value: GroundedValue = _

  def this(value: GroundedValue) {
    this()
    this.value = value.reduce()
  }

  def setValue(value: GroundedValue) = this.value = value

  def getValue: GroundedValue = value

  override def typeCheck(visitor: ExpressionVisitor,
                         contextInfo: ContextItemStaticInfo): Expression = this

  override def optimize(visitor: ExpressionVisitor,
                        contextItemType: ContextItemStaticInfo): Expression = this

  override def getNetCost(): Int = 0

  def getItemType(): ItemType =
    if (value.isInstanceOf[AtomicValue]) {
      value.asInstanceOf[AtomicValue].getItemType
    } else if (value.getLength == 0) {
      ErrorType.getInstance
    } else {
      val th: TypeHierarchy = getConfiguration.getTypeHierarchy
      SequenceTool.getItemType(value, th)
    }

  override def getStaticUType(contextItemType: UType): UType =
    if (value.getLength == 0) {
      UType.VOID
    } else if (value.isInstanceOf[AtomicValue]) {
      value.asInstanceOf[AtomicValue].getUType
    } else if (value.isInstanceOf[Function]) {
      UType.FUNCTION
    } else {
      super.getStaticUType(contextItemType)
    }

  def computeCardinality(): Int = {
    if (value.getLength == 0) {
      StaticProperty.EMPTY
    } else if (value.isInstanceOf[AtomicValue]) {
      StaticProperty.EXACTLY_ONE
    }
    try {
      val iter: SequenceIterator = value.iterate()
      val next: Item = iter.next()
      if (next == null) {
        StaticProperty.EMPTY
      } else {
        if (iter.next() != null) {
          StaticProperty.ALLOWS_MANY
        } else {
          StaticProperty.EXACTLY_ONE
        }
      }
    } catch {
      case err: XPathException => StaticProperty.ALLOWS_ZERO_OR_MORE

    }
  }

  override def computeSpecialProperties(): Int = {
    if (value.getLength == 0) {
      StaticProperty.SPECIAL_PROPERTY_MASK & ~StaticProperty.HAS_SIDE_EFFECTS
    }
    StaticProperty.NO_NODES_NEWLY_CREATED
  }

  override def getIntegerBounds(): Array[IntegerValue] =
    if (value.isInstanceOf[IntegerValue]) {
      Array(value.asInstanceOf[IntegerValue], value.asInstanceOf[IntegerValue])
    } else if (value.isInstanceOf[IntegerRange]) {
      Array(
        Int64Value.makeIntegerValue(value.asInstanceOf[IntegerRange].getStart),
        Int64Value.makeIntegerValue(value.asInstanceOf[IntegerRange].getEnd))
    } else {
      null
    }

  override def isVacuousExpression(): Boolean = value.getLength == 0

  def copy(rebindings: RebindingMap): Expression = {
    val l2: Literal = new Literal(value)
    ExpressionTool.copyLocationInfo(this, l2)
    l2
  }

  override def toPattern(config: Configuration): Pattern =
    if (isEmptySequence(this)) {
      new NodeTestPattern(ErrorType.getInstance)
    } else {
      super.toPattern(config)
    }

  override def addToPathMap(
                             pathMap: PathMap,
                             pathMapNodeSet: PathMap.PathMapNodeSet): PathMap.PathMapNodeSet =
    pathMapNodeSet

  override def getDependencies(): Int = 0

  override def iterate(context: XPathContext): SequenceIterator = value.iterate()

  def iterate(): SequenceIterator = value.iterate()

  override def evaluateItem(context: XPathContext): Item = value.head()

  override def process(output: Outputter, context: XPathContext): Unit = {
    if (value.isInstanceOf[Item]) {
      output.append(value.asInstanceOf[Item],
        getLocation,
        ReceiverOption.ALL_NAMESPACES)
    } else {
      value
        .iterate()
        .forEachOrFail((it) =>
          output.append(it, getLocation, ReceiverOption.ALL_NAMESPACES))
    }
  }

  override def getImplementationMethod(): Int = Expression.ITERATE_METHOD | Expression.PROCESS_METHOD | Expression.EVALUATE_METHOD

  override def evaluateAsString(context: XPathContext): CharSequence = {
    val value: AtomicValue = evaluateItem(context).asInstanceOf[AtomicValue]
    if (value == null) {
      return ""
    }
    value.getStringValueCS
  }

  override def effectiveBooleanValue(context: XPathContext): Boolean =
    value.effectiveBooleanValue()

  override def evaluatePendingUpdates(context: XPathContext,
                                      pul: PendingUpdateList): Unit = {
    if (value.getLength == 0) {} else {
      super.evaluatePendingUpdates(context, pul)
    }
  }

  override def equals(obj: Any): Boolean = {
    if (!(obj.isInstanceOf[Literal])) {
      return false
    }
    val v0: GroundedValue = value
    val v1: GroundedValue = obj.asInstanceOf[Literal].value
    try {
      val i0: SequenceIterator = v0.iterate()
      val i1: SequenceIterator = v1.iterate()
      while (true) {
        val m0: Item = i0.next()
        val m1: Item = i1.next()
        if (m0 == null && m1 == null) {
          return true
        }
        if (m0 == null || m1 == null) {
          return false
        }
        if (m0 ne m1) {
          val n0: Boolean = m0.isInstanceOf[NodeInfo]
          val n1: Boolean = m1.isInstanceOf[NodeInfo]
          if (n0 != n1) {
            return false
          }
          if (n0) {
            if (m0 != m1) {
              return false
            }
          } else {
            val a0: Boolean = m0.isInstanceOf[AtomicValue]
            val a1: Boolean = m1.isInstanceOf[AtomicValue]
            if (a0 != a1) {
              return false
            }
            if (a0) {
              if (! (m0.asInstanceOf[AtomicValue].isIdentical(m1.asInstanceOf[AtomicValue]) &&
                  m0.asInstanceOf[AtomicValue].getItemType == m1.asInstanceOf[AtomicValue].getItemType)) {
                return false
              }
            } else
              return false
          }
        }
      }
      false
    } catch {
      case _: XPathException => false
    }
  }

  override def computeHashCode(): Int =
    if (value.isInstanceOf[AtomicSequence]) {
      value.asInstanceOf[AtomicSequence].getSchemaComparable.hashCode
    } else {
      super.computeHashCode()
    }

  override def toString(): String = value.toString

  def export(out: ExpressionPresenter): Unit = {
    exportValue(value, out)
  }

  override def getExpressionName(): String = "literal"

  override def toShortString(): String =
    if (value.getLength == 0) {
      "()"
    } else if (value.getLength == 1) {
      value.toShortString()
    } else {
      "(" + value.head().toShortString() + ", ...{" + value.getLength +
        "})"
    }

  override def isSubtreeExpression(): Boolean = true

  override def getStreamerName(): String = "Literal"

}
