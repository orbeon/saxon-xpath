package org.orbeon.saxon.expr

import java.io.StringWriter
import java.util.{ArrayList, List, Properties}

import javax.xml.transform.stream.StreamResult
import org.orbeon.saxon.event.{Outputter, ReceiverOption}
import org.orbeon.saxon.expr.Literal._
import org.orbeon.saxon.expr.parser._
import org.orbeon.saxon.functions.hof.FunctionLiteral
import org.orbeon.saxon.ma.arrays.ArrayItem
import org.orbeon.saxon.ma.map.MapItem
import org.orbeon.saxon.model._
import org.orbeon.saxon.om._
import org.orbeon.saxon.pattern.{NodeTestPattern, Pattern}
import org.orbeon.saxon.query.QueryResult
import org.orbeon.saxon.trace.ExpressionPresenter
import org.orbeon.saxon.trans.XPathException
import org.orbeon.saxon.utils.Configuration
import org.orbeon.saxon.value._

//import scala.collection.compat._
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
    if (value.head == null) {
      out.startElement("empty")
      out.endElement()
    } else value match {
      case atomicValue: AtomicValue =>
        exportAtomicValue(atomicValue, out)
      case integerRange: IntegerRange =>
        out.startElement("range")
        out.emitAttribute("from", "" + integerRange.getStart)
        out.emitAttribute("to", "" + integerRange.getEnd)
        out.endElement()
      case nodeInfo: NodeInfo =>
        out.startElement("node")
        val nodeKind: Int = nodeInfo.getNodeKind
        out.emitAttribute("kind", nodeKind.toString)
        if (out.getOptions.asInstanceOf[ExpressionPresenter.ExportOptions].explaining) {
          val name: String = nodeInfo.getDisplayName
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
              QueryResult.serialize(nodeInfo,
                new StreamResult(sw),
                props)
              out.emitAttribute("content", sw.toString)
              out.emitAttribute("baseUri",
                nodeInfo.getBaseURI)
            case Type.TEXT | Type.COMMENT =>
              out.emitAttribute("content",
                nodeInfo.getStringValue)
            case Type.ATTRIBUTE | Type.NAMESPACE | Type.PROCESSING_INSTRUCTION =>
              val name: StructuredQName = NameOfNode
                .makeName(nodeInfo)
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
                nodeInfo.getStringValue)
            case _ => assert(false)

          }
        }
        out.endElement()
      case mapItem: MapItem =>
        out.startElement("map")
        out.emitAttribute("size", "" + mapItem.size)
        for (kvp <- mapItem.keyValuePairs.asScala) {
          exportAtomicValue(kvp.key, out)
          exportValue(kvp.value, out)
        }
        out.endElement()
      case function: Function =>
        function.export(out)
      case externalObject: ExternalObject[_] =>
        if (out.getOptions
          .asInstanceOf[ExpressionPresenter.ExportOptions]
          .explaining) {
          out.startElement("externalObject")
          out.emitAttribute(
            "class",
            externalObject.getObject.getClass.getName)
          out.endElement()
        } else {
          throw new XPathException(
            "Cannot export a stylesheet containing literal values bound to external Java objects")
        }
      case _ =>
        out.startElement("literal")
        value match {
          case groundedValue: GroundedValue =>
            out.emitAttribute("count", groundedValue.getLength.toString)
          case _ =>
        }
        value.iterate().forEachOrFail(it => exportValue(it, out))
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
        if (value.asInstanceOf[BooleanValue].effectiveBooleanValue) "true"
        else "false")
      out.endElement()
    } else value match {
      case qualifiedNameValue: QualifiedNameValue =>
        out.startElement("qName")
        out.emitAttribute("pre", qualifiedNameValue.getPrefix)
        out.emitAttribute("uri", qualifiedNameValue.getNamespaceURI)
        out.emitAttribute("loc", qualifiedNameValue.getLocalName)
        if (`type` != BuiltInAtomicType.QNAME) {
          out.emitAttribute("type", `type`.getEQName)
        }
        out.endElement()
      case _ =>
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
    exp match {
      case literal: Literal =>
        val b: GroundedValue = literal.value
        b.isInstanceOf[BooleanValue] && b.asInstanceOf[BooleanValue].getBooleanValue == value
      case _ =>
    }
    false
  }

  def hasEffectiveBooleanValue(exp: Expression, value: Boolean): Boolean = {
    exp match {
      case literal: Literal =>
        try value == literal.value.effectiveBooleanValue
        catch {
          case _: XPathException => return false
        }
      case _ =>
    }
    false
  }

  def isConstantOne(exp: Expression): Boolean = {
    exp match {
      case literal: Literal =>
        val v: GroundedValue = literal.value
        v.isInstanceOf[Int64Value] && v.asInstanceOf[Int64Value].longValue == 1
      case _ =>
    }
    false
  }

  def makeEmptySequence: Literal = new Literal(EmptySequence.getInstance)

  def makeLiteral[T <: Item](value: GroundedValue): Literal = {
    var valueVar = value
    valueVar = valueVar.reduce()
    valueVar match {
      case stringValue: StringValue =>
        new StringLiteral(stringValue)
      case function: Function if !(valueVar.isInstanceOf[MapItem] || valueVar.isInstanceOf[ArrayItem]) =>
        new FunctionLiteral(function)
      case _ =>
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

  def this(value: GroundedValue) = {
    this()
    this.value = value.reduce()
  }

  def setValue(value: GroundedValue): Unit = this.value = value

  def getValue: GroundedValue = value

  override def typeCheck(visitor: ExpressionVisitor,
                         contextInfo: ContextItemStaticInfo): Expression = this

  override def optimize(visitor: ExpressionVisitor,
                        contextItemType: ContextItemStaticInfo): Expression = this

  override def getNetCost: Int = 0

  def getItemType: ItemType =
    value match {
      case atomicValue: AtomicValue =>
        atomicValue.getItemType
      case _ =>
        if (value.getLength == 0) {
          ErrorType
        } else {
          val th = getConfiguration.getTypeHierarchy
          SequenceTool.getItemType(value, th)
        }
    }

  override def getStaticUType(contextItemType: UType): UType =
    if (value.getLength == 0) {
      UType.VOID
    } else value match {
      case atomicValue: AtomicValue =>
        atomicValue.getUType
      case _: Function =>
        UType.FUNCTION
      case _ =>
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

  override def getIntegerBounds: Array[IntegerValue] =
    value match {
      case integerValue: IntegerValue =>
        Array(integerValue, integerValue)
      case integerRange: IntegerRange =>
        Array(
          Int64Value.makeIntegerValue(integerRange.getStart),
          Int64Value.makeIntegerValue(integerRange.getEnd))
      case _ =>
        null
    }

  override def isVacuousExpression: Boolean = value.getLength == 0

  def copy(rebindings: RebindingMap): Expression = {
    val l2: Literal = new Literal(value)
    ExpressionTool.copyLocationInfo(this, l2)
    l2
  }

  override def toPattern(config: Configuration): Pattern =
    if (isEmptySequence(this)) {
      new NodeTestPattern(ErrorType)
    } else {
      super.toPattern(config)
    }

  override def addToPathMap(
                             pathMap: PathMap,
                             pathMapNodeSet: PathMap.PathMapNodeSet): PathMap.PathMapNodeSet =
    pathMapNodeSet

  override def getDependencies: Int = 0

  override def iterate(context: XPathContext): SequenceIterator = value.iterate()

  def iterate(): SequenceIterator = value.iterate()

  override def evaluateItem(context: XPathContext): Item = value.head

  override def process(output: Outputter, context: XPathContext): Unit = {
    value match {
      case item: Item =>
        output.append(item,
          getLocation,
          ReceiverOption.ALL_NAMESPACES)
      case _ =>
        value
          .iterate()
          .forEachOrFail(it => output.append(it, getLocation, ReceiverOption.ALL_NAMESPACES))
    }
  }

  override def getImplementationMethod: Int = Expression.ITERATE_METHOD | Expression.PROCESS_METHOD | Expression.EVALUATE_METHOD

  override def evaluateAsString(context: XPathContext): CharSequence = {
    val value: AtomicValue = evaluateItem(context).asInstanceOf[AtomicValue]
    if (value == null) {
      return ""
    }
    value.getStringValueCS
  }

  override def effectiveBooleanValue(context: XPathContext): Boolean =
    value.effectiveBooleanValue

  override def evaluatePendingUpdates(context: XPathContext,
                                      pul: PendingUpdateList): Unit = {
    if (value.getLength == 0) {} else {
      super.evaluatePendingUpdates(context, pul)
    }
  }

  override def equals(obj: Any): Boolean = {
    if (!obj.isInstanceOf[Literal]) {
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
    value match {
      case atomicSequence: AtomicSequence =>
        atomicSequence.getSchemaComparable.hashCode
      case _ =>
        super.computeHashCode()
    }

  override def toString: String = value.toString

  def export(out: ExpressionPresenter): Unit = {
    exportValue(value, out)
  }

  override def getExpressionName: String = "literal"

  override def toShortString: String =
    if (value.getLength == 0) {
      "()"
    } else if (value.getLength == 1) {
      value.toShortString
    } else {
      "(" + value.head.toShortString + ", ...{" + value.getLength +
        "})"
    }

  override def isSubtreeExpression: Boolean = true

  override def getStreamerName: String = "Literal"
}
