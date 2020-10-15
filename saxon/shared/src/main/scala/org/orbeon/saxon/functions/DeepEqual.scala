package org.orbeon.saxon.functions

import org.orbeon.saxon.expr.XPathContext
import org.orbeon.saxon.expr.sort.{AtomicComparer, GenericAtomicComparer}
import org.orbeon.saxon.lib.ErrorReporter
import org.orbeon.saxon.model.{ComplexType, Type}
import org.orbeon.saxon.om._
import org.orbeon.saxon.pattern.SameNameTest

//import scala.collection.compat._
import java.util.{ArrayList, HashSet}

import org.orbeon.saxon.functions.DeepEqual._
import org.orbeon.saxon.trans.XmlProcessingIncident
import org.orbeon.saxon.tree.iter.{AtomicIterator, AxisIterator}
import org.orbeon.saxon.tree.tiny.WhitespaceTextImpl
import org.orbeon.saxon.tree.util.{FastStringBuffer, Navigator, Orphan}
import org.orbeon.saxon.value._

import scala.jdk.CollectionConverters._
import scala.util.control.Breaks._

object DeepEqual {

  val INCLUDE_NAMESPACES              : Int = 1
  val INCLUDE_PREFIXES                : Int = 1 << 1
  val INCLUDE_COMMENTS                : Int = 1 << 2
  val INCLUDE_PROCESSING_INSTRUCTIONS : Int = 1 << 3
  val EXCLUDE_WHITESPACE_TEXT_NODES   : Int = 1 << 4
  val COMPARE_STRING_VALUES           : Int = 1 << 5
  val COMPARE_ANNOTATIONS             : Int = 1 << 6
  val WARNING_IF_FALSE                : Int = 1 << 7
  val JOIN_ADJACENT_TEXT_NODES        : Int = 1 << 8
  val COMPARE_ID_FLAGS                : Int = 1 << 9
  val EXCLUDE_VARIETY                 : Int = 1 << 10

  def deepEqual(op1: SequenceIterator,
                op2: SequenceIterator,
                comparer: AtomicComparer,
                context: XPathContext,
                flags: Int): Boolean = {
    var result: Boolean = true
    var reason: String = null
    var opSeqItr1 = op1
    var opSeqItr2 = op2
    val reporter: ErrorReporter = context.getErrorReporter
    try {
      if ((flags & JOIN_ADJACENT_TEXT_NODES) != 0) {
        opSeqItr1 = mergeAdjacentTextNodes(opSeqItr1)
        opSeqItr2 = mergeAdjacentTextNodes(opSeqItr2)
      }
      var pos1: Int = 0
      var pos2: Int = 0
      while (true) {
        val item1: Item = opSeqItr1.next()
        val item2: Item = opSeqItr2.next()
        breakable {
          if (item1 == null && item2 == null)
            break()
        }

        pos1 += 1
        pos2 += 1
        breakable {
          if (item1 == null || item2 == null) {
            result = false
            reason =
              if (item1 == null)
                "Second sequence is longer (first sequence length = " +
                  pos2 +
                  ")"
              else
                "First sequence is longer (second sequence length = " +
                  pos1 +
                  ")"
            if (item1.isInstanceOf[WhitespaceTextImpl] || item2
              .isInstanceOf[WhitespaceTextImpl]) {
              reason += " (the first extra node is whitespace text)"
            }
            break()
          }
        }
        if (item1.isInstanceOf[Function] || item2.isInstanceOf[Function]) {
          if (!(item1.isInstanceOf[Function] && item2
            .isInstanceOf[Function])) {
            reason = "if one item is a function then both must be functions (position " +
              pos1 +
              ")"
            return false
          }
          val fe: Boolean = item1
            .asInstanceOf[Function]
            .deepEquals(item2.asInstanceOf[Function], context, comparer, flags)
          breakable {
            if (!fe) {
              result = false
              reason = "functions at position " + pos1 + " differ"
              break()
            }
          }
        }
        if (item1.isInstanceOf[ObjectValue[_]] || item2
          .isInstanceOf[ObjectValue[_]]) {
          if (item1 != item2) return false
        }
        breakable {
          item1 match {
            case nodeInfo1: NodeInfo =>
              item2 match {
                case nodeInfo2: NodeInfo =>
                  if (!deepEquals(nodeInfo1,
                    nodeInfo2,
                    comparer,
                    context,
                    flags)) {
                    result = false
                    reason = "nodes at position " + pos1 + " differ"
                    break()
                  }
                case _ =>
                  result = false
                  reason = "comparing a node to an atomic value at position " + pos1
                  break()
              }
            case _ =>
              if (item2.isInstanceOf[NodeInfo]) {
                result = false
                reason = "comparing an atomic value to a node at position " + pos1
                break()
              } else {
                val av1 = item1.asInstanceOf[AtomicValue]
                val av2 = item2.asInstanceOf[AtomicValue]
                if (av1.isNaN && av2.isNaN) {} else if (!comparer.comparesEqual(
                  av1,
                  av2)) {
                  result = false
                  reason = "atomic values at position " + pos1 + " differ"
                  break()
                }
              }
          }
        }
      }
    } catch {
      case _: ClassCastException =>
        result = false
        reason = "sequences contain non-comparable values"
    }
    if (! result)
      explain(reporter, reason, flags, null, null)
    result
  }

  def deepEquals(n1: NodeInfo,
                 n2: NodeInfo,
                 comparer: AtomicComparer,
                 context: XPathContext,
                 flags: Int): Boolean = {
    if (n1 == n2)
      return true
    val reporter = context.getErrorReporter
    if (n1.getNodeKind != n2.getNodeKind) {
      explain(reporter,
        "node kinds differ: comparing " + showKind(n1) + " to " +
          showKind(n2),
        flags,
        n1,
        n2)
      return false
    }
    n1.getNodeKind match {
      case Type.ELEMENT =>
        if (!Navigator.haveSameName(n1, n2)) {
          explain(
            reporter,
            "element names differ: " +
              NameOfNode.makeName(n1).getStructuredQName.getEQName +
              " != " +
              NameOfNode.makeName(n2).getStructuredQName.getEQName,
            flags,
            n1,
            n2
          )
          return false
        }
        if (((flags & INCLUDE_PREFIXES) != 0) && n1.getPrefix != n2.getPrefix) {
          explain(reporter,
            "element prefixes differ: " + n1.getPrefix + " != " +
              n2.getPrefix,
            flags,
            n1,
            n2)
          return false
        }
        var a1: AxisIterator = n1.iterateAxis(AxisInfo.ATTRIBUTE)
        val a2: AxisIterator = n2.iterateAxis(AxisInfo.ATTRIBUTE)
        if (!SequenceTool.sameLength(a1, a2)) {
          explain(reporter,
            "elements have different number of attributes",
            flags,
            n1,
            n2)
          return false
        }
        var att1: NodeInfo = null
        a1 = n1.iterateAxis(AxisInfo.ATTRIBUTE)
        while (({
          att1 = a1.next()
          att1
        }) != null) {
          val a2iter: AxisIterator =
            n2.iterateAxis(AxisInfo.ATTRIBUTE, new SameNameTest(att1))
          val att2: NodeInfo = a2iter.next()
          if (att2 == null) {
            explain(reporter,
              "one element has an attribute " +
                NameOfNode.makeName(att1).getStructuredQName.getEQName +
                ", the other does not",
              flags,
              n1,
              n2)
            return false
          }
          if (!deepEquals(att1, att2, comparer, context, flags)) {
            deepEquals(att1, att2, comparer, context, flags)
            explain(reporter,
              "elements have different values for the attribute " +
                NameOfNode.makeName(att1).getStructuredQName.getEQName,
              flags,
              n1,
              n2)
            return false
          }
        }
        if ((flags & INCLUDE_NAMESPACES) != 0) {
          val ns1 = new HashSet[NamespaceBinding](10)
          val ns2 = new HashSet[NamespaceBinding](10)
          val it1 = n1.iterateAxis(AxisInfo.NAMESPACE)
          var nn1: NodeInfo = null
          while ({
            nn1 = it1.next()
            nn1
          } != null) {
            val nscode1 = new NamespaceBinding(nn1.getLocalPart, nn1.getStringValue)
            ns1.add(nscode1)
          }
          val it2 = n2.iterateAxis(AxisInfo.NAMESPACE)
          var nn2: NodeInfo = null
          while ({
            nn2 = it2.next()
            nn2
          } != null) {
            val nscode2 = new NamespaceBinding(nn2.getLocalPart, nn2.getStringValue)
            ns2.add(nscode2)
          }
          if (ns1 != ns2) {
            explain(
              reporter,
              "elements have different in-scope namespaces: " + showNamespaces(
                ns1) +
                " versus " +
                showNamespaces(ns2),
              flags,
              n1,
              n2)
            return false
          }
        }
        if ((flags & COMPARE_ANNOTATIONS) != 0) {
          if (n1.getSchemaType != n2.getSchemaType) {
            explain(reporter,
              "elements have different type annotation",
              flags,
              n1,
              n2)
            return false
          }
        }
        if ((flags & EXCLUDE_VARIETY) == 0) {
          if (n1.getSchemaType.isComplexType != n2.getSchemaType.isComplexType) {
            explain(reporter,
              "one element has complex type, the other simple",
              flags,
              n1,
              n2)
            return false
          }
          if (n1.getSchemaType.isComplexType) {
            val variety1 =
              n1.getSchemaType.asInstanceOf[ComplexType].getVariety
            val variety2 =
              n2.getSchemaType.asInstanceOf[ComplexType].getVariety
            if (variety1 != variety2) {
              explain(
                reporter,
                "both elements have complex type, but a different variety",
                flags,
                n1,
                n2)
              return false
            }
          }
        }
        if ((flags & COMPARE_STRING_VALUES) == 0) {
          val type1 = n1.getSchemaType
          val type2 = n2.getSchemaType
          val isSimple1 = type1.isSimpleType || type1.asInstanceOf[ComplexType].isSimpleContent
          val isSimple2 = type2.isSimpleType || type2.asInstanceOf[ComplexType].isSimpleContent
          if (isSimple1 != isSimple2) {
            explain(reporter,
              "one element has a simple type, the other does not",
              flags,
              n1,
              n2)
            return false
          }
          if (isSimple1) {
            assert(isSimple2)
            val v1 = n1.atomize().iterate().asInstanceOf[AtomicIterator[AtomicValue]]
            val v2 = n2.atomize().iterate().asInstanceOf[AtomicIterator[AtomicValue]]
            return deepEqual(v1, v2, comparer, context, flags)
          }
          return false
        }
        if ((flags & COMPARE_ID_FLAGS) != 0) {
          if (n1.isId != n2.isId) {
            explain(reporter,
              "one element is an ID, the other is not",
              flags,
              n1,
              n2)
            return false
          }
          if (n1.isIdref != n2.isIdref) {
            explain(reporter,
              "one element is an IDREF, the other is not",
              flags,
              n1,
              n2)
            return false
          }
          return false
        }
      case Type.DOCUMENT =>
        val c1 = n1.iterateAxis(AxisInfo.CHILD)
        val c2 = n2.iterateAxis(AxisInfo.CHILD)
        while (true) {
          var d1 = c1.next()
          while (d1 != null && isIgnorable(d1, flags))
            d1 = c1.next()
          var d2 = c2.next()
          while (d2 != null && isIgnorable(d2, flags))
            d2 = c2.next()
          if (d1 == null || d2 == null) {
            val r = d1 == d2
            if (! r) {
              var message: String = "the first operand contains a node with " + (if (d1 == null)
                "fewer"
              else
                "more") +
                " children than the second"
              if (d1.isInstanceOf[WhitespaceTextImpl] || d2
                .isInstanceOf[WhitespaceTextImpl]) {
                message += " (the first extra child is whitespace text)"
              }
              explain(reporter, message, flags, n1, n2)
            }
            return r
          }
          if (! deepEquals(d1, d2, comparer, context, flags))
            return false
          false
        }
      case Type.ATTRIBUTE =>
        if (! Navigator.haveSameName(n1, n2)) {
          explain(
            reporter,
            "attribute names differ: " +
              NameOfNode.makeName(n1).getStructuredQName.getEQName +
              " != " +
              NameOfNode.makeName(n1).getStructuredQName.getEQName,
            flags,
            n1,
            n2
          )
          return false
        }
        if (((flags & INCLUDE_PREFIXES) != 0) && n1.getPrefix != n2.getPrefix) {
          explain(reporter,
            "attribute prefixes differ: " + n1.getPrefix + " != " +
              n2.getPrefix,
            flags,
            n1,
            n2)
          return false
        }
        if ((flags & COMPARE_ANNOTATIONS) != 0) {
          if (n1.getSchemaType != n2.getSchemaType) {
            explain(reporter,
              "attributes have different type annotations",
              flags,
              n1,
              n2)
            return false
          }
        }
        var ar = false
        ar =
          if ((flags & COMPARE_STRING_VALUES) == 0)
            deepEqual(n1.atomize().iterate(),
              n2.atomize().iterate(),
              comparer,
              context,
              0)
          else
            comparer.comparesEqual(new StringValue(n1.getStringValueCS),
              new StringValue(n2.getStringValueCS))
        if (!ar) {
          explain(reporter, "attribute values differ", flags, n1, n2)
          return false
        }
        if ((flags & COMPARE_ID_FLAGS) != 0) {
          if (n1.isId != n2.isId) {
            explain(reporter,
              "one attribute is an ID, the other is not",
              flags,
              n1,
              n2)
            return false
          }
          if (n1.isIdref != n2.isIdref) {
            explain(reporter,
              "one attribute is an IDREF, the other is not",
              flags,
              n1,
              n2)
            return false
          }
        }
        return true
      case Type.PROCESSING_INSTRUCTION | Type.NAMESPACE =>
        if (n1.getLocalPart != n2.getLocalPart) {
          explain(reporter,
            Type.displayTypeName(n1) + " names differ",
            flags,
            n1,
            n2)
          return false
        }
      case Type.TEXT | Type.COMMENT =>
        val vr = comparer.comparesEqual(
          n1.atomize().asInstanceOf[AtomicValue],
          n2.atomize().asInstanceOf[AtomicValue])
        if (! vr && ((flags & WARNING_IF_FALSE) != 0)) {
          val v1 = n1.atomize().getStringValue
          val v2 = n2.atomize().getStringValue
          var message = ""
          if (v1.length != v2.length)
            message = "lengths (" + v1.length + "," + v2.length + ")"
          if (v1.length < 10 && v2.length < 10) {
            message = " (\"" + v1 + "\" vs \"" + v2 + "\")"
          } else {
            val min: Int = Math.min(v1.length, v2.length)
            if (v1.substring(0, min) == v2.substring(0, min)) {
              message += " different at char " + min + "(\"" +
                StringValue.diagnosticDisplay(
                  (if (v1.length > v2.length) v1 else v2).substring(min)) +
                "\")"
            } else if (v1.charAt(0) != v2.charAt(0)) {
              message += " different at start " + "(\"" + v1.substring(
                0,
                Math.min(v1.length, 10)) +
                "\", \"" +
                v2.substring(0, Math.min(v2.length, 10)) +
                "\")"
            } else {
              breakable {
                for (i <- 1 until min
                     if v1.substring(0, i) != v2.substring(0, i)) {
                  message += " different at char " + (i - 1) + "(\"" + v1
                    .substring(i - 1, Math.min(v1.length, i + 10)) +
                    "\", \"" +
                    v2.substring(i - 1, Math.min(v2.length, i + 10)) +
                    "\")"
                  break()
                }
              }
            }
          }
          explain(
            reporter,
            Type.displayTypeName(n1) + " values differ (" + Navigator.getPath(
              n1) +
              ", " +
              Navigator.getPath(n2) +
              "): " +
              message,
            flags,
            n1,
            n2)
        }
        return vr
      case _ => throw new IllegalArgumentException("Unknown node type")

    }
    false
  }

  private def isIgnorable(node: NodeInfo, flags: Int): Boolean = {
    val kind = node.getNodeKind
    if (kind == Type.COMMENT)
      (flags & INCLUDE_COMMENTS) == 0
    else if (kind == Type.PROCESSING_INSTRUCTION)
      (flags & INCLUDE_PROCESSING_INSTRUCTIONS) == 0
    else if (kind == Type.TEXT)
      ((flags & EXCLUDE_WHITESPACE_TEXT_NODES) != 0) && Whitespace.isWhite(node.getStringValueCS)
    else
      false
  }

  private def explain(reporter: ErrorReporter,
                      message: String,
                      flags: Int,
                      n1: NodeInfo,
                      n2: NodeInfo): Unit = {
    if ((flags & WARNING_IF_FALSE) != 0) {
      reporter.report(
        new XmlProcessingIncident(
          "deep-equal() " +
            (if (n1 != null && n2 != null)
              "comparing " + Navigator.getPath(n1) + " to " + Navigator
                .getPath(n2) +
                ": "
            else ": ") +
            message).asWarning())
    }
  }

  private def showKind(item: Item): String =
    item match {
      case nodeInfo: NodeInfo if Whitespace.isWhite(item.getStringValueCS) && nodeInfo.getNodeKind == Type.TEXT =>
        "whitespace text() node"
      case _ =>
        Type.displayTypeName(item)
    }

  private def showNamespaces(bindings: HashSet[NamespaceBinding]): String = {
    val sb = new FastStringBuffer(256)
    for (binding <- bindings.asScala) {
      sb.append(binding.getPrefix)
      sb.append("=")
      sb.append(binding.getURI)
      sb.append(" ")
    }
    sb.setLength(sb.length - 1)
    sb.toString
  }

  private def mergeAdjacentTextNodes(in: SequenceIterator): SequenceIterator = {
    val items = new ArrayList[Item](20)
    var prevIsText = false
    val textBuffer = new FastStringBuffer(FastStringBuffer.C64)
    breakable {
      while (true) {
        val next = in.next()
        if (next == null)
          break()
        next match {
          case nodeInfo: NodeInfo if nodeInfo.getNodeKind == Type.TEXT =>
            textBuffer.cat(next.getStringValueCS)
            prevIsText = true
          case _ =>
            if (prevIsText) {
              val textNode = new Orphan(null)
              textNode.setNodeKind(Type.TEXT)
              textNode.setStringValue(textBuffer.toString)
              items.add(textNode)
              textBuffer.setLength(0)
            }
            prevIsText = false
            items.add(next)
        }
      }
    }
    if (prevIsText) {
      val textNode = new Orphan(null)
      textNode.setNodeKind(Type.TEXT)
      textNode.setStringValue(textBuffer.toString)
      items.add(textNode)
    }
    val extent = new SequenceExtent(items)
    extent.iterate()
  }
}

class DeepEqual extends CollatingFunctionFixed {

  def call(context: XPathContext, arguments: Array[Sequence]): BooleanValue = {
    val comparer: GenericAtomicComparer =
      new GenericAtomicComparer(getStringCollator, context)
    val b: Boolean = deepEqual(arguments(0).iterate(),
      arguments(1).iterate(),
      comparer,
      context,
      0)
    BooleanValue.get(b)
  }

  override def getStreamerName: String = "DeepEqual"
}
