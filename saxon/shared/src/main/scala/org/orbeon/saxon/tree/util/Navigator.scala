////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// Copyright (c) 2018-2020 Saxonica Limited
// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is "Incompatible With Secondary Licenses", as defined by the Mozilla Public License, v. 2.0.
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
package org.orbeon.saxon.tree.util

import java.net.{URI, URISyntaxException}
import java.util
import java.util.function.Predicate

import org.orbeon.saxon.event.{Outputter, Receiver, ReceiverOption}
import org.orbeon.saxon.expr.{Expression, XPathContext}
import org.orbeon.saxon.lib.NamespaceConstant
import org.orbeon.saxon.model._
import org.orbeon.saxon.om._
import org.orbeon.saxon.pattern._
import org.orbeon.saxon.s9api.Location
import org.orbeon.saxon.trans.XPathException
import org.orbeon.saxon.tree.iter._
import org.orbeon.saxon.tree.tiny.{TinyElementImpl, TinyNodeImpl, TinyTextualElement}
import org.orbeon.saxon.tree.wrapper.{SiblingCountingNode, VirtualCopy}

import scala.annotation.tailrec
//import scala.collection.compat._
import scala.jdk.CollectionConverters._
import scala.util.control.Breaks._

/**
 * The Navigator class provides helper classes for navigating a tree, irrespective
 * of its implementation
 *
 * @author Michael H. Kay
 */
object Navigator {
  /**
   * Get the string value of an attribute of a given element, given the URI and
   * local part of the attribute name.
   *
   * @param element   the element on which the required attribute appears
   * @param uri       The namespace URI of the attribute name.
   *                  The "no namespace" case is represented as an empty string.
   * @param localName The local part of the attribute name.
   * @return the attribute value, or null if the attribute is not present
   * @since 9.0
   */
  def getAttributeValue(/*@NotNull*/ element: NodeInfo, uri: String, localName: String): String =
    element.getAttributeValue(uri, localName)

  /**
   * Get the string value of an inherited attribute of a given element, given the URI and
   * local part of the attribute name. If the element does not have such an attribute, go up
   * the ancestor axis to find such an attribute
   *
   * @param element   the element where the search should start
   * @param uri       The namespace URI of the attribute name.
   *                  The "no namespace" case is represented as an empty string.
   * @param localName The local part of the attribute name.
   * @return the attribute value, or null if the attribute is not present on any ancestor
   * @since 9.9
   */
  def getInheritedAttributeValue(element: NodeInfo, uri: String, localName: String): String = {
    var node = element
    while (node != null) {
      val value = node.getAttributeValue(uri, localName)
      if (value == null)
        node = node.getParent
      else
        return value
    }
    null
  }

  /**
   * Helper method to get the name of a node as a structuredQName. Used from bytecode
   *
   * @param node the node
   * @return the name of the node if it has a name, or null otherwise
   */
  def getNodeName(node: NodeInfo): StructuredQName =
    if (node.getLocalPart != null)
      new StructuredQName(node.getPrefix, node.getURI, node.getLocalPart)
    else
      null

  /**
   * Helper method to get the outermost element of a document, given the document node
   *
   * @param doc the document node at the root of the document
   * @return the first element child of the document node, if there is one, else null. This
   *         is often referred to as the "root element" or "document element". No error is reported
   *         if the document node has multiple element children, which can happen in a document
   *         constructed using XSLT or XQuery.
   * @since 9.3
   */
  /*@Nullable*/
  def getOutermostElement(doc: TreeInfo): NodeInfo =
    doc.getRootNode.iterateAxis(AxisInfo.CHILD, NodeKindTest.ELEMENT).next()

  /**
   * Helper method to get the base URI of an element or processing instruction node
   *
   * @param node the node whose base URI is required
   * @return the base URI of the node
   * @since 8.7
   */
  def getBaseURI(node: NodeInfo): String = getBaseURI(node, (n: NodeInfo) => {
    def foo(n: NodeInfo): Boolean = {
      val parent = n.getParent
      parent == null || !(parent.getSystemId == n.getSystemId)
    }

    foo(n)
  })

  /**
   * Helper method to get the base URI of an element or processing instruction node
   *
   * @param node                     the node whose base URI is required
   * @param isTopElementWithinEntity a predicate applied to a node that can be tested to determine
   *                                 if the node is an element that originated as the outermost element within
   *                                 an external entity. If this is the case, the rules for determining
   *                                 the base URI are slightly different; in particular, any @xml:base
   *                                 attribute on such a node is interpreted relative to the system ID of
   *                                 the entity, not relative to the base URI of the parent node.
   * @return the base URI of the node
   * @since 9.9
   */
  def getBaseURI(node: NodeInfo, isTopElementWithinEntity: Predicate[NodeInfo]): String = {
    val xmlBase = node match {
      case impl: TinyElementImpl => impl.getAttributeValue(StandardNames.XML_BASE)
      case _ => node.getAttributeValue(NamespaceConstant.XML, "base")
    }
    if (xmlBase != null) {
      var baseURI: URI = null
      try {
        baseURI = new URI(xmlBase)
        if (!baseURI.isAbsolute) {
          val parent = node.getParent
          if (parent == null) {
            // We have a parentless element with a relative xml:base attribute.
            // See for example test XQTS fn-base-uri-10 and base-uri-27
            val base = new URI(node.getSystemId)
            val resolved = if (xmlBase.isEmpty) base
            else base.resolve(baseURI)
            return resolved.toString
          }
          val startSystemId = node.getSystemId
          if (startSystemId == null) return null
          val parentSystemId = parent.getSystemId
          var isTopWithinEntity = false
          node match {
            case impl: TinyElementImpl =>
              isTopWithinEntity = impl.getTree.isTopWithinEntity(impl.getNodeNumber)
            case _ =>
              isTopWithinEntity = ! (startSystemId == parentSystemId)
          }
          val base =
            new URI(if (isTopElementWithinEntity.test(node)) startSystemId
          else
              parent.getBaseURI)
          //URI base = new URI(parent.getBaseURI);  //bug 3530
          baseURI =
            if (xmlBase.isEmpty)
              base
            else
              base.resolve(baseURI)
        }
      } catch {
        case _: URISyntaxException =>
          // xml:base is an invalid URI. Just return it as is: the operation that needs the base URI
          // will probably fail as a result.     \
          return xmlBase
      }
      return baseURI.toString
    }
    val startSystemId = node.getSystemId
    if (startSystemId == null)
      return null
    val parent = node.getParent
    if (parent == null)
      return startSystemId
    val parentSystemId = parent.getSystemId
    if (startSystemId == parentSystemId || parentSystemId.isEmpty)
      parent.getBaseURI
    else
      startSystemId
  }

  /**
   * Get an absolute XPath expression that identifies a given node within its document
   *
   * @param node the node whose path is required. If null is supplied,
   *             an empty string is returned - this fact is used in making a recursive call
   *             for a parentless node.
   * @return a path expression that can be used to retrieve the node
   */
  def getPath(node: NodeInfo): String = getPath(node, null)

  /**
   * Get an absolute XPath expression that identifies a given node within its document. The
   * resulting path is intended for human readers, not for software evaluation. It uses lexical
   * QNames (prefix:localname) for element and attribute names, with the original prefix as it
   * appears in the source document. The sibling position of an element (a/b[2]) is included
   * unless the node is a streamed node.
   *
   * @param node    the node whose path is required. If null is supplied,
   *                an empty string is returned - this fact is used in making a recursive call
   *                for a parentless node.
   * @param context the XPath dynamic evaluation context. May be null if no context is known
   * @return a path expression that can be used to retrieve the node
   */
  def getPath(node: NodeInfo, context: XPathContext): String = {
    if (node == null)
      return ""
    var pre: String = null
    val streamed = node.getConfiguration.isStreamedNode(node)
    val parent = node.getParent
    // System.err.println("node = " + node + " parent = " + parent);
    node.getNodeKind match {
      case Type.DOCUMENT =>
        "/"
      case Type.ELEMENT =>
        if (parent == null)
          node.getDisplayName
        else {
          pre = getPath(parent, context)
          if (pre == "/")
            "/" + node.getDisplayName
          else
            pre + "/" + node.getDisplayName + (if (streamed) "" else "[" + getNumberSimple(node, context).toString + "]")
        }
      case Type.ATTRIBUTE =>
        getPath(parent, context) + "/@" + node.getDisplayName
      case Type.TEXT =>
        pre = getPath(parent, context)
        (if (pre == "/") "" else pre) + "/text()" + (if (streamed) "" else "[" + getNumberSimple(node, context) + "]")
      case Type.COMMENT =>
        pre = getPath(parent, context)
        (if (pre == "/") "" else pre) + "/comment()" + (if (streamed) "" else "[" + getNumberSimple(node, context) + "]")
      case Type.PROCESSING_INSTRUCTION =>
        pre = getPath(parent, context)
        (if (pre == "/") "" else pre) + "/processing-instruction()" + (if (streamed) "" else "[" + getNumberSimple(node, context) + "]")
      case Type.NAMESPACE =>
        var test = node.getLocalPart
        if (test.isEmpty)
          // default namespace: need a node-test that selects unnamed nodes only
          test = "*[not(local-name()]"
        getPath(parent, context) + "/namespace::" + test
      case _ =>
        ""
    }
  }

  /**
   * Get the absolute path to a node
   *
   * @param node the node in question
   * @return an object representing the path to the node
   */
  def getAbsolutePath(node: NodeInfo): AbsolutePath = {
    var nodeInfo = node
    val streamed = nodeInfo.getConfiguration.isStreamedNode(nodeInfo)
    val path = new util.LinkedList[AbsolutePath.PathElement]
    val sysId = nodeInfo.getSystemId
    while (nodeInfo != null && node.getNodeKind != Type.DOCUMENT) {
      path.add(0, new AbsolutePath.PathElement(node.getNodeKind, NameOfNode.makeName(node), if (streamed) -1
      else getNumberSimple(node, null)))
      nodeInfo = nodeInfo.getParent
    }
    val a = new AbsolutePath(path)
    a.setSystemId(sysId)
    a
  }

  /**
   * Ask whether two nodes have the same name
   *
   * @param n1 the first node
   * @param n2 the second node
   * @return true if they have the same namespace and local part
   */
  def haveSameName(n1: NodeInfo, n2: NodeInfo): Boolean =
    if (n1.hasFingerprint && n2.hasFingerprint)
      n1.getFingerprint == n2.getFingerprint
    else
      n1.getLocalPart == n2.getLocalPart && n1.getURI == n2.getURI

  /**
   * Get simple node number. This is defined as one plus the number of previous siblings of the
   * same node type and name. It is not accessible directly in XSL.
   *
   * @param node    The node whose number is required
   * @param context Used for remembering previous result, for
   *                performance. May be null.
   * @return the node number, as defined above
   */
  def getNumberSimple(node: NodeInfo, context: XPathContext): Int = { //checkNumberable(node);
    var same: NodeTest = null
    if (node.getLocalPart.isEmpty)
      same = NodeKindTest.makeNodeKindTest(node.getNodeKind)
    else
      same = new SameNameTest(node)
    val controller = if (context == null) null else context.getController
    val preceding = node.iterateAxis(AxisInfo.PRECEDING_SIBLING, same)
    var i = 1
    breakable {
      while (true) {
        val prev = preceding.next()
        if (prev == null)
          break()
        if (controller != null) {
          var memo = controller.getRememberedNumber(prev)
          if (memo > 0) {
            memo += i
            controller.setRememberedNumber(node, memo)
            return memo
          }
        }
        i += 1
      }
    }
    if (controller != null)
      controller.setRememberedNumber(node, i)
    i
  }

  /*
    @throws[XPathException]
    def getNumberSingle(node: NodeInfo, count: Pattern, from: Pattern, context: XPathContext): Int = {
      var patternCount = count
      if (patternCount == null && from == null) return getNumberSimple(node, context)
      var knownToMatch = false
      if (patternCount == null) {
        if (node.getLocalPart.isEmpty) { // unnamed node
          patternCount = new NodeTestPattern(NodeKindTest.makeNodeKindTest(node.getNodeKind))
        }
        else patternCount = new NodeTestPattern(new SameNameTest(node))
        knownToMatch = true
      }
      var target = node
      // code changed in 9.5 to fix issue described in spec bug 9840
      if (!knownToMatch)
        while (true)
          if (patternCount.matches(target, context)) if (from == null) break
        else { // see whether there is an ancestor node that matches the from pattern
          var anc = target
          while ( {
            !from.matches(anc, context)
          }) {
            anc = anc.getParent
            if (anc == null) { // there's no ancestor that matches the "from" pattern
              return 0
            }
          }
          // we've found the node to be counted
          break //todo: break is not supported
        }
        else if (from != null && from.matches(target, context)) { // if we find something that matches "from" before we find something that matches "count", exit
          return 0
        }
        else {
          target = target.getParent
          if (target == null) { // found the root before finding a match on either "count" or "from"
            return 0
          }
        }
      // we've found the ancestor to count from
      val preceding = target.iterateAxis(AxisInfo.PRECEDING_SIBLING, getNodeTestForPattern(count))
      // pass the filter condition down to the axis enumeration where possible
      val alreadyChecked = count.isInstanceOf[NodeTestPattern]
      var i = 1
      while ( {
        true
      }) {
        val p = preceding.next.asInstanceOf[NodeInfo]
        if (p == null) return i
        if (alreadyChecked || count.matches(p, context)) i += 1
      }
    }
  */

  /**
   * Get node number (level="any").
   * Return one plus the number of previous nodes in the
   * document that match the supplied pattern
   *
   * @param inst                   Identifies the xsl:number expression; this is relevant
   *                               when the function is memoised to support repeated use of the same
   *                               instruction to number multiple nodes
   * @param node                   The node being numbered
   * @param count                  Pattern that identifies which nodes should be
   *                               counted. Default (null) is the element name if the current node is
   *                               an element, or "node()" otherwise.
   * @param from                   Pattern that specifies where counting starts from.
   *                               Default (null) is the root node. Only nodes at or after the first (most
   *                               recent) node that matches the 'from' pattern are counted.
   * @param context                The dynamic context for the transformation
   * @param hasVariablesInPatterns if the count or from patterns
   *                               contain variables, then it's not safe to get the answer by adding
   *                               one to the number of the most recent node that matches
   * @return one plus the number of nodes that precede the current node,
   *         that match the count pattern, and that follow the first node that
   *         matches the from pattern if specified.
   * @throws XPathException
   * if any dynamic error occurs
   */
  @throws[XPathException]
  def getNumberAny(inst: Expression, node: NodeInfo, count: Pattern, from: Pattern, context: XPathContext, hasVariablesInPatterns: Boolean): Int = {
    var patternCount = count
    var memoNode: NodeInfo = null
    var memoNumber = 0
    val controller = context.getController
    assert(controller != null)
    val memoise = ! hasVariablesInPatterns && from == null
    if (memoise) {
      val memo = controller.getUserData(inst.getLocation, "xsl:number").asInstanceOf[Array[AnyRef]]
      if (memo != null) {
        memoNode = memo(0).asInstanceOf[NodeInfo]
        memoNumber = memo(1).asInstanceOf[Integer]
      }
    }
    var num = 0
    if (patternCount == null) {
      if (node.getLocalPart.isEmpty)
        patternCount = new NodeTestPattern(NodeKindTest.makeNodeKindTest(node.getNodeKind))
      else
        patternCount = new NodeTestPattern(new SameNameTest(node))
      num = 1
    } else if (count.matches(node, context))
      num = 1
    // We use a special axis invented for the purpose: the union of the preceding and
    // ancestor axes, but in reverse document order
    // Pass part of the filtering down to the axis iterator if possible
    var filter: NodeTest = null
    if (from == null)
      filter = getNodeTestForPattern(count)
    else if ((from.getUType eq UType.ELEMENT) && (count.getUType eq UType.ELEMENT))
      filter = NodeKindTest.ELEMENT
    else
      filter = AnyNodeTest.getInstance
    if (from != null && from.matches(node, context))
      return num
    val preceding = node.iterateAxis(AxisInfo.PRECEDING_OR_ANCESTOR, filter)
    breakable {
      while (true) {
        val prev: NodeInfo = preceding.next()
        if (prev == null) break()
        if (count.matches(prev, context)) {
          if (num == 1 && prev == memoNode) {
            num = memoNumber + 1
            break()
          }
          num += 1
        }
        if (from != null && from.matches(prev, context))
          break()
      }
    }
    if (memoise) {
      val memo = (node, num)
      controller.setUserData(inst.getLocation, "xsl:number", memo)
    }
    num
  }

  /*
  @throws[XPathException]
  def getNumberMulti(node: NodeInfo, count: Pattern, from: Pattern, context: XPathContext) = {
    var patternCount = count
    val v = new util.ArrayList[Long](5)
    if (patternCount == null) if (node.getLocalPart.isEmpty) patternCount = new NodeTestPattern(NodeKindTest.makeNodeKindTest(node.getNodeKind))
    else patternCount = new NodeTestPattern(new SameNameTest(node))
    var curr = node
    breakable {
      while (true) {
        if (patternCount.matches(curr, context)) {
          val num = Navigator.getNumberSingle(curr, patternCount, null, context)
          v.add(0, num.toLong)
        }
        if (from != null && from.matches(curr, context)) break
        curr = curr.getParent
        if (curr == null) break
      }
    }
    v
  }*/

  /**
   * Get a NodeTest to use as a filter for nodes, given a pattern.
   */
  private def getNodeTestForPattern(pattern: Pattern): NodeTest = {
    val itemType = pattern.getItemType
    itemType match {
      case test: NodeTest => test
      case _ =>
        if (pattern.getUType.overlaps(UType.ANY_NODE))
          AnyNodeTest.getInstance
        else
          ErrorType
    }
  }

  /**
   * Generic (model-independent) implementation of deep copy algorithm for nodes.
   * This is available for use by any node implementations that choose to use it.
   *
   * @param node        The node to be copied.
   * @param out         The receiver to which events will be sent
   * @param copyOptions Options for copying namespaces, type annotations, etc,
   *                    as defined in { @link org.orbeon.saxon.om.CopyOptions}
   * @param locationId  The location of the instruction invoking the copy
   * @throws XPathException           on any failure reported by the Receiver
   * @throws IllegalArgumentException if the node is an attribute or namespace node
   */
  @throws[XPathException]
  def copy(node: NodeInfo, out: Receiver, copyOptions: Int, locationId: Location): Unit = node.getNodeKind match {
    case Type.DOCUMENT =>
      out.startDocument(CopyOptions.getStartDocumentProperties(copyOptions))
      for (child <- node.children)
        child.copy(out, copyOptions, locationId)
      out.endDocument()
    case Type.ELEMENT =>
      val annotation =
        if ((copyOptions & CopyOptions.TYPE_ANNOTATIONS) != 0)
          node.getSchemaType
        else
          Untyped.getInstance
      val ns =
        if (CopyOptions.includes(copyOptions, CopyOptions.ALL_NAMESPACES))
          node.getAllNamespaces
        else
          NamespaceMap.emptyMap
      out.startElement(NameOfNode.makeName(node), annotation, node.attributes, ns, locationId, ReceiverOption.BEQUEATH_INHERITED_NAMESPACES_ONLY | ReceiverOption.NAMESPACE_OK)
      // output the children

      for (child <- node.children)
        child.copy(out, copyOptions, locationId)
      // finally the end tag
      out.endElement()
    case Type.ATTRIBUTE =>
      throw new IllegalArgumentException("Cannot copy attribute to Receiver")
    //                SimpleType annotation = (copyOptions & CopyOptions.TYPE_ANNOTATIONS) != 0 ?
    //                        (SimpleType) node.getSchemaType :
    //                        BuiltInAtomicType.UNTYPED_ATOMIC;
    //                ((ComplexContentOutputter)out).attribute(NameOfNode.makeName(node), annotation, node.getStringValueCS, locationId, ReceiverOption.NONE);
    //                return;
    case Type.TEXT =>
      val value = node.getStringValueCS
      if (value.length != 0) // zero-length text nodes can arise from external model wrappers
        out.characters(value, locationId, ReceiverOption.NONE)
    case Type.COMMENT =>
      out.comment(node.getStringValueCS, locationId, ReceiverOption.NONE)
    case Type.PROCESSING_INSTRUCTION =>
      out.processingInstruction(node.getLocalPart, node.getStringValueCS, locationId, ReceiverOption.NONE)
    case Type.NAMESPACE =>
      throw new IllegalArgumentException("Cannot copy namespace to Receiver")
    //                out.namespacesOLD(NamespaceMap.of(node.getLocalPart, node.getStringValue), ReceiverOption.NONE);
    case _ =>
  }

  /**
   * Generic (model-independent) implementation of deep copy algorithm for nodes.
   * This is available for use by any node implementations that choose to use it.
   *
   * @param node        The node to be copied.
   * @param out         The outputter to which events will be sent
   * @param copyOptions Options for copying namespaces, type annotations, etc,
   *                    as defined in { @link org.orbeon.saxon.om.CopyOptions}
   * @param locationId  The location of the instruction invoking the copy
   * @throws XPathException on any failure reported by the Receiver
   */
  @throws[XPathException]
  def copy(node: NodeInfo, out: Outputter, copyOptions: Int, locationId: Location): Unit = {
    val keepTypes = (copyOptions & CopyOptions.TYPE_ANNOTATIONS) != 0
    node.getNodeKind match {
      case Type.DOCUMENT =>
        out.startDocument(CopyOptions.getStartDocumentProperties(copyOptions))
        for (child <- node.children)
          copy(child, out, copyOptions, locationId)
        out.endDocument()
      case Type.ELEMENT =>
        val annotation = if (keepTypes) node.getSchemaType else Untyped.getInstance
        out.startElement(NameOfNode.makeName(node), annotation, locationId, ReceiverOption.DISINHERIT_NAMESPACES | ReceiverOption.NAMESPACE_OK)
        if ((copyOptions & CopyOptions.ALL_NAMESPACES) != 0)
          for (ns <- node.getAllNamespaces.asScala)
            out.namespace(ns.getPrefix, ns.getURI, ReceiverOption.NONE)

        for (attr <- node.attributes) {
          val attType = if (keepTypes) attr.getType else BuiltInAtomicType.UNTYPED_ATOMIC
          out.attribute(attr.getNodeName, attType, attr.getValue, attr.getLocation, attr.getProperties)
        }

        for (child <- node.children)
          copy(child, out, copyOptions, locationId)
        out.endElement()
      case Type.ATTRIBUTE =>
        val attType = if (keepTypes) node.getSchemaType.asInstanceOf[SimpleType] else BuiltInAtomicType.UNTYPED_ATOMIC
        out.attribute(NameOfNode.makeName(node), attType, node.getStringValueCS, locationId, ReceiverOption.NONE)
      case Type.TEXT =>
        val value = node.getStringValueCS
        if (value.length != 0)
          out.characters(value, locationId, ReceiverOption.NONE)
      case Type.COMMENT =>
        out.comment(node.getStringValueCS, locationId, ReceiverOption.NONE)
      case Type.PROCESSING_INSTRUCTION =>
        out.processingInstruction(node.getLocalPart, node.getStringValueCS, locationId, ReceiverOption.NONE)
      case Type.NAMESPACE =>
        out.namespace(node.getLocalPart, node.getStringValue, ReceiverOption.NONE)
      case _ =>
    }
  }

  /**
   * Generic (model-independent) method to determine the relative position of two
   * nodes in document order. The nodes must be in the same tree.
   *
   * @param first  The first node
   * @param second The second node, whose position is to be compared with the first node
   * @return -1 if this node precedes the other node, +1 if it follows the other
   *         node, or 0 if they are the same node. (In this case, isSameNode() will always
   *         return true, and the two nodes will produce the same result for generateId())
   */
  def compareOrder(first: SiblingCountingNode, second: SiblingCountingNode): Int = {

    // are they the same node?
    if (first == second)
      return 0

    val firstParent = first.getParent
    if (firstParent == null) // first node is the root
      return -1

    val secondParent = second.getParent
    if (secondParent == null) // second node is the root
      return +1

    // do they have the same parent (common case)?
    if (firstParent == secondParent) {
      val cat1 = nodeCategories(first.getNodeKind)
      val cat2 = nodeCategories(second.getNodeKind)
      if (cat1 == cat2)
        return first.getSiblingPosition - second.getSiblingPosition
      else
        return cat1 - cat2
    }

    // find the depths of both nodes in the tree
    var depth1 = 0
    var depth2 = 0
    var p1: NodeInfo = first
    var p2: NodeInfo = second
    while (p1 != null) {
      depth1 += 1
      p1 = p1.getParent
    }
    while (p2 != null) {
      depth2 += 1
      p2 = p2.getParent
    }

    // move up one branch of the tree so we have two nodes on the same level
    p1 = first
    while (depth1 > depth2) {
      p1 = p1.getParent
      assert(p1 != null)
      if (p1 == second)
        return +1
      depth1 -= 1
    }
    p2 = second
    while (depth2 > depth1) {
      p2 = p2.getParent
      assert(p2 != null)
      if (p2 == first)
        return -1
      depth2 -= 1
    }

    // now move up both branches in sync until we find a common parent
    while (true) {
      val par1 = p1.getParent
      val par2 = p2.getParent
      if (par1 == null || par2 == null)
        throw new NullPointerException("Node order comparison - internal error")
      if (par1 == par2) {
        if (p1.getNodeKind == Type.ATTRIBUTE && p2.getNodeKind != Type.ATTRIBUTE)
          return -1 // attributes first
        if (p1.getNodeKind != Type.ATTRIBUTE && p2.getNodeKind == Type.ATTRIBUTE)
          return +1
        return p1.asInstanceOf[SiblingCountingNode].getSiblingPosition - p2.asInstanceOf[SiblingCountingNode].getSiblingPosition
      }
      p1 = par1
      p2 = par2
    }
    0
  }

  /**
   * Generic (model-independent) method to determine the relative position of two
   * node in document order. The nodes must be in the same tree.
   *
   * @param first  The first node
   * @param second The second node, whose position is to be compared with the first node
   * @return { @link org.orbeon.saxon.om.AxisInfo#PRECEDING} if this node is on the preceding axis of the other node;
   *         { @link org.orbeon.saxon.om.AxisInfo#FOLLOWING} if it is on the following axis; { @link org.orbeon.saxon.om.AxisInfo#ANCESTOR} if the first node is an
   *         ancestor of the second; { @link org.orbeon.saxon.om.AxisInfo#DESCENDANT} if the first is a descendant of the second;
   *         { @link org.orbeon.saxon.om.AxisInfo#SELF} if they are the same node.
   * @throws UnsupportedOperationException if either node is an attribute or namespace
   * @since 9.5
   */
  def comparePosition(first: NodeInfo, second: NodeInfo): Int = {
    if (first.getNodeKind == Type.ATTRIBUTE || first.getNodeKind == Type.NAMESPACE || second.getNodeKind == Type.ATTRIBUTE || second.getNodeKind == Type.NAMESPACE)
      throw new UnsupportedOperationException
    if (first == second)
      return AxisInfo.SELF
    val firstParent = first.getParent
    if (firstParent == null)
      return AxisInfo.ANCESTOR
    val secondParent = second.getParent
    if (secondParent == null)
      return AxisInfo.DESCENDANT
    if (firstParent == secondParent)
      if (first.compareOrder(second) < 0)
        return AxisInfo.PRECEDING
      else
        return AxisInfo.FOLLOWING
    var depth1 = 0
    var depth2 = 0
    var p1 = first
    var p2 = second
    while ( {
      p1 != null
    }) {
      depth1 += 1
      p1 = p1.getParent
    }
    while ( {
      p2 != null
    }) {
      depth2 += 1
      p2 = p2.getParent
    }
    // Test if either node is an ancestor of the other
    p1 = first
    while ( {
      depth1 > depth2
    }) {
      p1 = p1.getParent
      assert(p1 != null)
      if (p1 == second)
        return AxisInfo.DESCENDANT
      depth1 -= 1
    }
    p2 = second
    while ( {
      depth2 > depth1
    }) {
      p2 = p2.getParent
      assert(p2 != null)
      if (p2 == first)
        return AxisInfo.ANCESTOR
      depth2 -= 1
    }
    // now delegate to compareOrder()
    if (first.compareOrder(second) < 0)
      AxisInfo.PRECEDING
    else
      AxisInfo.FOLLOWING
  }

  /**
   * Classify node kinds into categories for sorting into document order:
   * 0 = document, 1 = namespace, 2 = attribute, 3 = (element, text, comment, pi)
   */
  private val nodeCategories = Array(-1, //0 = not used
    3, //1 = element
    2, //2 = attribute
    3, //3 = text
    -1, -1, -1, //4,5,6 = not used
    3, //7 = processing-instruction
    3, //8 = comment
    0, //9 = document
    -1, -1, -1, //10,11,12 = not used
    1 //13 = namespace
  )

  /**
   * Get a character string that uniquely identifies this node and that collates nodes
   * into document order
   *
   * @param node     the node whose unique identifier is reuqired
   * @param sb       a buffer to which the unique identifier will be appended
   * @param addDocNr true if a unique document number is to be included in the information
   */
  def appendSequentialKey(node: SiblingCountingNode, sb: FastStringBuffer, addDocNr: Boolean): Unit = {
    if (addDocNr) {
      sb.cat('w')
      sb.append(node.getTreeInfo.getDocumentNumber.toString)
    }
    if (node.getNodeKind != Type.DOCUMENT) {
      val parent = node.getParent
      if (parent != null)
        appendSequentialKey(parent.asInstanceOf[SiblingCountingNode], sb, addDocNr = false)
      if (node.getNodeKind == Type.ATTRIBUTE)
        sb.cat('A')
    }
    sb.append(alphaKey(node.getSiblingPosition))
  }

  /**
   * Construct an alphabetic key from an positive integer; the key collates in the same sequence
   * as the integer
   *
   * @param value The positive integer key value (negative values are treated as zero).
   * @return the alphabetic key value
   */
  def alphaKey(value: Int): String = {
    if (value < 1) return "a"
    if (value < 10) return "b" + value
    if (value < 100) return "c" + value
    if (value < 1000) return "d" + value
    if (value < 10000) return "e" + value
    if (value < 100000) return "f" + value
    if (value < 1000000) return "g" + value
    if (value < 10000000) return "h" + value
    if (value < 100000000) return "i" + value
    if (value < 1000000000) return "j" + value
    "k" + value
  }

  /**
   * Test if one node is an ancestor-or-self of another
   *
   * @param a the putative ancestor-or-self node
   * @param d the putative descendant node
   * @return true if a is an ancestor-or-self of d
   */
  def isAncestorOrSelf(a: NodeInfo, d: NodeInfo): Boolean = {
    val k = a.getNodeKind
    if (k != Type.ELEMENT && k != Type.DOCUMENT) return a == d
    // Fast path for the TinyTree implementation
    a match {
      case impl: TinyNodeImpl =>
        d match {
          case impl1: TinyNodeImpl => return impl.isAncestorOrSelf(impl1)
          case _: TinyTextualElement#TinyTextualElementText => return a == d || isAncestorOrSelf(a, d.getParent)
          case _ =>
            if (d.getNodeKind == Type.NAMESPACE) {
              // fall through
            } else if (d.isInstanceOf[VirtualCopy]) {
            } else
              return false
        }
      case _ =>
    }
    // Generic implementation
    var p = d
    while ( {
      p != null
    }) {
      if (a == p)
        return true
      p = p.getParent
    }
    false
  }

  /**
   * Create an iterator over a singleton node, if it exists and matches a nodetest;
   * otherwise return an empty iterator
   *
   * @param node     the singleton node, or null if the node does not exist
   * @param nodeTest the test to be applied
   * @return an iterator over the node if it exists and matches the test.
   */
  def filteredSingleton(node: NodeInfo, nodeTest: Predicate[_ >: NodeInfo]): AxisIterator =
    if (node != null && nodeTest.test(node))
      SingleNodeIterator.makeIterator(node)
    else
      EmptyIterator.ofNodes

  /**
   * Get the sibling position of a node: specifically, count how many preceding siblings
   * of a node satisfy the nodetest.
   *
   * @param node     the starting node, which is assumed to satisfy the node test
   * @param nodeTest the node test
   * @param max      the maximum number of nodes to be counted
   * @return the number of preceding siblings that satisfy the node test, plus one, unless the
   *         number exceeds max, in which case return some number greater than or equal to max.
   */
  def getSiblingPosition(node: NodeInfo, nodeTest: NodeTest, max: Int): Int = {
    node match {
      case node1: SiblingCountingNode if nodeTest.isInstanceOf[AnyNodeTest] =>
        return node1.getSiblingPosition
      case _ =>
    }
    val prev = node.iterateAxis(AxisInfo.PRECEDING_SIBLING, nodeTest)
    var count = 1
    while ( {
      prev.next != null
    }) if ( {
      count += 1
      count
    } > max)
      return count
    count
  }

  /**
   * AxisFilter is an iterator that applies a NodeTest filter to
   * the nodes returned by an underlying AxisIterator.
   */
  class AxisFilter(var base: AxisIterator, var nodeTest: Predicate[_ >: NodeInfo]) extends AxisIterator {

    override def next(): NodeInfo = {
      while (true) {
        val next = base.next()
        if (next == null)
          return null
        if (nodeTest.test(next))
          return next
      }
      null
    }
  }

  /**
   * EmptyTextFilter is an iterator that applies removes any zero-length text
   * nodes returned by an underlying AxisIterator.
   */
  class EmptyTextFilter(var base: AxisIterator) extends AxisIterator {
    override def next(): NodeInfo = {
      while (true) {
        val next = base.next()
        if (next == null)
          return null
        if (!(next.getNodeKind == Type.TEXT && next.getStringValueCS.length == 0))
          return next
      }
      null
    }
  }

  /**
   * General-purpose implementation of the ancestor and ancestor-or-self axes
   */
  final class AncestorEnumeration(var current: NodeInfo, var includeSelf: Boolean) extends AxisIterator {

    private var atStart: Boolean = true

    override def next(): NodeInfo = {
      if (atStart) {
        atStart = false
        if (includeSelf)
          return current
      }
      current =
        if (current == null)
          null
        else
          current.getParent
      current
    }

    // end of class AncestorEnumeration
  }

  /**
   * General-purpose implementation of the descendant and descendant-or-self axes,
   * in terms of the child axis.
   * But it also has the option to return the descendants in reverse document order;
   * this is used when evaluating the preceding axis. Note that the includeSelf option
   * should not be used when scanning in reverse order, as the self node will always be
   * returned first.
   */
  final class DescendantEnumeration(var start: NodeInfo, var includeSelf: Boolean, var forwards: Boolean) extends AxisIterator {
    private var children: AxisIterator = null
    private var descendants: AxisIterator = null
    private var atEnd = false

    @tailrec
    override def next(): NodeInfo = {
      if (descendants != null) {
        val nextd = descendants.next()
        if (nextd != null)
          return nextd
        else
          descendants = null
      }
      if (children != null) {
        val n = children.next()
        if (n != null)
            if (n.hasChildNodes)
              if (forwards) {
              descendants = new Navigator.DescendantEnumeration(n, false, true)
              n
            } else {
              descendants = new Navigator.DescendantEnumeration(n, true, false)
              next()
            }
          else
            n
        else if (forwards || !includeSelf)
          null
        else {
          atEnd = true
          children = null
          start
        }
      } else if (atEnd) // we're just finishing a backwards scan
        null
      else { // we're just starting...
        if (start.hasChildNodes) { //children = new NodeWrapper.ChildEnumeration(start, true, forwards);
          children = start.iterateAxis(AxisInfo.CHILD)
          if (!forwards) children match {
            case iterator: ReversibleIterator =>
              children = iterator.getReverseIterator.asInstanceOf[AxisIterator]
            case _ =>
              val list = new util.LinkedList[NodeInfo]
              val forwards = start.iterateAxis(AxisInfo.CHILD)
              var n: NodeInfo = null
              while ( {
                {
                  n = forwards.next(); n
                } != null
              }) list.addFirst(n)
              children = new ListIterator.OfNodes(list)
          }
        } else
          children = EmptyIterator.ofNodes
        if (forwards && includeSelf)
          start
        else
          next()
      }
    }

    def advance(): Unit = ()

    // end of class DescendantEnumeration
  }

  /**
   * General purpose implementation of the following axis, in terms of the
   * ancestor, child, and following-sibling axes
   */
  final class FollowingEnumeration() extends AxisIterator {
    private var ancestorEnum: AxisIterator = null
    private var siblingEnum: AxisIterator = null
    private var descendEnum: AxisIterator = null

    def this(start: NodeInfo) {
      this()
      ancestorEnum = new Navigator.AncestorEnumeration(start, false)
      start.getNodeKind match {
        case Type.ELEMENT =>
        case Type.TEXT =>
        case Type.COMMENT =>
        case Type.PROCESSING_INSTRUCTION =>
          //siblingEnum = new NodeWrapper.ChildEnumeration(start, false, true);
          // gets following siblings
          siblingEnum = start.iterateAxis(AxisInfo.FOLLOWING_SIBLING)
        case Type.ATTRIBUTE =>
        case Type.NAMESPACE =>
          //siblingEnum = new NodeWrapper.ChildEnumeration((NodeWrapper)start.getParent, true, true);
          // gets children of the attribute's parent node
          val parent = start.getParent
          if (parent == null)
            siblingEnum = EmptyIterator.ofNodes
          else
            siblingEnum = parent.iterateAxis(AxisInfo.CHILD)
        case _ =>
          siblingEnum = EmptyIterator.ofNodes
      }
    }


    @tailrec
    override def next(): NodeInfo = {
      if (descendEnum != null) {
        val nextd = descendEnum.next()
        if (nextd != null) return nextd
        else descendEnum = null
      }
      if (siblingEnum != null) {
        val nexts = siblingEnum.next()
        if (nexts != null) {
          if (nexts.hasChildNodes)
            descendEnum = new Navigator.DescendantEnumeration(nexts, false, true)
          else
            descendEnum = null
          return nexts
        } else {
          descendEnum = null
          siblingEnum = null
        }
      }
      val nexta = ancestorEnum.next()
      if (nexta != null) {
        if (nexta.getNodeKind == Type.DOCUMENT)
          siblingEnum = EmptyIterator.ofNodes
        else //siblingEnum = new NodeWrapper.ChildEnumeration(next, false, true);
          siblingEnum = nexta.iterateAxis(AxisInfo.FOLLOWING_SIBLING)
        next()
      } else
        null
    }

    // end of class FollowingEnumeration
  }

  /**
   * Helper method to iterate over the preceding axis, or Saxon's internal
   * preceding-or-ancestor axis, by making use of the ancestor, descendant, and
   * preceding-sibling axes.
   */
  final class PrecedingEnumeration() extends AxisIterator {
    private var ancestorEnum: AxisIterator = null
    private var siblingEnum: AxisIterator = null
    private var descendEnum: AxisIterator = null
    private var includeAncestors: Boolean = false

    def this(start: NodeInfo, includeAncestors: Boolean) {
      this
      this.includeAncestors = includeAncestors
      ancestorEnum = new Navigator.AncestorEnumeration(start, false)
      start.getNodeKind match {
        case Type.ELEMENT =>
        case Type.TEXT =>
        case Type.COMMENT =>
        case Type.PROCESSING_INSTRUCTION =>
          siblingEnum = start.iterateAxis(AxisInfo.PRECEDING_SIBLING)
        case _ =>
          siblingEnum = EmptyIterator.ofNodes
      }
    }


    override def next(): NodeInfo = {
      if (descendEnum != null) {
        val nextd = descendEnum.next()
        if (nextd != null)
          return nextd
        else
          descendEnum = null
      }
      if (siblingEnum != null) {
        val nexts = siblingEnum.next()
        if (nexts != null)
          if (nexts.hasChildNodes) {
            descendEnum = new Navigator.DescendantEnumeration(nexts, true, false)
            return next()
          } else {
            descendEnum = null
            return nexts
          }
        else {
          descendEnum = null
          siblingEnum = null
        }
      }
      val nexta = ancestorEnum.next()
      if (nexta != null) {
        if (nexta.getNodeKind == Type.DOCUMENT)
          siblingEnum = EmptyIterator.ofNodes
        else
          siblingEnum = nexta.iterateAxis(AxisInfo.PRECEDING_SIBLING)
        if (!includeAncestors)
          next()
        else
          nexta
      }
      else null
    }

    // end of class PrecedingEnumeration
  }
}
