////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
package net.sf.saxon.tree.tiny

import net.sf.saxon.utils.Configuration
import net.sf.saxon.event._
import net.sf.saxon.lib.Feature
import net.sf.saxon.model.SchemaType
import net.sf.saxon.model.SimpleType
import net.sf.saxon.model.Type
import net.sf.saxon.om._
import net.sf.saxon.s9api.Location
import net.sf.saxon.trans.XPathException
import net.sf.saxon.tree.util.FastStringBuffer
import java.util.Arrays
import java.util.Stack

import TinyBuilder._
import net.sf.saxon.tree.tiny.TinyBuilder.Eligibility.Eligibility

import scala.beans.{BeanProperty, BooleanBeanProperty}


object TinyBuilder {

  object Eligibility extends Enumeration {

    val INELIGIBLE: Eligibility = new Eligibility()

    val PRIMED: Eligibility = new Eligibility()

    val ELIGIBLE: Eligibility = new Eligibility()

    class Eligibility extends Val

    implicit def convertValue(v: Value): Eligibility =
      v.asInstanceOf[Eligibility]

  }

  private val PARENT_POINTER_INTERVAL: Int = 10

}

class TinyBuilder(pipe: PipelineConfiguration) extends Builder(pipe) {

  /*@Nullable*/

  @BeanProperty
  var tree: TinyTree = _

  private var namespaceStack: Stack[NamespaceMap] = new Stack()

  @BeanProperty
  var currentDepth: Int = 0

  // this is the local sequence within this document
  private var nodeNr: Int = 0

  private var ended: Boolean = false

  private var noNewNamespaces: Boolean = true

  val configur: Configuration = pipe.getConfiguration

  private var statistics: Statistics =
    configur.getTreeStatistics.TEMPORARY_TREE_STATISTICS

  private var markDefaultedAttributes: Boolean = config.isExpandAttributeDefaults &&
    configur.getBooleanProperty(Feature.MARK_DEFAULTED_ATTRIBUTES)

  private var textualElementEligibilityState: Eligibility = Eligibility.INELIGIBLE


  def setStatistics(stats: Statistics): Unit = {
    statistics = stats
  }

  /*@NotNull*/

  private var prevAtDepth: Array[Int] = new Array[Int](100)

  /*@NotNull*/

  private var siblingsAtDepth: Array[Int] = new Array[Int](100)

  private var isIDElement: Boolean = false

  override def open(): Unit = {
    if (started) {
      // this happens when using an IdentityTransformer
      return
    }
    if (tree == null) {
      tree = new TinyTree(config, statistics)
      currentDepth = 0
      if (lineNumbering) {
        tree.setLineNumbering()
      }
      uniformBaseURI = true
      tree.setUniformBaseUri(baseURI)
    }
    super.open()
  }

  def startDocument(properties: Int): Unit = {
    if ((started && !ended) || currentDepth > 0) {
      // the content of an element
      return
    }
    // this happens when using an IdentityTransformer, or when copying a document node to form
    // this happens when using an IdentityTransformer, or when copying a document node to form
    started = true
    ended = false
    val tt: TinyTree = tree
    assert(tt != null)
    currentRoot = new TinyDocumentImpl(tt)
    val doc: TinyDocumentImpl = currentRoot.asInstanceOf[TinyDocumentImpl]
    doc.setSystemId(getSystemId)
    doc.setBaseURI(getBaseURI)
    currentDepth = 0
    val nodeNr: Int =
      tt.addDocumentNode(currentRoot.asInstanceOf[TinyDocumentImpl])
    prevAtDepth(0) = nodeNr
    prevAtDepth(1) = -1
    siblingsAtDepth(0) = 0
    siblingsAtDepth(1) = 0
    tt.next(nodeNr) = -1
    currentDepth += 1
  }

  def endDocument(): Unit = {
    // decrement numberOfNodes so the next node will overwrite it
    tree.addNode(Type.STOPPER, 0, 0, 0, -1)
    tree.numberOfNodes -= 1
    if (currentDepth > 1) {
      return
    }
    if (ended) {
      // happens when using an IdentityTransformer
      return
    }
    ended = true
    prevAtDepth(currentDepth) = -1
    currentDepth -= 1

  }

  override def reset(): Unit = {
    super.reset()
    tree = null
    currentDepth = 0
    nodeNr = 0
    ended = false
    statistics = config.getTreeStatistics.TEMPORARY_TREE_STATISTICS
  }

  override def close(): Unit = {
    val tt: TinyTree = tree
    if (tt != null) {
      tt.addNode(Type.STOPPER, 0, 0, 0, -1)
      tt.condense(statistics)
    }
    super.close()
  }

  def startElement(elemName: NodeName,
                   `type`: SchemaType,
                   attributes: AttributeMap,
                   namespaces: NamespaceMap,
                   location: Location,
                   properties: Int): Unit = {
    //System.err.println(this + " startElement " + elemName + " " + currentDepth);
    val tt: TinyTree = tree
    assert(tt != null)
    textualElementEligibilityState = Eligibility.INELIGIBLE
    noNewNamespaces = true
    if (namespaceStack.isEmpty) {
      noNewNamespaces = false
      namespaceStack.push(namespaces)
    } else {
      noNewNamespaces = namespaces == namespaceStack.peek()
      namespaceStack.push(namespaces)
    }
    if (siblingsAtDepth(currentDepth) > PARENT_POINTER_INTERVAL) {
      nodeNr = tt.addNode(Type.PARENT_POINTER,
        currentDepth,
        prevAtDepth(currentDepth - 1),
        0,
        0)
      val prev: Int = prevAtDepth(currentDepth)
      if (prev > 0) {
        tt.next(prev) = nodeNr
      }
      tt.next(nodeNr) = prevAtDepth(currentDepth - 1)
      prevAtDepth(currentDepth) = nodeNr
      siblingsAtDepth(currentDepth) = 0
    }
    // now add the element node itself
    val fp: Int = elemName.obtainFingerprint(namePool)
    val prefixCode: Int = tree.prefixPool.obtainPrefixCode(elemName.getPrefix)
    val nameCode: Int = (prefixCode << 20) | fp
    nodeNr = tt.addNode(Type.ELEMENT, currentDepth, -1, -1, nameCode)
    isIDElement = ReceiverOption.contains(properties, ReceiverOption.IS_ID)
    val typeCode: Int = `type`.getFingerprint
    if (typeCode != StandardNames.XS_UNTYPED) {
      tt.setElementAnnotation(nodeNr, `type`)
      if (ReceiverOption.contains(properties, ReceiverOption.NILLED_ELEMENT)) {
        tt.setNilled(nodeNr)
      }
      if (!isIDElement && `type`.isIdType) {
        isIDElement = true
      }
    }
    if (currentDepth == 0) {
      prevAtDepth(0) = nodeNr
      prevAtDepth(1) = -1
      currentRoot = tt.getNode(nodeNr)
    } else {
      val prev: Int = prevAtDepth(currentDepth)
      if (prev > 0) {
        tt.next(prev) = nodeNr
      }
      // *O* owner pointer in last sibling
      tt.next(nodeNr) = prevAtDepth(currentDepth - 1)
      prevAtDepth(currentDepth) = nodeNr
      siblingsAtDepth(currentDepth) += 1

    }
    {
      currentDepth += 1;
      currentDepth - 1

    }
    if (currentDepth == prevAtDepth.length) {
      prevAtDepth = Arrays.copyOf(prevAtDepth, currentDepth * 2)
      siblingsAtDepth = Arrays.copyOf(siblingsAtDepth, currentDepth * 2)
    }
    prevAtDepth(currentDepth) = -1
    siblingsAtDepth(currentDepth) = 0
    val localSystemId: String = location.getSystemId
    if (isUseEventLocation && localSystemId != null) {
      tt.setSystemId(nodeNr, localSystemId)
    } else if (currentDepth == 1) {
      tt.setSystemId(nodeNr, systemId)
    }
    if (uniformBaseURI && localSystemId != null && localSystemId != baseURI) {
      uniformBaseURI = false
      tt.setUniformBaseUri(null)
    }
    if (lineNumbering) {
      tt.setLineNumber(nodeNr,
        location.getLineNumber,
        location.getColumnNumber)
    }
    if (location.isInstanceOf[ReceivingContentHandler.LocalLocator] &&
      location
        .asInstanceOf[ReceivingContentHandler.LocalLocator]
        .levelInEntity ==
        0 &&
      currentDepth >= 1) {
      tt.markTopWithinEntity(nodeNr)
    }
    for (att <- attributes) {
      attribute2(att.getNodeName,
        att.getType,
        getAttValue(att),
        location,
        att.getProperties)
    }
    textualElementEligibilityState =
      if (noNewNamespaces) Eligibility.PRIMED else Eligibility.INELIGIBLE
    tree.addNamespaces(nodeNr, namespaceStack.peek())
    nodeNr += 1
  }

  // if the number of siblings exceeds a certain threshold, add a parent pointer, in the form
  // of a pseudo-node
  // if the number of siblings exceeds a certain threshold, add a parent pointer, in the form
  // of a pseudo-node

  def getAttValue(att: AttributeInfo): String = att.getValue

  private def attribute2(attName: NodeName,
                         `type`: SimpleType,
                         value: CharSequence,
                         locationId: Location,
                         properties: Int): Unit = {
    // System.err.println("attribute " + nameCode + "=" + value);
    val fp: Int = attName.obtainFingerprint(namePool)
    val prefix: String = attName.getPrefix
    val nameCode: Int =
      if (prefix.isEmpty) fp
      else (tree.prefixPool.obtainPrefixCode(prefix) << 20) | fp
    assert(tree != null)
    assert(currentRoot != null)
    tree.addAttribute(currentRoot, nodeNr, nameCode, `type`, value, properties)
    if (markDefaultedAttributes &&
      ReceiverOption.contains(properties, ReceiverOption.DEFAULTED_VALUE)) {
      tree.markDefaultedAttribute(tree.numberOfAttributes - 1)
    }
    if (fp == StandardNames.XML_BASE) {
      uniformBaseURI = false
      tree.setUniformBaseUri(null)
    }
  }

  def endElement(): Unit = {
    assert(tree != null)
    val eligibleAsTextualElement: Boolean = textualElementEligibilityState == Eligibility.ELIGIBLE
    textualElementEligibilityState = Eligibility.INELIGIBLE
    prevAtDepth(currentDepth) = -1
    siblingsAtDepth(currentDepth) = 0
    currentDepth -= 1
    namespaceStack.pop()
    if (isIDElement) {
      // we're relying on the fact that an ID element has no element children!
      tree.indexIDElement(currentRoot, prevAtDepth(currentDepth))
      isIDElement = false
    } else if (eligibleAsTextualElement && tree.nodeKind(nodeNr) == Type.TEXT &&
      tree.nodeKind(nodeNr - 1) == Type.ELEMENT &&
      tree.alpha(nodeNr - 1) == -1 &&
      noNewNamespaces) {

      tree.nodeKind(nodeNr - 1) = Type.TEXTUAL_ELEMENT.toByte
      tree.alpha(nodeNr - 1) = tree.alpha(nodeNr)
      tree.beta(nodeNr - 1) = tree.beta(nodeNr)

      nodeNr -= 1
      tree.numberOfNodes -= 1

      if (currentDepth == 0) {
        currentRoot = tree.getNode(nodeNr)
      }
    }
    // Collapse a simple element with text content and no attributes or namespaces into a single node
    // of type TRIVIAL_ELEMENT
    // Collapse a simple element with text content and no attributes or namespaces into a single node
    // of type TRIVIAL_ELEMENT
  }

  //System.err.println(this + " end element " + currentDepth);
  //System.err.println(this + " end element " + currentDepth);

  def getLastCompletedElement(): TinyNodeImpl = {
    if (tree == null) {
      null
    }
    tree.getNode(if (currentDepth >= 0) prevAtDepth(currentDepth) else 0)
  }

  // Note: reading an incomplete tree needs care if it constructs a prior index, etc.
  // Note: reading an incomplete tree needs care if it constructs a prior index, etc.

  def characters(chars: CharSequence,
                 locationId: Location,
                 properties: Int): Unit = {
    //System.err.println("characters: " + chars);
    if (chars.isInstanceOf[CompressedWhitespace] &&
      ReceiverOption.contains(properties, ReceiverOption.WHOLE_TEXT_NODE)) {
      val tt: TinyTree = tree
      assert(tt != null)
      val lvalue: Long =
        chars.asInstanceOf[CompressedWhitespace].getCompressedValue
      nodeNr = tt.addNode(Type.WHITESPACE_TEXT,
        currentDepth,
        (lvalue >> 32).toInt,
        lvalue.toInt,
        -1)
      val prev: Int = prevAtDepth(currentDepth)
      if (prev > 0) {
        tt.next(prev) = nodeNr
      }
      // *O* owner pointer in last sibling
      tt.next(nodeNr) = prevAtDepth(currentDepth - 1)
      prevAtDepth(currentDepth) = nodeNr
      siblingsAtDepth(currentDepth) += 1

      if (lineNumbering) {
        tt.setLineNumber(nodeNr,
          locationId.getLineNumber,
          locationId.getColumnNumber)
      }
      return
    }
    val len: Int = chars.length
    if (len > 0) {
      nodeNr = makeTextNode(chars, len)
      if (lineNumbering) {
        tree.setLineNumber(nodeNr,
          locationId.getLineNumber,
          locationId.getColumnNumber)
      }
      textualElementEligibilityState =
        if (textualElementEligibilityState == Eligibility.PRIMED)
          Eligibility.ELIGIBLE
        else Eligibility.INELIGIBLE
    }
  }

  def makeTextNode(chars: CharSequence, len: Int): Int = {
    val tt: TinyTree = tree
    assert(tt != null)
    val bufferStart: Int = tt.getCharacterBuffer.length
    tt.appendChars(chars)
    val n: Int = tt.numberOfNodes - 1
    if (tt.nodeKind(n) == Type.TEXT && tt.depth(n) == currentDepth) {
      // merge this text node with the previous text node
      tt.beta(n) += len
    } else {
      nodeNr = tt.addNode(Type.TEXT, currentDepth, bufferStart, len, -1)
      val prev: Int = prevAtDepth(currentDepth)
      if (prev > 0) {
        tt.next(prev) = nodeNr
      }
      tt.next(nodeNr) = prevAtDepth(currentDepth - 1)
      prevAtDepth(currentDepth) = nodeNr
      siblingsAtDepth(currentDepth) += 1
    }
    nodeNr
  }

  def processingInstruction(piname: String,
                            remainder: CharSequence,
                            locationId: Location,
                            properties: Int): Unit = {
    val tt: TinyTree = tree
    assert(tt != null)
    textualElementEligibilityState = Eligibility.INELIGIBLE
    if (tt.commentBuffer == null) {
      tt.commentBuffer = new FastStringBuffer(FastStringBuffer.C256)
    }
    val s: Int = tt.commentBuffer.length
    tt.commentBuffer.append(remainder.toString)
    val nameCode: Int = namePool.allocateFingerprint("", piname)
    nodeNr = tt.addNode(Type.PROCESSING_INSTRUCTION,
      currentDepth,
      s,
      remainder.length,
      nameCode)
    val prev: Int = prevAtDepth(currentDepth)
    if (prev > 0) {
      tt.next(prev) = nodeNr
    }
    // *O* owner pointer in last sibling
    tt.next(nodeNr) = prevAtDepth(currentDepth - 1)
    prevAtDepth(currentDepth) = nodeNr
    siblingsAtDepth(currentDepth) += 1

    val localLocation: String = locationId.getSystemId
    tt.setSystemId(nodeNr, localLocation)
    if (localLocation != null && localLocation != baseURI) {
      uniformBaseURI = false
      tree.setUniformBaseUri(null)
    }
    if (lineNumbering) {
      tt.setLineNumber(nodeNr,
        locationId.getLineNumber,
        locationId.getColumnNumber)
    }
  }

  def comment(chars: CharSequence,
              locationId: Location,
              properties: Int): Unit = {
    val tt: TinyTree = tree
    assert(tt != null)
    textualElementEligibilityState = Eligibility.INELIGIBLE
    if (tt.commentBuffer == null) {
      tt.commentBuffer = new FastStringBuffer(FastStringBuffer.C256)
    }
    val s: Int = tt.commentBuffer.length
    tt.commentBuffer.append(chars.toString)
    nodeNr = tt.addNode(Type.COMMENT, currentDepth, s, chars.length, -1)
    val prev: Int = prevAtDepth(currentDepth)
    if (prev > 0) {
      tt.next(prev) = nodeNr
    }
    // *O* owner pointer in last sibling
    tt.next(nodeNr) = prevAtDepth(currentDepth - 1)
    prevAtDepth(currentDepth) = nodeNr
    siblingsAtDepth(currentDepth) += 1

    if (lineNumbering) {
      tt.setLineNumber(nodeNr,
        locationId.getLineNumber,
        locationId.getColumnNumber)
    }
  }

  def setUnparsedEntity(name: String, uri: String, publicId: String): Unit = {
    if (tree.getUnparsedEntity(name) == null) {
      // bug 2187
      tree.setUnparsedEntity(name, uri, publicId)
    }
  }

  /*@NotNull*/

  override def getBuilderMonitor(): BuilderMonitor = new TinyBuilderMonitor(this)

}

// Copyright (c) 2018-2020 Saxonica Limited
// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is "Incompatible With Secondary Licenses", as defined by the Mozilla Public License, v. 2.0.
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
/**
 * The TinyBuilder class is responsible for taking a stream of SAX events and constructing
 * a Document tree, using the "TinyTree" implementation.
 *
 * @author Michael H. Kay
 */
