////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
package net.sf.saxon.tree.linked

import net.sf.saxon.event.Builder

import net.sf.saxon.expr.parser.Loc

import net.sf.saxon.model.Type

import net.sf.saxon.om.CopyOptions

import net.sf.saxon.om.NodeInfo

import net.sf.saxon.pattern.AnyNodeTest

import net.sf.saxon.trans.XPathException

import net.sf.saxon.tree.iter.ArrayIterator

import net.sf.saxon.tree.iter.AxisIterator

import net.sf.saxon.tree.iter.EmptyIterator

import net.sf.saxon.tree.iter.SingleNodeIterator

import net.sf.saxon.tree.jiter.MonoIterator

import net.sf.saxon.tree.util.FastStringBuffer

import net.sf.saxon.tree.util.Navigator

import java.util.Arrays

import java.util.Collections

import java.util.function.Predicate

import scala.jdk.CollectionConverters._
import scala.util.control.Breaks._


abstract class ParentNodeImpl extends NodeImpl {

  // null for no children
  private var childrenImpl: Any = null

  // sequence number allocated during original tree creation.
  private var sequence: Int = _

  override def getSequenceNumber(): Long =
    if (getRawSequenceNumber == -1) -1L else getRawSequenceNumber.toLong << 32

  def getRawSequenceNumber: Int = sequence

  def setRawSequenceNumber(seq: Int): Unit = {
    sequence = seq
  }

  def setChildren(children: AnyRef): Unit = {
    this.childrenImpl = children
  }

  override def hasChildNodes: Boolean = childrenImpl != null

  override def children(): Iterator[NodeImpl] =
    if (childrenImpl == null) {
      Collections.emptyList().iterator().asScala
    } else if (childrenImpl.isInstanceOf[NodeImpl]) {
      new MonoIterator(childrenImpl.asInstanceOf[NodeImpl]).asInstanceOf[Iterator[NodeImpl]]
    } else {
      Arrays.asList(childrenImpl.asInstanceOf[Array[NodeImpl]]: _*).iterator().asScala
    }

  def getNumberOfChildren: Int =
    if (childrenImpl == null) {
      0
    } else if (childrenImpl.isInstanceOf[NodeImpl]) {
      1
    } else {
      childrenImpl.asInstanceOf[Array[NodeInfo]].length
    }

  def iterateChildren(test: Predicate[_ >: NodeInfo]): AxisIterator =
    if (childrenImpl == null) {
      EmptyIterator.ofNodes()
    } else if (childrenImpl.isInstanceOf[NodeImpl]) {
      val child: NodeImpl = childrenImpl.asInstanceOf[NodeImpl]
      if (test == null || test == AnyNodeTest.getInstance) {
        SingleNodeIterator.makeIterator(child)
      } else {
        Navigator.filteredSingleton(child, test)
      }
    } else {
      if (test == null || test == AnyNodeTest.getInstance) {
        new ArrayIterator.OfNodes(childrenImpl.asInstanceOf[Array[NodeInfo]])
      } else {
        new ChildEnumeration(this, test)
      }
    }

  /*@Nullable*/

  override def getFirstChild(): NodeImpl =
    if (childrenImpl == null) {
      null
    } else if (childrenImpl.isInstanceOf[NodeImpl]) {
      childrenImpl.asInstanceOf[NodeImpl]
    } else {
      childrenImpl.asInstanceOf[Array[NodeImpl]](0)
    }

  /*@Nullable*/

  override def getLastChild(): NodeImpl = {
    if (childrenImpl == null) {
      return null
    }
    if (childrenImpl.isInstanceOf[NodeImpl]) {
      childrenImpl.asInstanceOf[NodeImpl]
    }
    val n: Array[NodeImpl] = childrenImpl.asInstanceOf[Array[NodeImpl]]
    n(n.length - 1)
  }

  /*@Nullable*/

  def getNthChild(n: Int): NodeImpl = {
    if (childrenImpl == null) {
      return null
    }
    if (childrenImpl.isInstanceOf[NodeImpl]) {
      if (n == 0) childrenImpl.asInstanceOf[NodeImpl] else return null
    }
    val nodes: Array[NodeImpl] = childrenImpl.asInstanceOf[Array[NodeImpl]]
    if (n < 0 || n >= nodes.length) {
      return null
    }
    nodes(n)
  }

  def removeChild(child: NodeImpl): Unit = {
    if (childrenImpl == null) {
      return
    }
    if (childrenImpl == child) {
      childrenImpl = null
      return
    }
    val nodes: Array[NodeImpl] = childrenImpl.asInstanceOf[Array[NodeImpl]]
    breakable {
      for (i <- 0 until nodes.length if nodes(i) == child) {
        if (nodes.length == 2) {
          childrenImpl = nodes(1 - i)
        } else {
          val n2: Array[NodeImpl] = Array.ofDim[NodeImpl](nodes.length - 1)
          if (i > 0) {
            System.arraycopy(nodes, 0, n2, 0, i)
          }
          if (i < nodes.length - 1) {
            System.arraycopy(nodes, i + 1, n2, i, nodes.length - i - 1)
          }
          childrenImpl = cleanUpChildren(n2)
        }
        break()
      }
    }
  }

  /*@NotNull*/

  private def cleanUpChildren(children: Array[NodeImpl]): Array[NodeImpl] = {
    var prevText: Boolean = false
    var j: Int = 0
    val c2: Array[NodeImpl] = Array.ofDim[NodeImpl](children.length)
    for (node <- children) {
      if (node.isInstanceOf[TextImpl]) {
        if (prevText) {
          val prev: TextImpl = c2(j - 1).asInstanceOf[TextImpl]
          prev.replaceStringValue(prev.getStringValue + node.getStringValue)
        } else if (!node.getStringValue.isEmpty) {
          prevText = true
          node.setSiblingPosition(j)
          j = j + 1
          c2(j) = node
        }
      } else {
        node.setSiblingPosition(j)
        j = j + 1
        c2(j) = node
        prevText = false
      }
    }
    if (j == c2.length) {
      c2
    } else {
      Arrays.copyOf(c2, j)
    }
  }

  def getStringValue: String = getStringValueCS.toString

  override def getStringValueCS: CharSequence = {
    var sb: FastStringBuffer = null
    var next: NodeImpl = getFirstChild
    while (next != null) {
      if (next.isInstanceOf[TextImpl]) {
        if (sb == null) {
          sb = new FastStringBuffer(FastStringBuffer.C64)
        }
        sb.cat(next.getStringValueCS)
      }
      next = next.getNextInDocument(this)
    }
    if (sb == null) return ""
    sb.condense()
  }

  def addChild(node: NodeImpl, index: Int): Unit = {
    synchronized {
      var c: Array[NodeImpl] = null
      if (childrenImpl == null) {
        c = Array.ofDim[NodeImpl](10)
      } else if (childrenImpl.isInstanceOf[NodeImpl]) {
        c = Array.ofDim[NodeImpl](10)
        c(0) = childrenImpl.asInstanceOf[NodeImpl]
      } else {
        c = childrenImpl.asInstanceOf[Array[NodeImpl]]
      }
      if (index >= c.length) {
        c = Arrays.copyOf(c, c.length * 2)
      }
      c(index) = node
      node.setRawParent(this)
      node.setSiblingPosition(index)
      childrenImpl = c
    }
  }

  override def insertChildren(source: Array[NodeInfo],
                              atStart: Boolean,
                              inherit: Boolean): Unit = {
    if (atStart) {
      insertChildrenAt(source, 0, inherit)
    } else {
      insertChildrenAt(source, getNumberOfChildren, inherit)
    }
  }

  def insertChildrenAt(source: Array[NodeInfo],
                       index: Int,
                       inherit: Boolean): Unit = {
    synchronized {
      if (source.length == 0) {
        return
      }
      val source2: Array[NodeImpl] = adjustSuppliedNodeArray(source, inherit)
      if (childrenImpl == null) {
        if (source2.length == 1) {
          childrenImpl = source2(0)
          childrenImpl.asInstanceOf[NodeImpl].setSiblingPosition(0)
        } else {
          childrenImpl = cleanUpChildren(source2)
        }
      } else if (childrenImpl.isInstanceOf[NodeImpl]) {
        val adjacent: Int = if (index == 0) source2.length - 1 else 0
        if (childrenImpl.isInstanceOf[TextImpl] && source2(adjacent)
          .isInstanceOf[TextImpl]) {
          if (index == 0) {
            source2(adjacent).replaceStringValue(
              source2(adjacent).getStringValue + childrenImpl
                .asInstanceOf[TextImpl]
                .getStringValue)
          } else {
            source2(adjacent).replaceStringValue(
              childrenImpl.asInstanceOf[TextImpl].getStringValue + source2(
                adjacent).getStringValue)
          }
          childrenImpl = cleanUpChildren(source2)
        } else {
          val n2: Array[NodeImpl] = Array.ofDim[NodeImpl](source2.length + 1)
          if (index == 0) {
            System.arraycopy(source2, 0, n2, 0, source2.length)
            n2(source2.length) = childrenImpl.asInstanceOf[NodeImpl]
          } else {
            n2(0) = childrenImpl.asInstanceOf[NodeImpl]
            System.arraycopy(source2, 0, n2, 1, source2.length)
          }
          childrenImpl = cleanUpChildren(n2)
        }
      } else {
        val n0: Array[NodeImpl] = childrenImpl.asInstanceOf[Array[NodeImpl]]
        val n2: Array[NodeImpl] =
          Array.ofDim[NodeImpl](n0.length + source2.length)
        System.arraycopy(n0, 0, n2, 0, index)
        System.arraycopy(source2, 0, n2, index, source2.length)
        System.arraycopy(n0,
          index,
          n2,
          index + source2.length,
          n0.length - index)
        childrenImpl = cleanUpChildren(n2)
      }
    }
  }

  /*@NotNull*/

  private def convertForeignNode(source: NodeInfo): NodeImpl = {
    if (!(source.isInstanceOf[NodeImpl])) {
      val kind: Int = source.getNodeKind
      kind match {
        case Type.TEXT =>
          return new TextImpl(source.getStringValue)
        case Type.COMMENT =>
          return new CommentImpl(source.getStringValue)
        case Type.PROCESSING_INSTRUCTION =>
          return new ProcInstImpl(source.getLocalPart, source.getStringValue)
        case Type.ELEMENT =>
          var builder: Builder = null
          try {
            builder = new LinkedTreeBuilder(
              getConfiguration.makePipelineConfiguration)
            builder.open()
            source.copy(builder, CopyOptions.ALL_NAMESPACES, Loc.NONE)
            builder.close()
          } catch {
            case e: XPathException => throw new IllegalArgumentException(
              "Failed to convert inserted element node to an instance of net.sf.saxon.om.tree.ElementImpl");
          }
          return builder.getCurrentRoot.asInstanceOf[NodeImpl]
        case _ =>
          throw new IllegalArgumentException(
            "Cannot insert a node unless it is an element, comment, text node, or processing instruction")

      }
    }
    source.asInstanceOf[NodeImpl]
  }

  def replaceChildrenAt(source: Array[NodeInfo],
                        index: Int,
                        inherit: Boolean): Unit = {
    synchronized {
      if (childrenImpl == null) {
        return
      }
      val source2: Array[NodeImpl] = adjustSuppliedNodeArray(source, inherit)
      if (childrenImpl.isInstanceOf[NodeImpl]) {
        if (source2.length == 0) {
          childrenImpl = null
        } else if (source2.length == 1) {
          childrenImpl = source2(0)
        } else {
          val n2: Array[NodeImpl] = Array.ofDim[NodeImpl](source2.length)
          System.arraycopy(source2, 0, n2, 0, source.length)
          childrenImpl = cleanUpChildren(n2)
        }
      } else {
        val n0: Array[NodeImpl] = childrenImpl.asInstanceOf[Array[NodeImpl]]
        val n2: Array[NodeImpl] =
          Array.ofDim[NodeImpl](n0.length + source2.length - 1)
        System.arraycopy(n0, 0, n2, 0, index)
        System.arraycopy(source2, 0, n2, index, source2.length)
        System.arraycopy(n0,
          index + 1,
          n2,
          index + source2.length,
          n0.length - index - 1)
        childrenImpl = cleanUpChildren(n2)
      }
    }
  }

  private def adjustSuppliedNodeArray(source: Array[NodeInfo],
                                      inherit: Boolean): Array[NodeImpl] = {
    val source2: Array[NodeImpl] = Array.ofDim[NodeImpl](source.length)
    for (i <- 0 until source.length) {
      source2(i) = convertForeignNode(source(i))
      val child: NodeImpl = source2(i)
      child.setRawParent(this)
      if (child.isInstanceOf[ElementImpl]) {
        // from the new parent
        child.asInstanceOf[ElementImpl].fixupInsertedNamespaces(inherit)
      }
      // If the child has no xmlns="xxx" declaration, then add an xmlns="" to prevent false inheritance
      // If the child has no xmlns="xxx" declaration, then add an xmlns="" to prevent false inheritance
    }
    source2
  }

  def compact(size: Int): Unit = {
    synchronized {
      if (size == 0) {
        childrenImpl = null
      } else if (size == 1) {
        if (childrenImpl.isInstanceOf[Array[NodeImpl]]) {
          childrenImpl = childrenImpl.asInstanceOf[Array[NodeImpl]](0)
        }
      } else {
        childrenImpl = Arrays.copyOf(childrenImpl.asInstanceOf[Array[NodeImpl]], size)
      }
    }
  }

}

// Copyright (c) 2018-2020 Saxonica Limited
// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is "Incompatible With Secondary Licenses", as defined by the Mozilla Public License, v. 2.0.
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
//import com.sun.tools.javac.util.List;
/**
 * ParentNodeImpl is an implementation of a non-leaf node (specifically, an Element node
 * or a Document node)
 *
 * @author Michael H. Kay
 */
