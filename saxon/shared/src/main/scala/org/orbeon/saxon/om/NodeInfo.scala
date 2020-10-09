////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// Copyright (c) 2018-2020 Saxonica Limited
// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is "Incompatible With Secondary Licenses", as defined by the Mozilla Public License, v. 2.0.
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
package org.orbeon.saxon.om

import java.util.Collections
import java.util.function.Predicate

import javax.xml.transform.Source
import org.orbeon.saxon.event.{Receiver, ReceiverOption}
import org.orbeon.saxon.expr.parser.Loc
import org.orbeon.saxon.model._
import org.orbeon.saxon.om.Genre.Genre
import org.orbeon.saxon.pattern.AnyNodeTest
import org.orbeon.saxon.s9api.Location
import org.orbeon.saxon.trans.Err
import org.orbeon.saxon.tree.iter.AxisIterator
import org.orbeon.saxon.tree.util.{FastStringBuffer, Navigator}
import org.orbeon.saxon.utils.Configuration

//import scala.collection.compat._
import scala.jdk.CollectionConverters._


/**
  * The NodeInfo interface represents a node in Saxon's implementation of the XPath 2.0 data model.
  * <p>Note that several NodeInfo objects may represent the same node. To test node identity, the
  * method {@link #equals(Object)} should be used. An exception to this rule applies for
  * document nodes, where the correspondence between document nodes and DocumentInfo objects is one to
  * one. NodeInfo objects are never reused: a given NodeInfo object represents the same node for its entire
  * lifetime.</p>
  * <p>This is the primary interface for accessing trees in Saxon, and it forms part of the public
  * Saxon API. Methods that form part of the public API are (since Saxon 8.4)
  * labelled with a JavaDoc "since" tag: classes and methods that have no such label should not be
  * regarded as stable interfaces.</p>
  * <p>The interface represented by this class is at a slightly higher level than the abstraction described
  * in the W3C data model specification, in that it includes support for the XPath axes, rather than exposing
  * the lower-level properties (such as "parent" and "children") directly. All navigation within trees,
  * except for a few convenience methods, is done by following the axes using the {@link #iterateAxis} method.
  * This allows different implementations of the XPath tree model to implement axis navigation in different ways.
  * Some implementations may choose to use the helper methods provided in class {@link org.orbeon.saxon.tree.util.Navigator}.</p>
  * <p>Note that the stability of this interface applies to classes that use the interface,
  * not to classes that implement it. The interface may be extended in future to add new methods.</p>
  * <p>New implementations of NodeInfo are advised also to implement the methods in interface
  * ExtendedNodeInfo, which will be moved into this interface at some time in the future.</p>
  *
  * @since 8.4. Extended with three extra methods, previously in ExtendedNodeInfo, in 9.1.
  * In 9.7, DocumentInfo is no longer defined as a sub-interface; it is replaced with TreeInfo,
  * which contains information about any XML node tree, whether or not it is rooted at a document node.
  * In 9.8, default implementations are provided for some of the methods, making it easier to create
  * a new implementation of this interface.
  */
trait NodeInfo extends Source with Item with Location {
  def getTreeInfo: TreeInfo
  def getConfiguration: Configuration = getTreeInfo.getConfiguration
  def getNodeKind: Int
  def isSameNodeInfo(other: NodeInfo): Boolean = equals(other)
  def equals(other: Any): Boolean
  def hashCode: Int
  /*@Nullable*/
  def getSystemId: String
  def getPublicId: String = null
  def getBaseURI: String
  def getLineNumber: Int = -1
  def getColumnNumber: Int = -1
  def compareOrder(other: NodeInfo): Int
  def getStringValue: String
  def hasFingerprint: Boolean
  def getFingerprint: Int
  def getLocalPart: String
  def getURI: String
  def getDisplayName: String
  def getPrefix: String
  def getSchemaType: SchemaType = getNodeKind match {
    case Type.ATTRIBUTE => BuiltInAtomicType.UNTYPED_ATOMIC
    case Type.DOCUMENT | Type.ELEMENT => Untyped.getInstance
    case _ => null
  }
  var IS_DTD_TYPE: Int = 1 << 30
  var IS_NILLED: Int = 1 << 29
  def atomize(): AtomicSequence

  /*@Nullable*/
  def getParent: NodeInfo

  def iterateAxis(axisNumber: Int): AxisIterator =
    iterateAxis(axisNumber, AnyNodeTest.getInstance)

  def iterateAxis(axisNumber: Int,
                  nodeTest: Predicate[_ >: NodeInfo]): AxisIterator

  /*@Nullable*/

  def getAttributeValue(uri: String, local: String): String
  def getRoot: NodeInfo
  def hasChildNodes: Boolean
  def children: Iterator[NodeInfo] = {
    if (hasChildNodes) {
      val parent: NodeInfo = this
      parent.iterateAxis(AxisInfo.CHILD).asiterator
    } else {
      Collections.emptyList().asInstanceOf[Iterator[NodeInfo]]
    }
  }

  def children(filter: Predicate[_ >: NodeInfo]): Iterable[_ <: NodeInfo] =
    if (hasChildNodes) {
      val parent: NodeInfo = this
      parent.iterateAxis(AxisInfo.CHILD, nodeTest = filter).asiterator.iterator.to(Iterable)
    } else {
      Collections.emptyList().asScala
    }

  def attributes: AttributeMap = {
    var atts: AttributeMap = EmptyAttributeMap.getInstance
    if (getNodeKind == Type.ELEMENT) {
      val iter: AxisIterator = iterateAxis(AxisInfo.ATTRIBUTE)
      var attr: NodeInfo = null
      while ({
        attr = iter.next()
        attr
      } != null) atts = atts.put(
        new AttributeInfo(NameOfNode.makeName(attr),
                          attr.getSchemaType.asInstanceOf[SimpleType],
                          attr.getStringValue,
                          Loc.NONE,
                          ReceiverOption.NONE))
    }
    atts
  }

  def generateId(buffer: FastStringBuffer): Unit

  def copy(out: Receiver, copyOptions: Int, locationId: Location): Unit =
    Navigator.copy(this, out, copyOptions, locationId)

  def getDeclaredNamespaces(buffer: Array[NamespaceBinding]): Array[NamespaceBinding]
  def getAllNamespaces: NamespaceMap
  def isId: Boolean = false
  def isIdref: Boolean = false
  def isNilled: Boolean = false

  override def isStreamed: Boolean = false

  override def toShortString: String = getNodeKind match {
    case Type.DOCUMENT => "document-node()"
    case Type.ELEMENT => "<" + getDisplayName + "/>"
    case Type.ATTRIBUTE => "@" + getDisplayName
    case Type.TEXT => "text(\"" + Err.truncate30(getStringValue) + "\")"
    case Type.COMMENT => "<!--" + Err.truncate30(getStringValue) + "-->"
    case Type.PROCESSING_INSTRUCTION => "<?" + getDisplayName + "?>"
    case Type.NAMESPACE =>
      val prefix: String = getLocalPart
      "xmlns" + (if (prefix.==("")) "" else ":" + prefix) +
        "=\"" +
        getStringValue +
        '"'
    case _ => ""
  }

  /**
    * Get the genre of this item
    *
    * @return the genre: specifically, {@link Genre#NODE}. The default implementation (which should not
    * be overridden) returns {@link Genre#NODE}.
    */
  override def getGenre: Genre = Genre.NODE
}
