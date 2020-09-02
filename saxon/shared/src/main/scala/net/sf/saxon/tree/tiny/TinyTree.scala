////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// Copyright (c) 2018-2020 Saxonica Limited
// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is "Incompatible With Secondary Licenses", as defined by the Mozilla Public License, v. 2.0.
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
/**
 * A data structure to hold the contents of a tree. As the name implies, this implementation
 * of the data model is optimized for size, and for speed of creation: it minimizes the number
 * of Java objects used.
 * <p>
 * <p>It can be used to represent a tree that is rooted at a document node, or one that is rooted
 * at an element node.</p>
 * <p>
 * <p>From Saxon 9.7, as a consequence of bug 2220, it is used only to hold a single tree, whose
 * root is always node number zero.</p>
 */

package net.sf.saxon.tree.tiny


import net.sf.saxon.utils.Configuration
import net.sf.saxon.event.ReceiverOption
import net.sf.saxon.lib.Feature
import net.sf.saxon.lib.FeatureKeys
import net.sf.saxon.model._
import net.sf.saxon.om._
import net.sf.saxon.trans.XPathException
import net.sf.saxon.tree.linked.SystemIdMap
import net.sf.saxon.tree.util.FastStringBuffer
import net.sf.saxon.value._
import net.sf.saxon.z._
import java.util._

import TinyTree._

import scala.beans.{BeanProperty, BooleanBeanProperty}
import scala.jdk.CollectionConverters._

//remove if not needed
import scala.jdk.CollectionConverters._

import scala.util.control.Breaks._


object TinyTree {

  /*@NotNull*/

  private val EMPTY_STRING_ARRAY: Array[String] = new Array[String](0)

  val TYPECODE_IDREF: Int = 1 << 29

  def diagnosticDump(node: NodeInfo): Unit = {
    synchronized {
      if (node.isInstanceOf[TinyNodeImpl]) {
        val tree: TinyTree = node.asInstanceOf[TinyNodeImpl].tree
        System.err.println(
          "Tree containing node " + node.asInstanceOf[TinyNodeImpl].nodeNr)
        tree.diagnosticDump()
      } else {
        System.err.println("Node is not in a TinyTree")
      }
    }
  }

  private def n8(`val`: Int): String = {
    val s: String = "        " + `val`
    s.substring(s.length - 8)
  }

}

class TinyTree(config: Configuration, statistics: Statistics)
  extends GenericTreeInfo(config)
    with NodeVectorTree {

  /*@Nullable*/

  val nodes: Int = statistics.getAverageNodes.toInt + 1

  val attributes: Int = statistics.getAverageAttributes.toInt + 1

  val namespaces: Int = statistics.getAverageNamespaces.toInt + 1

  val characters: Int = statistics.getAverageCharacters.toInt + 1

  var commentBuffer: FastStringBuffer = null

  var numberOfNodes: Int = 0

  var nodeKind: Array[Byte] = new Array[Byte](nodes)

  var depth: Array[Short] = new Array[Short](nodes)

  var next: Array[Int] = new Array[Int](nodes)

  var alpha: Array[Int] = new Array[Int](nodes)

  var beta: Array[Int] = new Array[Int](nodes)

  var nameCode: Array[Int] = new Array[Int](nodes)

  /*@Nullable*/

  var prior: Array[Int] = null

  /*@Nullable*/

  var typeArray: Array[SchemaType] = null

  /*@Nullable*/

  var typedValueArray: Array[AtomicSequence] = null

  var idRefElements: IntSet = null

  var idRefAttributes: IntSet = null

  var nilledElements: IntSet = null

  var defaultedAttributes: IntSet = null

  var topWithinEntity: IntSet = null

  private var allowTypedValueCache: Boolean = true

  private var localNameIndex: Map[String, IntSet] = null

  var numberOfAttributes: Int = 0

  var attParent: Array[Int] = new Array[Int](attributes)

  var attCode: Array[Int] = new Array[Int](attributes)

  var attValue: Array[CharSequence] = new Array[CharSequence](attributes)

  var attTypedValue: Array[AtomicSequence] = _

  /*@Nullable*/

  var attType: Array[SimpleType] = _

  var numberOfNamespaces: Int = 0

  var namespaceMaps: Array[NamespaceMap] =
    new Array[NamespaceMap](namespaces)

  /*@Nullable*/

  private var lineNumbers: Array[Int] = null

  /*@Nullable*/

  private var columnNumbers: Array[Int] = null

  /*@Nullable*/

  private var systemIdMap: SystemIdMap = null

  var usesNamespaces: Boolean = false

  var prefixPool: PrefixPool = new PrefixPool()

  private var root: TinyDocumentImpl = _

  private var idTable: HashMap[String, NodeInfo] = _

  var entityTable: HashMap[String, Array[String]] = _

  @BeanProperty
  var copiedFrom: NodeInfo = _

  var knownBaseUris: IntHashMap[String] = _

  @BeanProperty
  var uniformBaseUri: String = null

  this.setConfiguration(config)

  var charBuffer: AppendableCharSequence =
    if (characters > 65000) new LargeStringBuffer()
    else new FastStringBuffer(characters)

  override def setConfiguration(config: Configuration): Unit = {
    super.setConfiguration(config)
    allowTypedValueCache = config.isLicensedFeature(
      Configuration.LicenseFeature.SCHEMA_VALIDATION) &&
      config.getBooleanProperty(Feature.USE_TYPED_VALUE_CACHE)
  }

  private def ensureNodeCapacity(kind: Short, needed: Int): Unit = {
    if (nodeKind.length < numberOfNodes + needed) {
      val k: Int =
        if (kind == Type.STOPPER) numberOfNodes + 1
        else Math.max(numberOfNodes * 2, numberOfNodes + needed)
      nodeKind = Arrays.copyOf(nodeKind, k)
      next = Arrays.copyOf(next, k)
      depth = Arrays.copyOf(depth, k)
      alpha = Arrays.copyOf(alpha, k)
      beta = Arrays.copyOf(beta, k)
      nameCode = Arrays.copyOf(nameCode, k)
      if (typeArray != null) {
        typeArray = Arrays.copyOf(typeArray, k)
      }
      if (typedValueArray != null) {
        typedValueArray = Arrays.copyOf(typedValueArray, k)
      }
      if (lineNumbers != null) {
        lineNumbers = Arrays.copyOf(lineNumbers, k)
        columnNumbers = Arrays.copyOf(columnNumbers, k)
      }
    }
  }

  private def ensureAttributeCapacity(needed: Int): Unit = {
    if (attParent.length < numberOfAttributes + needed) {
      var k: Int =
        Math.max(numberOfAttributes + needed, numberOfAttributes * 2)
      if (k == 0) {
        k = 10 + needed
      }
      attParent = Arrays.copyOf(attParent, k)
      attCode = Arrays.copyOf(attCode, k)
      attValue = Arrays.copyOf(attValue, k)
      if (attType != null) {
        attType = Arrays.copyOf(attType, k)
      }
      if (attTypedValue != null) {
        attTypedValue = Arrays.copyOf(attTypedValue, k)
      }
    }
  }

  private def ensureNamespaceCapacity(needed: Int): Unit = {
    if (namespaceMaps.length < numberOfNamespaces + needed) {
      var k: Int =
        Math.max(numberOfNamespaces * 2, numberOfNamespaces + needed)
      if (k == 0) {
        k = 10
      }
      namespaceMaps = Arrays.copyOf(namespaceMaps, k)
    }
  }

  def getPrefixPool: PrefixPool = prefixPool

  def addDocumentNode(doc: TinyDocumentImpl): Int = {
    this.setRootNode(doc)
    addNode(Type.DOCUMENT, 0, 0, 0, -1)
  }

  /**
   * Add a node to the tree
   *
   * @param kind     The kind of the node. This must be a document, element, text, comment,
   *                 or processing-instruction node (not an attribute or namespace)
   * @param depth    The depth in the tree
   * @param alpha    Pointer to attributes or text
   * @param beta     Pointer to namespaces or text
   * @param nameCode The name of the node
   * @return the node number of the node that was added
   */
  def addNode(kind: Short,
              depth: Int,
              alpha: Int,
              beta: Int,
              nameCode: Int): Int = {
    ensureNodeCapacity(kind, 1)
    nodeKind(numberOfNodes) = kind.toByte
    this.depth(numberOfNodes) = depth.toShort
    this.alpha(numberOfNodes) = alpha
    this.beta(numberOfNodes) = beta
    this.nameCode(numberOfNodes) = nameCode
    next(numberOfNodes) = -1
    if (typeArray != null) {
      typeArray(numberOfNodes) = Untyped.getInstance
    }
    if (numberOfNodes == 0) {
      this.setDocumentNumber(getConfiguration.getDocumentNumberAllocator.allocateDocumentNumber())

    }
    {
      numberOfNodes += 1; numberOfNodes - 1
    }
  }

  def appendChars(chars: CharSequence): Unit = {
    if (charBuffer
      .isInstanceOf[FastStringBuffer] && charBuffer.length > 65000) {
      val lsb: LargeStringBuffer = new LargeStringBuffer()
      charBuffer = lsb.cat(charBuffer)
    }
    charBuffer.cat(chars)
  }

  def addTextNodeCopy(depth: Int, existingNodeNr: Int): Int =
    addNode(Type.TEXT, depth, alpha(existingNodeNr), beta(existingNodeNr), -1)

  def condense(statistics: Statistics): Unit = {
    if (numberOfNodes * 3 < nodeKind.length || (nodeKind.length - numberOfNodes > 20000)) {
      nodeKind = Arrays.copyOf(nodeKind, numberOfNodes)
      next = Arrays.copyOf(next, numberOfNodes)
      depth = Arrays.copyOf(depth, numberOfNodes)
      alpha = Arrays.copyOf(alpha, numberOfNodes)
      beta = Arrays.copyOf(beta, numberOfNodes)
      nameCode = Arrays.copyOf(nameCode, numberOfNodes)
      if (typeArray != null) {
        typeArray = Arrays.copyOf(typeArray, numberOfNodes)
      }
      if (lineNumbers != null) {
        lineNumbers = Arrays.copyOf(lineNumbers, numberOfNodes)
        columnNumbers = Arrays.copyOf(columnNumbers, numberOfNodes)
      }
    }
    if ((numberOfAttributes * 3 < attParent.length) || (attParent.length - numberOfAttributes > 1000)) {
      val k: Int = numberOfAttributes
      if (k == 0) {
        attParent = IntArraySet.EMPTY_INT_ARRAY
        attCode = IntArraySet.EMPTY_INT_ARRAY
        attValue = Array()
        attType = null
      } else {
        attParent = Arrays.copyOf(attParent, numberOfAttributes)
        attCode = Arrays.copyOf(attCode, numberOfAttributes)
        attValue = Arrays.copyOf(attValue, numberOfAttributes)
      }
      if (attType != null) {
        attType = Arrays.copyOf(attType, numberOfAttributes)
      }
    }
    if (numberOfNamespaces * 3 < namespaceMaps.length) {
      namespaceMaps = Arrays.copyOf(namespaceMaps, numberOfNamespaces)
    }
    prefixPool.condense()
    statistics.updateStatistics(numberOfNodes,
      numberOfAttributes,
      numberOfNamespaces,
      charBuffer.length)
  }

  def setElementAnnotation(nodeNr: Int, `type`: SchemaType): Unit = {
    if (`type` != Untyped.getInstance) {
      if (typeArray == null) {
        typeArray = Array.ofDim[SchemaType](nodeKind.length)
        Arrays.fill(typeArray.asInstanceOf[Array[AnyRef]], 0, nodeKind.length, Untyped.getInstance)
      }
      assert(typeArray != null)
      typeArray(nodeNr) = `type`
    }
  }

  def getTypeAnnotation(nodeNr: Int): Int = {
    if (typeArray == null) {
      StandardNames.XS_UNTYPED
    }
    typeArray(nodeNr).getFingerprint
  }

  def getSchemaType(nodeNr: Int): SchemaType = {
    if (typeArray == null) {
      Untyped.getInstance
    }
    typeArray(nodeNr)
  }

  /*@Nullable*/

  def getTypedValueOfElement(element: TinyElementImpl): AtomicSequence = {
    val nodeNr: Int = element.nodeNr
    if (typedValueArray == null || typedValueArray(nodeNr) == null) {
      val stype: SchemaType = getSchemaType(nodeNr)
      val annotation: Int = stype.getFingerprint
      if (annotation == StandardNames.XS_UNTYPED || annotation == StandardNames.XS_UNTYPED_ATOMIC ||
        annotation == StandardNames.XS_ANY_TYPE) {
        val stringValue: CharSequence =
          TinyParentNodeImpl.getStringValueCS(this, nodeNr)
        new UntypedAtomicValue(stringValue)
      } else if (annotation == StandardNames.XS_STRING) {
        val stringValue: CharSequence =
          TinyParentNodeImpl.getStringValueCS(this, nodeNr)
        new StringValue(stringValue)
      } else if (annotation == StandardNames.XS_ANY_URI) {
        val stringValue: CharSequence =
          TinyParentNodeImpl.getStringValueCS(this, nodeNr)
        new AnyURIValue(stringValue)
      } else {
        val value: AtomicSequence = stype.atomize(element)
        if (allowTypedValueCache) {
          if (typedValueArray == null) {
            typedValueArray = Array.ofDim[AtomicSequence](nodeKind.length)
          }
          typedValueArray(nodeNr) = value
        }
        value
      }
    } else {
      typedValueArray(nodeNr)
    }
  }

  /*@Nullable*/

  def getTypedValueOfElement(nodeNr: Int): AtomicSequence =
    if (typedValueArray == null || typedValueArray(nodeNr) == null) {
      val stype: SchemaType = getSchemaType(nodeNr)
      val annotation: Int = stype.getFingerprint
      if (annotation == StandardNames.XS_UNTYPED_ATOMIC || annotation == StandardNames.XS_UNTYPED) {
        val stringValue: CharSequence =
          TinyParentNodeImpl.getStringValueCS(this, nodeNr)
        new UntypedAtomicValue(stringValue)
      } else if (annotation == StandardNames.XS_STRING) {
        val stringValue: CharSequence =
          TinyParentNodeImpl.getStringValueCS(this, nodeNr)
        new StringValue(stringValue)
      } else if (annotation == StandardNames.XS_ANY_URI) {
        val stringValue: CharSequence =
          TinyParentNodeImpl.getStringValueCS(this, nodeNr)
        new AnyURIValue(stringValue)
      } else if (annotation == StandardNames.XS_ID) {
        val stringValue: CharSequence =
          TinyParentNodeImpl.getStringValueCS(this, nodeNr)
        new StringValue(stringValue, BuiltInAtomicType.ID)
      } else {
        val element: TinyNodeImpl = getNode(nodeNr)
        val value: AtomicSequence = stype.atomize(element)
        if (allowTypedValueCache) {
          if (typedValueArray == null) {
            typedValueArray = Array.ofDim[AtomicSequence](nodeKind.length)
          }
          typedValueArray(nodeNr) = value
        }
        value
      }
    } else {
      typedValueArray(nodeNr)
    }

  def getTypedValueOfAttribute(att: TinyAttributeImpl,
                               nodeNr: Int): AtomicSequence = {
    var tinyAtt: TinyAttributeImpl = att
    if (attType == null) {
      new UntypedAtomicValue(attValue(nodeNr))
    }
    if (attTypedValue == null || attTypedValue(nodeNr) == null) {
      val `type`: SimpleType = getAttributeType(nodeNr)
      if (`type` == BuiltInAtomicType.UNTYPED_ATOMIC) {
        new UntypedAtomicValue(attValue(nodeNr))
      } else if (`type` == BuiltInAtomicType.STRING) {
        new StringValue(attValue(nodeNr))
      } else if (`type` == BuiltInAtomicType.ANY_URI) {
        new AnyURIValue(attValue(nodeNr))
      } else {
        if (tinyAtt == null) {
          tinyAtt = new TinyAttributeImpl(this, nodeNr)
        }
        val value: AtomicSequence = `type`.atomize(tinyAtt)
        if (allowTypedValueCache) {
          if (attTypedValue == null) {
            attTypedValue = Array.ofDim[AtomicSequence](attParent.length)
          }
          attTypedValue(nodeNr) = value
        }
        value
      }
    } else {
      attTypedValue(nodeNr)
    }
  }

  def getNodeKind(nodeNr: Int): Int = {
    val kind: Int = nodeKind(nodeNr)
    if (kind == Type.WHITESPACE_TEXT) Type.TEXT else kind
  }

  def getNameCode(nodeNr: Int): Int = nameCode(nodeNr)

  def getFingerprint(nodeNr: Int): Int = {
    val nc: Int = nameCode(nodeNr)
    if (nc == -1) -1 else nc & NamePool.FP_MASK
  }

  def getPrefix(nodeNr: Int): String = {
    val code: Int = nameCode(nodeNr) >> 20
    if (code <= 0) {
      if (code == 0) return "" else return null
    }
    prefixPool.getPrefix(code)
  }

  def ensurePriorIndex(): Unit = {
    if (prior == null || prior.length < numberOfNodes) {
      makePriorIndex()
    }
  }

  private def makePriorIndex(): Unit = {
    synchronized {
      val p: Array[Int] = Array.ofDim[Int](numberOfNodes)
      Arrays.fill(p, 0, numberOfNodes, -1)
      for (i <- 0 until numberOfNodes) {
        val nextNode: Int = next(i)
        if (nextNode > i) {
          p(nextNode) = i
        }
      }
      prior = p
    }
  }

  def addAttribute(root: NodeInfo,
                   parent: Int,
                   nameCode: Int,
                   `type`: SimpleType,
                   attValue: CharSequence,
                   properties: Int): Unit = {
    ensureAttributeCapacity(1)
    attParent(numberOfAttributes) = parent
    attCode(numberOfAttributes) = nameCode
    this.attValue(numberOfAttributes) = attValue.toString
    if (`type` != BuiltInAtomicType.UNTYPED_ATOMIC) {
      initializeAttributeTypeCodes()
    }
    if (attType != null) {
      attType(numberOfAttributes) = `type`
    }
    if (alpha(parent) == -1) {
      alpha(parent) = numberOfAttributes
    }
    if (root.isInstanceOf[TinyDocumentImpl]) {
      var isID: Boolean = false
      try if (ReceiverOption.contains(properties, ReceiverOption.IS_ID)) {
        isID = true
      } else if ((nameCode & NamePool.FP_MASK) == StandardNames.XML_ID) {
        isID = true
      } else if (`type`.isIdType) {
        isID = true
      } catch {
        case e: MissingComponentException => {}

      }
      if (isID) {
        val id: String = Whitespace.trim(attValue)
        this.attValue(numberOfAttributes) = id
        if (NameChecker.isValidNCName(id)) {
          val e: NodeInfo = getNode(parent)
          registerID(e, id)
        } else if (attType != null) {
          attType(numberOfAttributes) = BuiltInAtomicType.UNTYPED_ATOMIC
        }
      }
      var isIDREF: Boolean = false
      try if (ReceiverOption.contains(properties, ReceiverOption.IS_IDREF)) {
        isIDREF = true
      } else if (`type` == BuiltInAtomicType.IDREF || `type` == BuiltInListType.IDREFS) {
        isIDREF = true
      } else if (`type`.isIdRefType) {
        try {
          //          val as: AtomicSequence = `type`.getTypedValue(
          //            attValue,
          //            null,
          //            getConfiguration.getConversionRules)
          //          for (v <- as if v.getItemType.isIdRefType) {
          //            isIDREF = true
          //            //break
          //          }

          val as = `type`.getTypedValue(attValue, null, getConfiguration.getConversionRules)
          breakable {
            for (v <- as.asScala) {
              if (v.getItemType.isIdRefType) {
                isIDREF = true
                break()
              }
            }
          }


        } catch {
          case ve: ValidationException => {}

        }
      } catch {
        case e: MissingComponentException => {}

      }
      if (isIDREF) {
        if (idRefAttributes == null) {
          idRefAttributes = new IntHashSet()
        }
        idRefAttributes.add(numberOfAttributes)
      }
    }
    {
      numberOfAttributes += 1;
    }
  }

  private def initializeAttributeTypeCodes(): Unit = {
    if (attType == null) {
      attType = Array.ofDim[SimpleType](attParent.length)
      Arrays.fill(attType.asInstanceOf[Array[AnyRef]],
        0,
        numberOfAttributes,
        BuiltInAtomicType.UNTYPED_ATOMIC)
    }
  }

  def markDefaultedAttribute(attNr: Int): Unit = {
    if (defaultedAttributes == null) {
      defaultedAttributes = new IntHashSet()
    }
    defaultedAttributes.add(attNr)
  }

  def isDefaultedAttribute(attNr: Int): Boolean =
    defaultedAttributes != null && defaultedAttributes.contains(attNr)

  def indexIDElement(root: NodeInfo, nodeNr: Int): Unit = {
    val id: String =
      Whitespace.trim(TinyParentNodeImpl.getStringValueCS(this, nodeNr))
    if (root.getNodeKind == Type.DOCUMENT && NameChecker.isValidNCName(id)) {
      val e: NodeInfo = getNode(nodeNr)
      registerID(e, id)
    }
  }

  def hasXmlSpacePreserveAttribute: Boolean =
    (0 until numberOfAttributes)
      .find(i =>
        (attCode(i) & NamePool.FP_MASK) == StandardNames.XML_SPACE &&
          "preserve" == attValue(i).toString)
      .map(_ => true)
      .getOrElse(false)

  /**
   * Add a set of namespace bindings to the current element
   *
   * @param parent the node number of the element
   * @param nsMap  namespace map identifying the prefix and uri
   */
  def addNamespaces(parent: Int, nsMap: NamespaceMap): Unit = {
    usesNamespaces = true
    for (i <- 0 until numberOfNamespaces if namespaceMaps(i) == nsMap) {
      beta(parent) = i
      return
    }
    ensureNamespaceCapacity(1)
    namespaceMaps(numberOfNamespaces) = nsMap
    beta(parent) = numberOfNamespaces
    numberOfNamespaces += 1
  }

  def getNode(nr: Int): TinyNodeImpl = nodeKind(nr) match {
    case Type.DOCUMENT => getRootNode.asInstanceOf[TinyDocumentImpl]
    case Type.ELEMENT => new TinyElementImpl(this, nr)
    case Type.TEXTUAL_ELEMENT => new TinyTextualElement(this, nr)
    case Type.TEXT => new TinyTextImpl(this, nr)
    case Type.WHITESPACE_TEXT => new WhitespaceTextImpl(this, nr)
    case Type.COMMENT => new TinyCommentImpl(this, nr)
    case Type.PROCESSING_INSTRUCTION => new TinyProcInstImpl(this, nr)
    case Type.PARENT_POINTER =>
      throw new IllegalArgumentException(
        "Attempting to treat a parent pointer as a node")
    case Type.STOPPER =>
      throw new IllegalArgumentException(
        "Attempting to treat a stopper entry as a node")
    case _ =>
      throw new IllegalStateException("Unknown node kind " + nodeKind(nr))

  }

  def getAtomizedValueOfUntypedNode(nodeNr: Int): AtomicValue =
    nodeKind(nodeNr) match {
      case Type.ELEMENT | Type.DOCUMENT =>
        var level: Int = depth(nodeNr)
        var next: Int = nodeNr + 1
        if (depth(next) <= level) {
          UntypedAtomicValue.ZERO_LENGTH_UNTYPED
        } else if (nodeKind(next) == Type.TEXT && depth(next + 1) <= level) {
          val length: Int = beta(next)
          val start: Int = alpha(next)
          new UntypedAtomicValue(charBuffer.subSequence(start, start + length))
        } else if (nodeKind(next) == Type.WHITESPACE_TEXT && depth(next + 1) <= level) {
          new UntypedAtomicValue(
            WhitespaceTextImpl.getStringValueCS(this, next))
        }
        var sb: FastStringBuffer = null
        while (next < numberOfNodes && depth(next) > level) {
          if (nodeKind(next) == Type.TEXT) {
            if (sb == null) {
              sb = new FastStringBuffer(FastStringBuffer.C256)
            }
            sb.cat(TinyTextImpl.getStringValue(this, next))
          } else if (nodeKind(next) == Type.WHITESPACE_TEXT) {
            if (sb == null) {
              sb = new FastStringBuffer(FastStringBuffer.C256)
            }
            WhitespaceTextImpl.appendStringValue(this, next, sb)
          }
          {
            next += 1; next - 1
          }
        }
        if (sb == null) {
          UntypedAtomicValue.ZERO_LENGTH_UNTYPED
        } else {
          new UntypedAtomicValue(sb.condense())
        }
      case Type.TEXT =>
        new UntypedAtomicValue(TinyTextImpl.getStringValue(this, nodeNr))
      case Type.WHITESPACE_TEXT =>
        new UntypedAtomicValue(
          WhitespaceTextImpl.getStringValueCS(this, nodeNr))
      case Type.COMMENT | Type.PROCESSING_INSTRUCTION =>
        var start2: Int = alpha(nodeNr)
        var len2: Int = beta(nodeNr)
        if (len2 == 0) {
          UntypedAtomicValue.ZERO_LENGTH_UNTYPED
        }
        var dest: Array[Char] = Array.ofDim[Char](len2)
        assert(commentBuffer != null)
        commentBuffer.getChars(start2, start2 + len2, dest, 0)
        new StringValue(new CharSlice(dest, 0, len2))
      case _ => throw new IllegalStateException("Unknown node kind")

    }

  /*@NotNull*/

  def getAttributeNode(nr: Int): TinyAttributeImpl =
    new TinyAttributeImpl(this, nr)

  def getAttributeAnnotation(nr: Int): Int =
    if (attType == null) {
      StandardNames.XS_UNTYPED_ATOMIC
    } else {
      attType(nr).getFingerprint
    }

  def getAttributeType(nr: Int): SimpleType =
    if (attType == null) {
      BuiltInAtomicType.UNTYPED_ATOMIC
    } else {
      attType(nr)
    }

  def isIdAttribute(nr: Int): Boolean =
    try attType != null && getAttributeType(nr).isIdType
    catch {
      case e: MissingComponentException => false

    }

  def isIdrefAttribute(nr: Int): Boolean =
    idRefAttributes != null && idRefAttributes.contains(nr)

  def isIdElement(nr: Int): Boolean =
    try getSchemaType(nr).isIdType && getTypedValueOfElement(nr).getLength == 1
    catch {
      case e: XPathException => false

    }

  def isIdrefElement(nr: Int): Boolean = {
    val `type`: SchemaType = getSchemaType(nr)
    try if (`type`.isIdRefType) {
      if (`type` == BuiltInAtomicType.IDREF || `type` == BuiltInListType.IDREFS) {
        return true
      }
      try for (av <- getTypedValueOfElement(nr).asScala
               if av.getItemType.isIdRefType) {
        true
      } catch {
        case _: XPathException =>

      }
    } catch {
      case e: MissingComponentException => return false

    }
    false
  }

  def setSystemId(seq: Int, uri: String): Unit = {
    var uriStr: String = uri
    if (uriStr == null) {
      uriStr = ""
    }
    if (systemIdMap == null) {
      systemIdMap = new SystemIdMap()
    }
    systemIdMap.setSystemId(seq, uriStr)
  }

  /*@Nullable*/

  def getSystemId(seq: Int): String = {
    if (systemIdMap == null) {
      return null
    }
    systemIdMap.getSystemId(seq)
  }

  override def getRootNode: NodeInfo =
    if (getNodeKind(0) == Type.DOCUMENT) {
      if (root != null) {
        root
      } else {
        root = new TinyDocumentImpl(this)
        root
      }
    } else {
      getNode(0)
    }

  def setLineNumbering(): Unit = {
    lineNumbers = Array.ofDim[Int](nodeKind.length)
    Arrays.fill(lineNumbers, -1)
    columnNumbers = Array.ofDim[Int](nodeKind.length)
    Arrays.fill(columnNumbers, -1)
  }

  def setLineNumber(sequence: Int, line: Int, column: Int): Unit = {
    if (lineNumbers != null) {
      assert(columnNumbers != null)
      lineNumbers(sequence) = line
      columnNumbers(sequence) = column
    }
  }

  def getLineNumber(sequence: Int): Int = {
    if (lineNumbers != null) {
      var i: Int = sequence
      while (i >= 0) {
        val c: Int = lineNumbers(i)
        if (c > 0) {
          return c
        }
        {
          i -= 1; i + 1
        }
      }
    }
    -1
  }

  def getColumnNumber(sequence: Int): Int = {
    if (columnNumbers != null) {
      var i: Int = sequence
      while (i >= 0) {
        val c: Int = columnNumbers(i)
        if (c > 0) {
          return c
        }
        {
          i -= 1; i + 1
        }
      }
    }
    -1
  }

  def setNilled(nodeNr: Int): Unit = {
    if (nilledElements == null) {
      nilledElements = new IntHashSet()
    }
    nilledElements.add(nodeNr)
  }

  def isNilled(nodeNr: Int): Boolean =
    nilledElements != null && nilledElements.contains(nodeNr)

  def registerID(e: NodeInfo, id: String): Unit = {
    if (idTable == null) {
      idTable = new HashMap(256)
    }
    idTable.putIfAbsent(id, e)
  }

  /*@Nullable*/

  override def selectID(id: String, getParent: Boolean): NodeInfo = {
    if (idTable == null) {
      return null
    }
    var node: NodeInfo = idTable.get(id)
    if (node != null && getParent && node.isId && node.getStringValue == id) {
      node = node.getParent
    }
    node
  }

  def setUnparsedEntity(name: String, uri: String, publicId: String): Unit = {
    if (entityTable == null) {
      entityTable = new HashMap(20)
    }
    val ids: Array[String] = Array.ofDim[String](2)
    ids(0) = uri
    ids(1) = publicId
    entityTable.put(name, ids)
  }

  override def getUnparsedEntityNames: Iterator[String] =
    if (entityTable == null) {
      val emptyList: List[String] = Collections.emptyList()
      emptyList.iterator()
    } else {
      entityTable.keySet.iterator()
    }

  /*@Nullable*/

  override def getUnparsedEntity(name: String): Array[String] = {
    if (entityTable == null) {
      return null
    }
    entityTable.get(name)
  }

  def getNamePool: NamePool = getConfiguration.getNamePool

  def markTopWithinEntity(nodeNr: Int): Unit = {
    if (topWithinEntity == null) {
      topWithinEntity = new IntHashSet()
    }
    topWithinEntity.add(nodeNr)
  }

  def isTopWithinEntity(nodeNr: Int): Boolean =
    topWithinEntity != null && topWithinEntity.contains(nodeNr)

  def diagnosticDump(): Unit = {
    val pool: NamePool = getNamePool
    System.err.println(
      "    node    kind   depth    next   alpha    beta    name    type")
    for (i <- 0 until numberOfNodes) {
      var eqName: String = ""
      if (nameCode(i) != -1) {
        try eqName = pool.getEQName(nameCode(i))
        catch {
          case err: Exception => eqName = "#" + nameCode(1)

        }
      }
      System.err.println(
        n8(i) + n8(nodeKind(i)) + n8(depth(i)) + n8(next(i)) +
          n8(alpha(i)) +
          n8(beta(i)) +
          n8(nameCode(i)) +
          n8(getTypeAnnotation(i)) +
          " " +
          eqName)
    }
    System.err.println("    attr  parent    name    value")
    for (i <- 0 until numberOfAttributes) {
      System.err.println(
        n8(i) + n8(attParent(i)) + n8(attCode(i)) + "    " + attValue(i))
    }
    System.err.println("      ns  parent  prefix     uri")
    for (i <- 0 until numberOfNamespaces) {
      System.err.println(n8(i) + "  " + namespaceMaps(i))
    }
  }

  def showSize(): Unit = {
    System.err.println(
      "Tree size: " + numberOfNodes + " nodes, " + charBuffer.length +
        " characters, " +
        numberOfAttributes +
        " attributes")
  }

  /**
   * Ask whether the document contains any nodes whose type annotation is anything other than
   * UNTYPED
   *
   * @return true if the document contains elements whose type is other than UNTYPED
   */
  override def isTyped: Boolean = typeArray != null

  def getNumberOfNodes: Int = numberOfNodes

  def getNumberOfAttributes: Int = numberOfAttributes

  def getNumberOfNamespaces: Int = numberOfNamespaces

  def getNodeKindArray(): Array[Byte] = nodeKind

  def getNodeDepthArray: Array[Short] = depth

  def getNameCodeArray(): Array[Int] = nameCode

  /*@Nullable*/

  def getTypeArray: Array[SchemaType] = typeArray

  def getNextPointerArray: Array[Int] = next

  def getAlphaArray: Array[Int] = alpha

  def getBetaArray: Array[Int] = beta

  def getCharacterBuffer: AppendableCharSequence = charBuffer

  /*@Nullable*/

  def getCommentBuffer: CharSequence = commentBuffer

  def getAttributeNameCodeArray: Array[Int] = attCode

  /*@Nullable*/

  def getAttributeTypeArray: Array[SimpleType] = attType

  def getAttributeParentArray: Array[Int] = attParent

  def getAttributeValueArray: Array[CharSequence] = attValue

  def getNamespaceBindings: Array[NamespaceBinding] =
    throw new UnsupportedOperationException()

  def getNamespaceMaps: Array[NamespaceMap] = namespaceMaps

  def getNamespaceParentArray: Array[Int] =
    throw new UnsupportedOperationException()

  def isUsesNamespaces: Boolean = usesNamespaces

  def bulkCopy(source: TinyTree,
               nodeNr: Int,
               currentDepth: Int,
               parentNodeNr: Int): Unit = {
    var end: Int = source.next(nodeNr)
    while (end < nodeNr && end >= 0) end = source.next(end)
    if (end == -1) {
      end = source.numberOfNodes
      if (end - 1 < source.nodeKind.length && source.nodeKind(end - 1) == Type.STOPPER) {
        {
          end -= 1;
        }
      }
    }
    val length: Int = end - nodeNr
    assert(length > 0)
    ensureNodeCapacity(Type.ELEMENT, length)
    System.arraycopy(source.nodeKind, nodeNr, nodeKind, numberOfNodes, length)
    val depthDiff: Int = currentDepth - source.depth(nodeNr)
    val subtreeRoot: NamespaceMap = source.namespaceMaps(source.beta(nodeNr))
    val inherited: NamespaceMap = namespaceMaps(beta(parentNodeNr))
    val sameNamespaces: Boolean = subtreeRoot == inherited || inherited.isEmpty
    for (i <- 0 until length) {
      val from: Int = nodeNr + i
      val to: Int = numberOfNodes + i
      depth(to) = (source.depth(from) + depthDiff).toShort
      next(to) = source.next(from) + (to - from)
      source.nodeKind(from) match {
        case Type.ELEMENT => {
          nameCode(to) = (source.nameCode(from) & NamePool.FP_MASK) |
            (prefixPool.obtainPrefixCode(source.getPrefix(from)) <<
              20)
          val firstAtt: Int = source.alpha(from)
          if (firstAtt >= 0) {
            var lastAtt: Int = firstAtt
            while (lastAtt < source.numberOfAttributes && source.attParent(
              lastAtt) == from) {
              lastAtt += 1; lastAtt - 1
            }
            val atts: Int = lastAtt - firstAtt
            ensureAttributeCapacity(atts)
            var aFrom: Int = firstAtt
            var aTo: Int = numberOfAttributes
            alpha(to) = aTo
            System.arraycopy(source.attValue, firstAtt, attValue, aTo, atts)
            Arrays.fill(attParent, aTo, aTo + atts, to)
            var a: Int = 0
            while (a < atts) {
              attCode(aTo) = source.attCode(aFrom)
              val attNameCode: Int = attCode(aTo)
              if (NamePool.isPrefixed(attNameCode)) {
                val prefix: String =
                  source.prefixPool.getPrefix(attNameCode >> 20)
                attCode(aTo) = (attNameCode & 0xfffff) | (prefixPool
                  .obtainPrefixCode(prefix) << 20)
              } else {
                attCode(aTo) = attNameCode
              }
              if (source.isIdAttribute(aFrom)) {
                registerID(getNode(to), source.attValue(aFrom).toString)
              }
              if (source.isIdrefAttribute(aFrom)) {
                if (idRefAttributes == null) {
                  idRefAttributes = new IntHashSet()
                }
                idRefAttributes.add(aTo)
              }
              {
                a += 1;
              }
              {
                aFrom += 1;
              }
              {
                aTo += 1; aTo - 1
              }
            }
            numberOfAttributes += atts
          } else {
            alpha(to) = -1
          }
          if (sameNamespaces) {
            if (source.beta(from) == source.beta(nodeNr)) {
              beta(to) = beta(parentNodeNr)
            } else {
              ensureNamespaceCapacity(1)
              namespaceMaps(numberOfNamespaces) =
                source.namespaceMaps(source.beta(nodeNr))
              beta(to) = {
                numberOfNamespaces += 1; numberOfNamespaces - 1
              }
            }
          } else {
            if (i > 0 && source.beta(from) == source.beta(nodeNr)) {
              beta(to) = beta(parentNodeNr)
            } else {
              ensureNamespaceCapacity(1)
              val in: NamespaceMap = source.namespaceMaps(source.beta(from))
              val out: NamespaceMap = inherited.putAll(in)
              namespaceMaps(numberOfNamespaces) = out
              beta(to) = {
                numberOfNamespaces += 1; numberOfNamespaces - 1
              }
            }
          }
        }
        case Type.TEXTUAL_ELEMENT => {
          val start: Int = source.alpha(from)
          val len: Int = source.beta(from)
          nameCode(to) = (source.nameCode(from) & NamePool.FP_MASK) |
            (prefixPool.obtainPrefixCode(source.getPrefix(from)) <<
              20)
          alpha(to) = charBuffer.length
          appendChars(source.charBuffer.subSequence(start, start + len))
          beta(to) = len
        }
        case Type.TEXT => {
          val start: Int = source.alpha(from)
          val len: Int = source.beta(from)
          nameCode(to) = -1
          alpha(to) = charBuffer.length
          appendChars(source.charBuffer.subSequence(start, start + len))
          beta(to) = len
        }
        case Type.WHITESPACE_TEXT => {
          nameCode(to) = -1
          alpha(to) = source.alpha(from)
          beta(to) = source.beta(from)
        }
        case Type.COMMENT => {
          val start: Int = source.alpha(from)
          val len: Int = source.beta(from)
          nameCode(to) = -1
          val text: CharSequence =
            source.commentBuffer.subSequence(start, start + len)
          if (commentBuffer == null) {
            commentBuffer = new FastStringBuffer(FastStringBuffer.C256)
          }
          alpha(to) = commentBuffer.length
          commentBuffer.cat(text)
          beta(to) = len
        }
        case Type.PROCESSING_INSTRUCTION =>
          var start: Int = source.alpha(from)
          var len: Int = source.beta(from)
          nameCode(to) = source.nameCode(from)
          var text: CharSequence =
            source.commentBuffer.subSequence(start, start + len)
          if (commentBuffer == null) {
            commentBuffer = new FastStringBuffer(FastStringBuffer.C256)
          }
          alpha(to) = commentBuffer.length
          commentBuffer.cat(text)
          beta(to) = len
        case Type.PARENT_POINTER =>
          nameCode(to) = -1
          alpha(to) = source.alpha(from) + (to - from)
          beta(to) = -1
        case _ =>

      }
    }
    numberOfNodes += length
  }

  def getLocalNameIndex: Map[String, IntSet] = synchronized {
    if (localNameIndex == null) {
      localNameIndex = new HashMap()
      val indexed: IntHashSet = new IntHashSet()
      for (i <- 0 until numberOfNodes if (nodeKind(i) & 0xf) == Type.ELEMENT) {
        val fp: Int = nameCode(i) & NamePool.FP_MASK
        if (!indexed.contains(fp)) {
          val local: String = getNamePool.getLocalName(fp)
          indexed.add(fp)
          val existing: IntSet = localNameIndex.get(local)
          if (existing == null) {
            localNameIndex.put(local, new IntSingletonSet(fp))
          } else {
            val copy: IntSet =
              if (existing.isMutable) existing else existing.mutableCopy()
            copy.add(fp)
            localNameIndex.put(local, copy)
          }
        }
      }
    }
    localNameIndex
  }

}

