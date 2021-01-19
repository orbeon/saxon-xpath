package org.orbeon.saxon.tree.tiny

import org.orbeon.saxon.utils.Configuration
import org.orbeon.saxon.event.CopyInformee
import org.orbeon.saxon.event.CopyNamespaceSensitiveException
import org.orbeon.saxon.event.Receiver
import org.orbeon.saxon.event.ReceiverOption
import org.orbeon.saxon.expr.parser.Loc
import org.orbeon.saxon.model._
import org.orbeon.saxon.om._
import org.orbeon.saxon.s9api.Location
import org.orbeon.saxon.trans.XPathException
import org.orbeon.saxon.tree.tiny.TinyNodeImpl.getParentNodeNr
import org.orbeon.saxon.tree.util.Navigator
import org.orbeon.saxon.value.AtomicValue
import org.orbeon.saxon.z.IntHashMap

//import scala.collection.compat._
import scala.jdk.CollectionConverters._

class TinyElementImpl(treeImpl: TinyTree, nodeNrImpl: Int) extends TinyParentNodeImpl {

  this.tree = treeImpl

  this.nodeNr = nodeNrImpl

  def getNodeKind: Int = Type.ELEMENT

  override def getBaseURI: String = {
    if (tree.getUniformBaseUri != null) {
     return tree.getUniformBaseUri
    }
    tree.synchronized {
      if (tree.knownBaseUris == null) {
        tree.knownBaseUris = new IntHashMap()
      }
      var uri: String = tree.knownBaseUris.get(nodeNr)
      if (uri == null) {
        uri = Navigator.getBaseURI(
          this,
          (n:NodeInfo) =>
            tree.isTopWithinEntity(
              n.asInstanceOf[TinyElementImpl].getNodeNumber))
        tree.knownBaseUris.put(nodeNr, uri)
      }
      uri
    }
  }

  override def getSchemaType: SchemaType = tree.getSchemaType(nodeNr)

  def atomize(): AtomicSequence = tree.getTypedValueOfElement(this)

  override def getDeclaredNamespaces(
                             buffer: Array[NamespaceBinding]): Array[NamespaceBinding] = {
    val parent: TinyNodeImpl = getParent
    if (parent != null && parent.getNodeKind == Type.ELEMENT) {
      getAllNamespaces.getDifferences(parent.getAllNamespaces, addUndeclarations = false)
    } else {
      getAllNamespaces.getNamespaceBindings
    }
  }

  override def getAllNamespaces: NamespaceMap = tree.namespaceMaps(tree.beta(nodeNr))

  def hasUniformNamespaces: Boolean = false

  override def getAttributeValue(uri: String, local: String): String = {
    var a: Int = tree.alpha(nodeNr)
    if (a < 0) {
      return null
    }
    val pool: NamePool = getNamePool
    while (a < tree.numberOfAttributes && tree.attParent(a) == nodeNr) {
      val nc   = tree.attCode(a)
      val name = pool.getUnprefixedQName(nc)
      if (name.getLocalPart == local && name.hasURI(uri))
        return tree.attValue(a).toString
      a += 1
    }
    null
  }

  def getAttributeValue(fp: Int): String = {
    var a: Int = tree.alpha(nodeNr)
    if (a < 0) {
      return null
    }
    while (a < tree.numberOfAttributes && tree.attParent(a) == nodeNr) {
      if (fp == (tree.attCode(a) & NamePool.FP_MASK))
        return tree.attValue(a).toString
      a += 1
    }
    null
  }

  private def subtreeSize(): Int = {
    var next: Int = tree.next(nodeNr)
    while (next < nodeNr) {
      if (next < 0) {
        return tree.numberOfNodes - nodeNr
      }
      next = tree.next(next)
    }
    nodeNr - next
  }

  override def copy(receiver: Receiver, copyOptions: Int, location: Location): Unit = {
    var lLocation = location
    val copyTypes: Boolean =
      CopyOptions.includes(copyOptions, CopyOptions.TYPE_ANNOTATIONS)
    var level: Int = -1
    var closePending: Boolean = false
    val startLevel: Short = tree.depth(nodeNr)
    val disallowNamespaceSensitiveContent: Boolean = ((copyOptions & CopyOptions.TYPE_ANNOTATIONS) != 0) &&
      ((copyOptions & CopyOptions.ALL_NAMESPACES) == 0)
    val config: Configuration = tree.getConfiguration
    val pool: NamePool = config.getNamePool
    var next: Int = nodeNr
    val informee: CopyInformee[Location] = receiver.getPipelineConfiguration
      .getComponent(classOf[CopyInformee[_]].getName)
      .asInstanceOf[CopyInformee[Location]]
    var elementType: SchemaType = Untyped.getInstance
    var attributeType: SimpleType = BuiltInAtomicType.UNTYPED_ATOMIC
    do {
      val nodeLevel: Short = tree.depth(next)
      if (closePending) {
        { level += 1;}
      }

      while (level > nodeLevel) {
        receiver.endElement()
        level = (level - 1)
      }
      level = nodeLevel
      val kind: Int = tree.nodeKind(next)
      kind match {
        case Type.ELEMENT | Type.TEXTUAL_ELEMENT => {
          if (copyTypes) {
            elementType = tree.getSchemaType(next)
            if (disallowNamespaceSensitiveContent) {
              try checkNotNamespaceSensitiveElement(elementType, next)
              catch {
                case e: CopyNamespaceSensitiveException => {
                  e.setErrorCode(
                    if (receiver.getPipelineConfiguration.isXSLT) "XTTE0950"
                    else "XQTY0086")
                  throw e
                }

              }
            }
          }
          if (informee != null) {
            val loc: Location = informee.notifyElementNode(tree.getNode(next))
            if (loc != null) {
              lLocation = loc
            }
          }
          val nameCode: Int = tree.nameCode(next)
          val fp: Int = nameCode & NamePool.FP_MASK
          val prefix: String = tree.getPrefix(next)
          if (lLocation.getLineNumber < tree.getLineNumber(next)) {
            val systemId: String =
              if (lLocation.getSystemId == null) getSystemId
              else lLocation.getSystemId
            lLocation =
              new Loc(systemId, tree.getLineNumber(next), getColumnNumber)
          }
          var namespaces: NamespaceMap = NamespaceMap.emptyMap
          var addAttributeNamespaces: Boolean = false
          if (tree.usesNamespaces) {
            if ((copyOptions & CopyOptions.ALL_NAMESPACES) != 0) {
              if (kind == Type.TEXTUAL_ELEMENT) {
                val parent: Int = getParentNodeNr(tree, next)
                namespaces = tree.namespaceMaps(tree.beta(parent))
              } else {
                namespaces = tree.namespaceMaps(tree.beta(next))
              }
            } else {
              addAttributeNamespaces = true
              val uri: String = pool.getURI(nameCode)
              if (!uri.isEmpty) {
                namespaces = NamespaceMap.of(prefix, uri)
              }
            }
          }
          if (kind == Type.TEXTUAL_ELEMENT) {
            closePending = false
            receiver.startElement(
              new CodedName(fp, prefix, pool),
              elementType,
              EmptyAttributeMap.getInstance,
              namespaces,
              lLocation,
              ReceiverOption.BEQUEATH_INHERITED_NAMESPACES_ONLY)
            val value: CharSequence = TinyTextImpl.getStringValue(tree, next)
            receiver.characters(value,
              lLocation,
              ReceiverOption.WHOLE_TEXT_NODE)
            receiver.endElement()
          } else {
            closePending = true
            var attributes: AttributeMap = EmptyAttributeMap.getInstance
            var att: Int = tree.alpha(next)
            if (att >= 0) {
              while (att < tree.numberOfAttributes && tree.attParent(att) == next) {
                val attCode: Int = tree.attCode(att)
                val attfp: Int = attCode & NamePool.FP_MASK
                if (copyTypes) {
                  attributeType = tree.getAttributeType(att)
                  if (disallowNamespaceSensitiveContent) {
                    try checkNotNamespaceSensitiveAttribute(attributeType, att)
                    catch {
                      case e: CopyNamespaceSensitiveException => {
                        e.setErrorCode(
                          if (receiver.getPipelineConfiguration.isXSLT)
                            "XTTE0950"
                          else "XQTY0086")
                        throw e
                      }

                    }
                  }
                }
                val attPrefix: String =
                  tree.prefixPool.getPrefix(attCode >> 20)
                var attProps: Int = ReceiverOption.NOT_A_DUPLICATE
                if (tree.isIdAttribute(att)) {
                  attProps |= ReceiverOption.IS_ID
                }
                if (tree.isIdrefAttribute(att)) {
                  attProps |= ReceiverOption.IS_IDREF
                }
                attributes = attributes.put(
                  new AttributeInfo(new CodedName(attfp, attPrefix, pool),
                    attributeType,
                    tree.attValue(att).toString,
                    location,
                    attProps))
                if (addAttributeNamespaces && !attPrefix.isEmpty) {
                  namespaces = namespaces.put(attPrefix, pool.getURI(attCode))
                }
                { att += 1; att - 1 }
              }
            }
            receiver.startElement(
              new CodedName(fp, prefix, pool),
              elementType,
              attributes,
              namespaces,
              lLocation,
              ReceiverOption.BEQUEATH_INHERITED_NAMESPACES_ONLY)
          }
        }
        case Type.TEXT => {
          closePending = false
          val value: CharSequence = TinyTextImpl.getStringValue(tree, next)
          receiver.characters(value, location, ReceiverOption.WHOLE_TEXT_NODE)
        }
        case Type.WHITESPACE_TEXT => {
          closePending = false
          val value: CharSequence =
            WhitespaceTextImpl.getStringValueCS(tree, next)
          receiver.characters(value, location, ReceiverOption.WHOLE_TEXT_NODE)
        }
        case Type.COMMENT => {
          closePending = false
          val start: Int = tree.alpha(next)
          val len = tree.beta(next)
          if (len > 0) {
            receiver.comment(
              tree.commentBuffer.subSequence(start, start + len),
              lLocation,
              ReceiverOption.NONE)
          } else {
            receiver.comment("", Loc.NONE, ReceiverOption.NONE)
          }
        }
        case Type.PROCESSING_INSTRUCTION => {
          closePending = false
          val pi: NodeInfo = tree.getNode(next)
          receiver.processingInstruction(pi.getLocalPart,
            pi.getStringValue,
            lLocation,
            ReceiverOption.NONE)
        }
        case Type.PARENT_POINTER => closePending = false

      }
      next = next + 1
    } while (next < tree.numberOfNodes && tree.depth(next) > startLevel);
    if (closePending) {
      level = level + 1
    }

    while (level > startLevel) {
      receiver.endElement()
      level = level - 1
    }
  }

   def checkNotNamespaceSensitiveElement(`type`: SchemaType,
                                                  nodeNr: Int): Unit = {
    if (`type`.isInstanceOf[SimpleType] &&
      `type`.asInstanceOf[SimpleType].isNamespaceSensitive) {
      if (`type`.isAtomicType) {
        throw new CopyNamespaceSensitiveException(
          "Cannot copy QName or NOTATION values without copying namespaces")
      } else {
        val value: AtomicSequence = tree.getTypedValueOfElement(nodeNr)
        for (lVal <- value.asScala if lVal.getPrimitiveType.isNamespaceSensitive) {
          throw new CopyNamespaceSensitiveException(
            "Cannot copy QName or NOTATION values without copying namespaces")
        }
      }
    }
  }

  private def checkNotNamespaceSensitiveAttribute(`type`: SimpleType,
                                                  nodeNr: Int): Unit = {
    if (`type`.isNamespaceSensitive) {
      if (`type`.isAtomicType) {
        throw new CopyNamespaceSensitiveException(
          "Cannot copy QName or NOTATION values without copying namespaces")
      } else {
        val value: AtomicSequence = tree.getTypedValueOfAttribute(null, nodeNr)
        for (lVal <- value.asScala if lVal.getPrimitiveType.isNamespaceSensitive) {
          throw new CopyNamespaceSensitiveException(
            "Cannot copy QName or NOTATION values without copying namespaces")
        }
      }
    }
  }

  def getURIForPrefix(prefix: String, useDefault: Boolean): String = {
    if (!useDefault && (prefix == null || prefix.isEmpty)) {
      return ""
    }
    val ns: Int = tree.beta(nodeNr)
    val map: NamespaceMap = tree.namespaceMaps(ns)
    map.getURIForPrefix(prefix, useDefault)
  }

  override def isId: Boolean = tree.isIdElement(nodeNr)

  override def isIdref(): Boolean = tree.isIdrefElement(nodeNr)

  private def isSkipValidator(r: Receiver): Boolean = false

}