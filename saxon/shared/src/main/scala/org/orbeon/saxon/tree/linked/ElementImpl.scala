package org.orbeon.saxon.tree.linked

import java.util.function.Predicate

import org.orbeon.saxon.event.{CopyInformee, CopyNamespaceSensitiveException, Receiver, ReceiverOption}
import org.orbeon.saxon.expr.parser.Loc
import org.orbeon.saxon.lib.NamespaceConstant
import org.orbeon.saxon.model._
import org.orbeon.saxon.om._
import org.orbeon.saxon.pattern.NodeKindTest
import org.orbeon.saxon.s9api.Location
import org.orbeon.saxon.trans.XPathException
import org.orbeon.saxon.tree.iter.AxisIterator
import org.orbeon.saxon.tree.linked.ElementImpl.isIdRefNode
import org.orbeon.saxon.tree.util.{FastStringBuffer, Navigator}
import org.orbeon.saxon.value.Whitespace

import java.{util => ju}

import scala.jdk.CollectionConverters._
import scala.util.control.Breaks._

object ElementImpl {

  def isIdRefNode(node: NodeImpl): Boolean = {
    val `type`: SchemaType = node.getSchemaType
    try if (`type`.isIdRefType) {
      if (`type` == BuiltInAtomicType.IDREF || `type` == BuiltInListType.IDREFS) {
        return true
      }
      try
        for (av <- node.atomize().asScala if av.getItemType.isIdRefType) {
          return true
        }
      catch {
        case _: XPathException =>
      }
    } catch {
      case _: MissingComponentException =>
        return false
    }
    false
  }
}

class ElementImpl extends ParentNodeImpl with NamespaceResolver {

  var nodeN: NodeName = _

  private var `type`: SchemaType = Untyped.getInstance

  private var attributeMap: AttributeMap = EmptyAttributeMap.getInstance

  private var namespaceMap: NamespaceMap = NamespaceMap.emptyMap

  override def setAttributes(atts: AttributeMap): Unit = {
    this.attributeMap = atts
  }

  def initialise(elemName: NodeName,
                 elementType: SchemaType,
                 atts: AttributeMap,
                 parent: NodeInfo,
                 sequenceNumber: Int): Unit = {
    this.nodeN = elemName
    this.`type` = elementType
    this.setRawParent(parent.asInstanceOf[ParentNodeImpl])
    this.setRawSequenceNumber(sequenceNumber)
    attributeMap = atts
  }

  def setLocation(systemId: String, line: Int, column: Int): Unit = {
    val root: DocumentImpl = getRawParent.getPhysicalRoot
    root.setLineAndColumn(getRawSequenceNumber, line, column)
    root.setSystemId(getRawSequenceNumber, systemId)
  }

  override def setSystemId(uri: String): Unit = {
    getPhysicalRoot.setSystemId(getRawSequenceNumber, uri)
  }

  override def getRoot: NodeInfo = {
    val up: ParentNodeImpl = getRawParent
    if (up == null ||
      (up.isInstanceOf[DocumentImpl] && up
        .asInstanceOf[DocumentImpl]
        .imaginary)) {
      this
    } else {
      up.getRoot
    }
  }

  override def getSystemId: String = {
    val root: DocumentImpl = getPhysicalRoot
    if (root == null) null else root.getSystemId(getRawSequenceNumber)
  }

  override def getBaseURI: String =
    Navigator.getBaseURI(
      this.asInstanceOf[NodeInfo],
      (n: NodeInfo) => getPhysicalRoot.isTopWithinEntity(n.asInstanceOf[ElementImpl]))

  override def isNilled: Boolean = getPhysicalRoot.isNilledElement(this)

  override def setTypeAnnotation(`type`: SchemaType): Unit =
    this.`type` = `type`

  def setNilled(): Unit =
    getPhysicalRoot.addNilledElement(this)

  override def getSchemaType: SchemaType = `type`

  override def getLineNumber: Int = {
    val root: DocumentImpl = getPhysicalRoot
    if (root == null) {
      -1
    } else {
      root.getLineNumber(getRawSequenceNumber)
    }
  }

  override def getColumnNumber(): Int = {
    val root: DocumentImpl = getPhysicalRoot
    if (root == null) {
      -1
    } else {
      root.getColumnNumber(getRawSequenceNumber)
    }
  }

  override def generateId(buffer: FastStringBuffer): Unit = {
    val sequence: Int = getRawSequenceNumber
    if (sequence >= 0) {
      getPhysicalRoot.generateId(buffer)
      buffer.append("e")
      buffer.append(java.lang.Integer.toString(sequence))
    } else {
      getRawParent.generateId(buffer)
      buffer.append("f")
      buffer.append(java.lang.Integer.toString(getSiblingPosition))
    }
  }

  def getNodeKind: Int = Type.ELEMENT

  override def attributes: AttributeMap = attributeMap

  def iterateAttributes(test: Predicate[_ >: NodeInfo]): AxisIterator =
    attributeMap match {
      case withIdentity: AttributeMapWithIdentity =>
        new Navigator.AxisFilter(withIdentity.iterateAttributes(this), test)
      case _ =>
        new AttributeAxisIterator(this, test)
    }

  override def copy(out: Receiver, copyOptions: Int, location: Location): Unit = {
    var lLocation = location
    val copyTypes = CopyOptions.includes(copyOptions, CopyOptions.TYPE_ANNOTATIONS)
    val typeCode: SchemaType =
      if (copyTypes)
        getSchemaType
      else Untyped.getInstance
    val informee: CopyInformee[AnyRef] = out.getPipelineConfiguration
      .getComponent(classOf[CopyInformee[AnyRef]].getName)
      .asInstanceOf[CopyInformee[AnyRef]]
    if (informee != null) {
      val o: AnyRef = informee.notifyElementNode(this)
      o match {
        case l: Location =>
          lLocation = l
        case _ =>
      }
    }
    val ns: NamespaceMap =
      if (CopyOptions.includes(copyOptions, CopyOptions.ALL_NAMESPACES))
        getAllNamespaces
      else NamespaceMap.emptyMap

    val disallowNamespaceSensitiveContent =
      ((copyOptions & CopyOptions.TYPE_ANNOTATIONS) != 0) &&
        ((copyOptions & CopyOptions.ALL_NAMESPACES) == 0)
    if (copyTypes && disallowNamespaceSensitiveContent) {
      try {
        checkNotNamespaceSensitiveElement(getSchemaType)
      } catch {
        case e: CopyNamespaceSensitiveException =>
          e.setErrorCode(if (out.getPipelineConfiguration.isXSLT) "XTTE0950" else "XQTY0086")
          throw e
      }
    }

    val atts: ju.List[AttributeInfo] =
      new ju.ArrayList[AttributeInfo](attributes.size)
    for (att <- attributes.iterator.asScala) {
      var attributeType: SimpleType = BuiltInAtomicType.UNTYPED_ATOMIC
      if (copyTypes) {
        attributeType = att.`type`
        if (disallowNamespaceSensitiveContent) {
          try {
            checkNotNamespaceSensitiveAttribute(attributeType, att)
          } catch {
            case e: CopyNamespaceSensitiveException =>
              e.setErrorCode(if (out.getPipelineConfiguration.isXSLT) "XTTE0950" else "XQTY0086")
              throw e
          }
        }
      }
      atts.add(
        new AttributeInfo(att.nodeName,
          attributeType,
          att.value,
          att.location,
          0))
    }
    out.startElement(
      NameOfNode.makeName(this),
      typeCode,
      AttributeMap.fromList(atts),
      ns,
      lLocation,
      ReceiverOption.BEQUEATH_INHERITED_NAMESPACES_ONLY | ReceiverOption.NAMESPACE_OK
    )
    var next: NodeImpl = getFirstChild
    while (next != null) {
      next.copy(out, copyOptions, location)
      next = next.getNextSibling
    }
    out.endElement()
  }

  protected def checkNotNamespaceSensitiveElement(`type`: SchemaType): Unit = {
    if (`type`.isInstanceOf[SimpleType] && `type`.asInstanceOf[SimpleType].isNamespaceSensitive) {
      if (`type`.isAtomicType) {
        throw new CopyNamespaceSensitiveException(
          "Cannot copy QName or NOTATION values without copying namespaces")
      } else {
        val value = atomize()
        for (valItem <- value.asScala) {
          if (valItem.getPrimitiveType.isNamespaceSensitive) {
            throw new CopyNamespaceSensitiveException(
              "Cannot copy QName or NOTATION values without copying namespaces")
          }
        }
      }
    }
  }

  private def checkNotNamespaceSensitiveAttribute(`type`: SimpleType, att: AttributeInfo): Unit = {
    if (`type`.isNamespaceSensitive) {
      if (`type`.isAtomicType) {
        throw new CopyNamespaceSensitiveException(
          "Cannot copy QName or NOTATION values without copying namespaces")
      } else {
        val value = `type`.getTypedValue(att.value, namespaceMap, getConfiguration.getConversionRules)
        for (valItem <- value.asScala) {
          if (valItem.getPrimitiveType.isNamespaceSensitive) {
            throw new CopyNamespaceSensitiveException(
              "Cannot copy QName or NOTATION values without copying namespaces")
          }
        }
      }
    }
  }

  override def delete(): Unit = {
    val root: DocumentImpl = getPhysicalRoot
    super.delete()
    if (root != null) {
      val iter: AxisIterator =
        iterateAxis(AxisInfo.DESCENDANT_OR_SELF, NodeKindTest.ELEMENT)
      breakable {
        while (true) {
          val n: ElementImpl = iter.next().asInstanceOf[ElementImpl]
          for (att <- attributeMap.iterator.asScala if att.isId) {
            root.deregisterID(att.value)
          }
          if (n == null) {
            break()
          }
          root.deIndex(n)
        }
      }
    }
  }

  override def rename(newName: NodeName, inheritNamespaces: Boolean): Unit = {
    val prefix: String = newName.getPrefix
    val uri: String = newName.getURI
    val ns: NamespaceBinding = new NamespaceBinding(prefix, uri)
    var uc: String = getURIForPrefix(prefix, useDefault = true)
    if (uc == null) {
      uc = ""
    }
    if (uc != uri) {
      if (uc.isEmpty) {
        addNamespace(ns, inheritNamespaces)
      } else {
        throw new IllegalArgumentException(
          "Namespace binding of new name conflicts with existing namespace binding")
      }
    }
    nodeN = newName
  }

  override def addNamespace(binding: NamespaceBinding, inheritNamespaces: Boolean): Unit = {
    if (binding.getURI.isEmpty) {
      throw new IllegalArgumentException(
        "Cannot add a namespace undeclaration")
    }
    val existing: String = namespaceMap.getURI(binding.prefix)
    if (existing != null) {
      if (existing != binding.getURI) {
        throw new IllegalArgumentException(
          "New namespace conflicts with existing namespace binding")
      }
    } else {
      namespaceMap = namespaceMap.put(binding.prefix, binding.getURI)
    }
  }

  def replaceStringValue(stringValue: CharSequence): Unit = {
    if (stringValue.length == 0) {
      this.setChildren(null)
    } else {
      val text: TextImpl = new TextImpl(stringValue.toString)
      text.setRawParent(this)
      this.setChildren(text)
    }
  }

  def setAttributeInfo(index: Int, attInfo: AttributeInfo): Unit = {
    var attMap: AttributeMapWithIdentity = prepareAttributesForUpdate()
    attMap = attMap.set(index, attInfo)
    this.setAttributes(attMap)
  }

  private def prepareAttributesForUpdate(): AttributeMapWithIdentity =
    attributes match {
      case withIdentity: AttributeMapWithIdentity =>
        withIdentity
      case _ =>
        val newAtts: AttributeMapWithIdentity = new AttributeMapWithIdentity(
          attributes.asList)
        this.setAttributes(newAtts)
        newAtts
    }

  override def addAttribute(nodeName: NodeName,
                            attType: SimpleType,
                            value: CharSequence,
                            properties: Int,
                            inheritNamespaces: Boolean): Unit = {
    var atts: AttributeMapWithIdentity = prepareAttributesForUpdate()
    atts = atts.add(
      new AttributeInfo(nodeName,
        attType,
        value.toString,
        Loc.NONE,
        ReceiverOption.NONE))
    this.setAttributes(atts)
    if (!nodeName.hasURI("")) {
      val binding: NamespaceBinding = nodeName.getNamespaceBinding
      val prefix: String = binding.prefix
      val uc: String = getURIForPrefix(prefix, useDefault = false)
      if (uc == null) {
        addNamespace(binding, inheritNamespaces)
      } else if (uc != binding.getURI) {
        throw new IllegalStateException(
          "Namespace binding of new name conflicts with existing namespace binding")
      }
    }
    if (ReceiverOption.contains(properties, ReceiverOption.IS_ID)) {
      val root: DocumentImpl = getPhysicalRoot
      if (root != null) {
        root.registerID(this, Whitespace.trim(value))
      }
    }
  }

  override def removeAttribute(attribute: NodeInfo): Unit = {

    if (! attribute.isInstanceOf[AttributeImpl])
      return

    val index: Int = attribute.asInstanceOf[AttributeImpl].getSiblingPosition
    val info: AttributeInfo = attributes.itemAt(index)
    var atts: AttributeMapWithIdentity = prepareAttributesForUpdate()
    atts = atts.remove(index)
    this.setAttributes(atts)
    if (index >= 0 && info.isId) {
      val root: DocumentImpl = getPhysicalRoot
      root.deregisterID(info.value)
    }
    attribute.asInstanceOf[AttributeImpl].setRawParent(null)
  }

  override def removeNamespace(prefix: String): Unit = {
    ju.Objects.requireNonNull(prefix)
    if (prefix == getPrefix) {
      throw new IllegalStateException(
        "Cannot remove binding of namespace prefix used on the element name")
    }
    for (att <- attributeMap.iterator.asScala if att.nodeName.getPrefix == prefix) {
      throw new IllegalStateException(
        "Cannot remove binding of namespace prefix used on an existing attribute name")
    }
    namespaceMap = namespaceMap.remove(prefix)
  }

  override def addNamespace(prefix: String, uri: String, inherit: Boolean): Unit = {
    val binding = new NamespaceBinding(prefix, uri)
    if (binding.getURI.isEmpty) {
      throw new IllegalArgumentException("Cannot add a namespace undeclaration")
    }
    val existing = namespaceMap.getURI(binding.prefix)
    if (existing != null) {
      if (existing != binding.getURI) {
        throw new IllegalArgumentException(
          "New namespace conflicts with existing namespace binding")
      }
    } else {
      val oldMap = namespaceMap
      namespaceMap = namespaceMap.put(binding.prefix, binding.getURI)
      if (inherit && (namespaceMap ne oldMap)) {
        for (child <- getChildren(NodeKindTest.ELEMENT)) {
          child.asInstanceOf[ElementImpl].inheritParentNamespaces(binding, oldMap, namespaceMap)
        }
      }
    }
  }

  private def inheritParentNamespaces(binding: NamespaceBinding, oldParentMap: NamespaceMap, newParentMap: NamespaceMap): Unit = {
    val oldMap = namespaceMap
    if (oldMap.getURIForPrefix(binding.prefix, useDefault = false) == null) {
      if (namespaceMap eq oldParentMap) {
        namespaceMap = newParentMap
      } else {
        namespaceMap = namespaceMap.put(binding.prefix, binding.getURI)
      }
      for (child <- getChildren(NodeKindTest.ELEMENT)) {
        child.asInstanceOf[ElementImpl].inheritParentNamespaces(binding, oldMap, namespaceMap)
      }
    }
  }

  override def removeTypeAnnotation(): Unit = {
    if (getSchemaType != Untyped.getInstance) {
      `type` = AnyType.getInstance
      getRawParent.removeTypeAnnotation()
    }
  }

  def setNamespaceMap(map: NamespaceMap): Unit = {
    namespaceMap = map
  }

  def getURIForPrefix(prefix: String, useDefault: Boolean): String =
    if (prefix.isEmpty) {
      if (useDefault) {
        namespaceMap.getDefaultNamespace
      } else {
        NamespaceConstant.NULL
      }
    } else {
      namespaceMap.getURI(prefix)
    }

  def iteratePrefixes: ju.Iterator[String] = namespaceMap.iteratePrefixes

  def isInScopeNamespace(uri: String): Boolean =
    namespaceMap.asScala.exists(_.getURI == uri)

  override def getDeclaredNamespaces(buffer: Array[NamespaceBinding]): Array[NamespaceBinding] = {
    val bindings: ju.List[NamespaceBinding] = new ju.ArrayList[NamespaceBinding]
    for (nb <- namespaceMap.asScala)
      bindings.add(nb)
    bindings.toArray(NamespaceBinding.EMPTY_ARRAY)
  }

  def fixupInsertedNamespaces(inherit: Boolean): Unit = {
    if (getRawParent.getNodeKind == Type.DOCUMENT) {
      return
    }
    val parent: ElementImpl = getRawParent.asInstanceOf[ElementImpl]
    val parentNamespaces: NamespaceMap = parent.namespaceMap
    if (inherit) {
      deepAddNamespaces(parentNamespaces)
    } else {}
  }

  private def deepAddNamespaces(inheritedNamespaces: NamespaceMap): Unit = {
    var lInheritedNamespaces: NamespaceMap = inheritedNamespaces
    var childNamespaces: NamespaceMap = namespaceMap
    for (binding <- lInheritedNamespaces.asScala) {
      if (childNamespaces.getURI(binding.prefix) == null) {
        childNamespaces =
          childNamespaces.put(binding.prefix, binding.getURI)
      } else {
        lInheritedNamespaces = lInheritedNamespaces.remove(binding.prefix)
      }
    }
    namespaceMap = childNamespaces
    for (child <- getChildren((node: NodeInfo) => node.isInstanceOf[ElementImpl])) {
      child.asInstanceOf[ElementImpl].deepAddNamespaces(inheritedNamespaces)
    }

  }

  def getChildren(filter: Predicate[NodeInfo]): Iterable[NodeInfo] =
    if (hasChildNodes) {
      val parent: NodeInfo = this
      (parent iterateAxis(AxisInfo.CHILD, nodeTest = filter)).asIterator.to(Iterable)
    } else {
      ju.Collections.emptyList[NodeInfo]().asScala
    }

  override def getAllNamespaces: NamespaceMap = namespaceMap

  override def getAttributeValue(uri: String, localName: String): String =
    if (attributeMap == null) null else attributeMap.getValue(uri, localName)

  override def isId: Boolean =
    try {
      val `type`: SchemaType = getSchemaType
      `type`.getFingerprint == StandardNames.XS_ID ||
        `type`.isIdType && NameChecker.isValidNCName(getStringValueCS)
    } catch {
      case e: MissingComponentException => false

    }

  override def isIdref: Boolean = isIdRefNode(this)

}