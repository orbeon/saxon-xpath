package net.sf.saxon.tree.linked

import net.sf.saxon.event.CopyInformee
import net.sf.saxon.event.Receiver
import net.sf.saxon.event.ReceiverOption
import net.sf.saxon.expr.parser.Loc
import net.sf.saxon.lib.NamespaceConstant
import net.sf.saxon.model._
import net.sf.saxon.om._
import net.sf.saxon.pattern.NodeKindTest
import net.sf.saxon.s9api.Location
import net.sf.saxon.trans.XPathException
import net.sf.saxon.tree.iter.AxisIterator
import net.sf.saxon.tree.util.FastStringBuffer
import net.sf.saxon.tree.util.Navigator
import net.sf.saxon.value.AtomicValue
import net.sf.saxon.value.Whitespace
import java.util.{ArrayList, Collections, Iterator, List, Objects}
import java.util.function.Predicate

import net.sf.saxon.s9api.streams.Steps.child
import net.sf.saxon.tree.linked.ElementImpl.isIdRefNode

import scala.beans.{BeanProperty, BooleanBeanProperty}
import scala.jdk.CollectionConverters._
import scala.util.control.Breaks._

object ElementImpl {

  def isIdRefNode(node: NodeImpl): Boolean = {
    val `type`: SchemaType = node.getSchemaType
    try if (`type`.isIdRefType) {
      if (`type` == BuiltInAtomicType.IDREF || `type` == BuiltInListType.IDREFS) {
        return true
      }
      try for (av <- node.atomize().asScala if av.getItemType.isIdRefType) {
        true
      } catch {
        case _: XPathException =>

      }
    } catch {
      case e: MissingComponentException => return false

    }
    false
  }

}

class ElementImpl extends ParentNodeImpl with NamespaceResolver {

  @BeanProperty
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

  override def getRoot(): NodeInfo = {
    val up: ParentNodeImpl = getRawParent
    if (up == null ||
      (up.isInstanceOf[DocumentImpl] && up
        .asInstanceOf[DocumentImpl]
        .isImaginary)) {
      this
    } else {
      up.getRoot
    }
  }

  override def getSystemId(): String = {
    val root: DocumentImpl = getPhysicalRoot
    if (root == null) null else root.getSystemId(getRawSequenceNumber)
  }

  override def getBaseURI(): String =
    Navigator.getBaseURI(
      this.asInstanceOf[NodeInfo],
      (n: NodeInfo) => getPhysicalRoot.isTopWithinEntity(n.asInstanceOf[ElementImpl]))

  override def isNilled(): Boolean = getPhysicalRoot.isNilledElement(this)

  override def setTypeAnnotation(`type`: SchemaType): Unit = {
    this.`type` = `type`
  }

  def setNilled(): Unit = {
    getPhysicalRoot.addNilledElement(this)
  }

  override def getSchemaType(): SchemaType = `type`

  override def getLineNumber(): Int = {
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

  def getNodeKind(): Int = Type.ELEMENT

  override def attributes(): AttributeMap = attributeMap

  def iterateAttributes(test: Predicate[_ >: NodeInfo]): AxisIterator =
    if (attributeMap.isInstanceOf[AttributeMapWithIdentity]) {
      new Navigator.AxisFilter(attributeMap
        .asInstanceOf[AttributeMapWithIdentity]
        .iterateAttributes(this),
        test)
    } else {
      new AttributeAxisIterator(this, test)
    }

  override def copy(out: Receiver, copyOptions: Int, location: Location): Unit = {
    var lLocation = location
    val typeCode: SchemaType =
      if (CopyOptions.includes(copyOptions, CopyOptions.TYPE_ANNOTATIONS))
        getSchemaType
      else Untyped.getInstance
    val informee: CopyInformee[AnyRef] = out.getPipelineConfiguration
      .getComponent(classOf[CopyInformee[AnyRef]].getName)
      .asInstanceOf[CopyInformee[AnyRef]]
    if (informee != null) {
      val o: AnyRef = informee.notifyElementNode(this)
      if (o.isInstanceOf[Location]) {
        lLocation = o.asInstanceOf[Location]
      }
    }
    val ns: NamespaceMap =
      if (CopyOptions.includes(copyOptions, CopyOptions.ALL_NAMESPACES))
        getAllNamespaces
      else NamespaceMap.emptyMap
    val atts: List[AttributeInfo] =
      new ArrayList[AttributeInfo](attributes().size)
    for (att <- attributes()) {
      atts.add(
        new AttributeInfo(att.getNodeName,
          BuiltInAtomicType.UNTYPED_ATOMIC,
          att.getValue,
          att.getLocation,
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

  override def delete(): Unit = {
    val root: DocumentImpl = getPhysicalRoot
    super.delete()
    if (root != null) {
      val iter: AxisIterator =
        iterateAxis(AxisInfo.DESCENDANT_OR_SELF, NodeKindTest.ELEMENT)
      breakable {
        while (true) {
          val n: ElementImpl = iter.next().asInstanceOf[ElementImpl]
          for (att <- attributeMap if att.isId) {
            root.deregisterID(att.getValue)
          }
          if (n == null) {
            break()
          }
          root.deIndex(n)
        }
      }
    }
  }

  override def rename(newName: NodeName): Unit = {
    val prefix: String = newName.getPrefix
    val uri: String = newName.getURI
    val ns: NamespaceBinding = new NamespaceBinding(prefix, uri)
    var uc: String = getURIForPrefix(prefix, useDefault = true)
    if (uc == null) {
      uc = ""
    }
    if (uc != uri) {
      if (uc.isEmpty) {
        addNamespace(ns)
      } else {
        throw new IllegalArgumentException(
          "Namespace binding of new name conflicts with existing namespace binding")
      }
    }
    nodeN = newName
  }

  override def addNamespace(binding: NamespaceBinding): Unit = {
    if (binding.getURI.isEmpty) {
      throw new IllegalArgumentException(
        "Cannot add a namespace undeclaration")
    }
    val existing: String = namespaceMap.getURI(binding.getPrefix)
    if (existing != null) {
      if (existing != binding.getURI) {
        throw new IllegalArgumentException(
          "New namespace conflicts with existing namespace binding")
      }
    } else {
      namespaceMap = namespaceMap.put(binding.getPrefix, binding.getURI)
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
    if (attributes().isInstanceOf[AttributeMapWithIdentity]) {
      attributes().asInstanceOf[AttributeMapWithIdentity]
    } else {
      val newAtts: AttributeMapWithIdentity = new AttributeMapWithIdentity(
        attributes().asList())
      this.setAttributes(newAtts)
      newAtts
    }

  override def addAttribute(nodeName: NodeName,
                            attType: SimpleType,
                            value: CharSequence,
                            properties: Int): Unit = {
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
      val prefix: String = binding.getPrefix
      val uc: String = getURIForPrefix(prefix, useDefault = false)
      if (uc == null) {
        addNamespace(binding)
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
    if (!(attribute.isInstanceOf[AttributeImpl])) {
      return
    }
    val index: Int = attribute.asInstanceOf[AttributeImpl].getSiblingPosition
    val info: AttributeInfo = attributes().itemAt(index)
    var atts: AttributeMapWithIdentity = prepareAttributesForUpdate()
    atts = atts.remove(index)
    this.setAttributes(atts)
    if (index >= 0 && info.isId) {
      val root: DocumentImpl = getPhysicalRoot
      root.deregisterID(info.getValue)
    }
    attribute.asInstanceOf[AttributeImpl].setRawParent(null)
  }

  override def removeNamespace(prefix: String): Unit = {
    Objects.requireNonNull(prefix)
    if (prefix == getPrefix) {
      throw new IllegalStateException(
        "Cannot remove binding of namespace prefix used on the element name")
    }
    for (att <- attributeMap if att.getNodeName.getPrefix == prefix) {
      throw new IllegalStateException(
        "Cannot remove binding of namespace prefix used on an existing attribute name")
    }
    namespaceMap = namespaceMap.remove(prefix)
  }

  override def addNamespace(prefix: String, uri: String): Unit = {
    val existingURI: String = namespaceMap.getURI(prefix)
    if (existingURI == null) {
      namespaceMap = namespaceMap.put(prefix, uri)
    } else if (existingURI != uri) {
      throw new IllegalStateException(
        "New namespace binding conflicts with existing namespace binding")
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

  def iteratePrefixes(): Iterator[String] = namespaceMap.iteratePrefixes()

  def isInScopeNamespace(uri: String): Boolean =
    namespaceMap.asScala.find(_.getURI == uri).map(_ => true).getOrElse(false)

  override def getDeclaredNamespaces(
                                      buffer: Array[NamespaceBinding]): Array[NamespaceBinding] = {
    val bindings: List[NamespaceBinding] = new ArrayList[NamespaceBinding]()
    for (nb <- namespaceMap.asScala) {
      bindings.add(nb)
    }
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
      if (childNamespaces.getURI(binding.getPrefix) == null) {
        childNamespaces =
          childNamespaces.put(binding.getPrefix, binding.getURI)
      } else {
        lInheritedNamespaces = lInheritedNamespaces.remove(binding.getPrefix)
      }
    }
    namespaceMap = childNamespaces
    for (child <- getChildren((cls: Class[ElementImpl]) => cls.isInstance(ElementImpl))) {
      child.asInstanceOf[ElementImpl].deepAddNamespaces(inheritedNamespaces)
    }

  }

  def getChildren(filter: Predicate[_ <: NodeInfo]): Iterable[_ <: NodeInfo] =
    if (hasChildNodes) {
      val parent: NodeInfo = this
      (parent iterateAxis(AxisInfo.CHILD, nodeTest = filter.asInstanceOf[Predicate[_ >: NodeInfo]])).asIterator() .iterator.to(Iterable)
    } else {
      Collections.emptyList().asScala
    }

  override def getAllNamespaces(): NamespaceMap = namespaceMap

  override def getAttributeValue(uri: String, localName: String): String =
    if (attributeMap == null) null else attributeMap.getValue(uri, localName)

  override def isId(): Boolean =
    try {
      val `type`: SchemaType = getSchemaType
      `type`.getFingerprint == StandardNames.XS_ID ||
        `type`.isIdType && NameChecker.isValidNCName(getStringValueCS)
    } catch {
      case e: MissingComponentException => false

    }

  override def isIdref(): Boolean = isIdRefNode(this)

}