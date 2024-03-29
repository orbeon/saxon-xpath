package org.orbeon.saxon.dom

import org.orbeon.saxon.lib.NamespaceConstant

import org.orbeon.saxon.model.BuiltInAtomicType

import org.orbeon.saxon.model.SchemaType

import org.orbeon.saxon.model.Type

import org.orbeon.saxon.model.Untyped

import org.orbeon.saxon.om.AxisInfo

import org.orbeon.saxon.om.NamePool

import org.orbeon.saxon.om.NodeInfo

import org.orbeon.saxon.pattern.NameTest

import org.orbeon.saxon.tree.iter.AxisIterator

import org.w3c.dom._

import NodeOverNodeInfo._

class ElementOverNodeInfo extends NodeOverNodeInfo with Element {

  private var attributeMap: DOMAttributeMap = null

  def getTagName(): String = node.getDisplayName

  def getElementsByTagName(name: String): NodeList =
    DocumentOverNodeInfo.getElementsByTagName(node, name)

  def getElementsByTagNameNS(namespaceURI: String,
                             localName: String): NodeList =
    DocumentOverNodeInfo.getElementsByTagNameNS(node, namespaceURI, localName)

  override def getAttributes: NamedNodeMap = {
    if (attributeMap == null) {
      attributeMap = new DOMAttributeMap(node)
    }
    attributeMap
  }

  def getAttribute(name: String): String = {
    if (name.startsWith("xmlns")) {
      val node: Node = getAttributes.getNamedItem(name)
      if (node == null) return "" else return node.getNodeValue
    }
    val atts: AxisIterator = node.iterateAxis(AxisInfo.ATTRIBUTE)
    while (true) {
      val att: NodeInfo = atts.next()
      if (att == null) {
       return ""
      }
      if (att.getDisplayName == name) {
        val `val`: String = att.getStringValue
        if (`val` == null) {
          return ""
        }
        return `val`
      }
    }
    ""
  }

  def getAttributeNode(name: String): Attr = {
    val atts: AxisIterator = node.iterateAxis(AxisInfo.ATTRIBUTE)
    while (true) {
      val att: NodeInfo = atts.next()
      if (att == null) return null
      if (att.getDisplayName == name) {
        return wrap(att).asInstanceOf[AttrOverNodeInfo]
      }
    }
    null
  }

  def setAttributeNode(newAttr: Attr): Attr = {
    disallowUpdate()
    null
  }

  def removeAttribute(oldAttr: String): Unit = {
    disallowUpdate()
  }

  def removeAttributeNode(oldAttr: Attr): Attr = {
    disallowUpdate()
    null
  }

  def getAttributeNS(namespaceURI: String, localName: String): String = {
    if (NamespaceConstant.XMLNS == namespaceURI) {
      val node: Node = getAttributes.getNamedItemNS(namespaceURI, localName)
      if (node == null) return "" else return node.getNodeValue
    }
    val uri: String = if (namespaceURI == null) "" else namespaceURI
    val `val`: String = node.getAttributeValue(uri, localName)
    if (`val` == null) {
      return ""
    }
    `val`
  }

  def setAttribute(name: String, value: String): Unit = {
    disallowUpdate()
  }

  def setAttributeNS(namespaceURI: String,
                     qualifiedName: String,
                     value: String): Unit = {
    disallowUpdate()
  }

  def removeAttributeNS(namespaceURI: String, localName: String): Unit = {
    disallowUpdate()
  }

  def getAttributeNodeNS(namespaceURI: String, localName: String): Attr = {
    val pool: NamePool = node.getConfiguration.getNamePool
    val test: NameTest =
      new NameTest(Type.ATTRIBUTE, namespaceURI, localName, pool)
    val atts: AxisIterator = node.iterateAxis(AxisInfo.ATTRIBUTE, test)
    wrap(atts.next()).asInstanceOf[Attr]
  }

  def setAttributeNodeNS(newAttr: Attr): Attr = {
    disallowUpdate()
    null
  }

  def hasAttribute(name: String): Boolean = {
    if (name.startsWith("xmlns")) {
      val node: Node = getAttributes.getNamedItem(name)
      return node != null
    }
    val atts: AxisIterator = node.iterateAxis(AxisInfo.ATTRIBUTE)
    while (true) {
      val att: NodeInfo = atts.next()
      if (att == null) {
       return  false
      }
      if (att.getDisplayName == name) {
        return true
      }
    }
    false
  }

  def hasAttributeNS(namespaceURI: String, localName: String): Boolean = {
    if (NamespaceConstant.XMLNS == namespaceURI) {
      val node: Node = getAttributes.getNamedItemNS(namespaceURI, localName)
      return node != null
    }
    val uri: String = if (namespaceURI == null) "" else namespaceURI
    node.getAttributeValue(uri, localName) != null
  }

  def setIdAttribute(name: String, isId: Boolean): Unit = {
    disallowUpdate()
  }

  def setIdAttributeNS(namespaceURI: String,
                       localName: String,
                       isId: Boolean): Unit = {
    disallowUpdate()
  }

  def setIdAttributeNode(idAttr: Attr, isId: Boolean): Unit = {
    disallowUpdate()
  }

  def getSchemaTypeInfo(): TypeInfo = {
    val `type`: SchemaType = node.getSchemaType
    if (`type` == null || Untyped.getInstance == `type` || BuiltInAtomicType.UNTYPED_ATOMIC == `type`) {
      return null
    }
    new TypeInfoImpl(node.getConfiguration, `type`)
  }

}
