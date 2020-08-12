package net.sf.saxon.s9api.streams

import net.sf.saxon.s9api._
import java.util.function.Predicate
import java.util.regex.Pattern

import net.sf.saxon.s9apir.XdmFunctionItem


object Predicates {

  def isNode: Predicate[XdmItem] = (item: XdmItem) => item.isInstanceOf[XdmNode]


  def isElement: Predicate[XdmNode] = nodeKindPredicate(XdmNodeKind.ELEMENT)


  def isAttribute: Predicate[XdmNode] = nodeKindPredicate(XdmNodeKind.ATTRIBUTE)


  def isText: Predicate[XdmNode] = nodeKindPredicate(XdmNodeKind.TEXT)


  def isComment: Predicate[XdmNode] = nodeKindPredicate(XdmNodeKind.COMMENT)


  def isProcessingInstruction: Predicate[XdmNode] = nodeKindPredicate(XdmNodeKind.PROCESSING_INSTRUCTION)


  def isDocument: Predicate[XdmNode] = nodeKindPredicate(XdmNodeKind.DOCUMENT)


  def isNamespace: Predicate[XdmNode] = nodeKindPredicate(XdmNodeKind.NAMESPACE)


  def isAtomic: Predicate[XdmItem] = (item: XdmItem) => item.isInstanceOf[XdmAtomicValue]


  def isFunction: Predicate[XdmItem] = (item: XdmItem) => item.isInstanceOf[XdmFunctionItem]


  def isMap: Predicate[XdmItem] = (item: XdmItem) => item.isInstanceOf[XdmMap]


  def isArray: Predicate[XdmItem] = (item: XdmItem) => item.isInstanceOf[XdmArray]


  def empty[T <: XdmItem](step: Step[T]): Predicate[XdmItem] = (item: XdmItem) => !step.apply(item).findFirst.isPresent


  def not[T](condition: Predicate[T]): Predicate[T] = condition.negate


  def exists[T <: XdmItem](step: Step[T]): Predicate[XdmItem] = (item: XdmItem) => step.apply(item).findFirst.isPresent


  def hasName(uri: String, localName: String): Predicate[XdmNode] = (item: XdmNode) => {
    def foo(item: XdmNode) = {
      val name = item.getNodeName
      name != null && name.getLocalName == localName && name.getNamespaceURI == uri
    }

    foo(item)
  }


  def hasLocalName(localName: String): Predicate[XdmNode] = (item: XdmNode) => {
    def foo(item: XdmNode) = {
      val name = item.getNodeName
      name != null && name.getLocalName == localName
    }

    foo(item)
  }


  def hasNamespace(uri: String): Predicate[XdmNode] = (item: XdmNode) => {
    def foo(item: XdmNode) = {
      val name = item.getNodeName
      name != null && name.getNamespaceURI == uri
    }

    foo(item)
  }


  def hasAttribute(local: String): Predicate[XdmNode] = (item: XdmNode) => item.attribute(local) != null


  def attributeEq(local: String, value: String): Predicate[XdmNode] = (item: XdmNode) => value == item.attribute(local)


  def hasType(itmType: ItemType): Predicate[XdmItem] = (xdmItem: XdmItem) => itmType.matches(xdmItem)


  def some[T <: XdmItem](step: Step[T], condition: Predicate[_ >: T]): Predicate[XdmItem] = (item: XdmItem) => step.apply(item).anyMatch(condition)


  def every[T <: XdmItem](step: Step[T], condition: Predicate[_ >: XdmItem]): Predicate[XdmItem] = (item: XdmItem) => step.apply(item).allMatch(condition)


  def eq(value: XdmAtomicValue): Predicate[XdmAtomicValue] = {
    val k2 = value.getUnderlyingValue.asMapKey
    (item: XdmAtomicValue) => item.getUnderlyingValue.asMapKey == k2
  }


  def eq(value: String): Predicate[XdmItem] = (item: XdmItem) => item.getStringValue == value


  def matchesRegex(regex: String): Predicate[XdmItem] = {
    val re = Pattern.compile(regex)
    (item: XdmItem) => re.matcher(item.getStringValue).find
  }


  def eq[T <: XdmItem](step: Step[T], value: String): Predicate[XdmItem] = some(step, eq(value))

  private def nodeKindPredicate(kind: XdmNodeKind.XdmNodeKind): Predicate[XdmNode] =
    (item: XdmItem) => item.isInstanceOf[XdmNode] && (item.asInstanceOf[XdmNode].getNodeKind eq kind)
}