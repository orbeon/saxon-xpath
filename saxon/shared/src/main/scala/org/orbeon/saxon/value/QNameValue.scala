package org.orbeon.saxon.value

import org.orbeon.saxon.functions.AccessorFn.Component._
import org.orbeon.saxon.model.AtomicType
import org.orbeon.saxon.model.BuiltInAtomicType
import org.orbeon.saxon.om.NameChecker
import org.orbeon.saxon.om.SequenceTool
import org.orbeon.saxon.om.StructuredQName
import org.orbeon.saxon.trans.XPathException


class QNameValue(prefix: String,
                 uri: String,
                 localName: String,
                 typeImpl: AtomicType) extends QualifiedNameValue {

  var lTypeImpl: AtomicType = typeImpl

  qName = new StructuredQName(prefix, uri, localName)

  if (lTypeImpl == null) {
    lTypeImpl = BuiltInAtomicType.QNAME
  }

  typeLabel = lTypeImpl

  def this(prefix: String, uri: String, localName: String) =
    this(prefix, uri, localName, BuiltInAtomicType.QNAME)

  def this(prefix: String,
           uri: String,
           localName: String,
           typeImpl: AtomicType,
           check: Boolean) = {

    this(prefix, uri, localName, typeImpl)

    var lPrefix: String = prefix
    var lUri: String = uri

    if (!NameChecker.isValidNCName(localName)) {
      val err = new XPathException(
        "Malformed local name in QName: '" + localName + '\'')
      err.setErrorCode("FORG0001")
      throw err
    }
    lPrefix = if (lPrefix == null) "" else lPrefix
    lUri = if ("" == lUri) null else lUri
    if (check && lUri == null && lPrefix.length != 0) {
      val err = new XPathException(
        "QName has null namespace but non-empty prefix")
      err.setErrorCode("FOCA0002")
      throw err
    }
    qName = new StructuredQName(lPrefix, lUri, localName)
    typeLabel = typeImpl
  }

  def this(qName: StructuredQName, typeLabel: AtomicType) = {
    this("", "", "", typeLabel)
    if (qName == null) {
      throw new NullPointerException("qName")
    }
    if (typeLabel == null) {
      throw new NullPointerException("typeLabel")
    }
    this.qName = qName
    this.typeLabel = typeLabel
  }

  def copyAsSubType(typeLabel: AtomicType): AtomicValue =
    new QNameValue(qName, typeLabel)

  def getPrimitiveType: BuiltInAtomicType = BuiltInAtomicType.QNAME

  override def getComponent(part: Component): AtomicValue = part match {
    case LOCALNAME => new StringValue(getLocalName, BuiltInAtomicType.NCNAME)
    case NAMESPACE => new AnyURIValue(getNamespaceURI)
    case PREFIX =>
      var prefix: String = getPrefix
      if (prefix.isEmpty) {
        null
      } else {
        new StringValue(prefix, BuiltInAtomicType.NCNAME)
      }
    case _ =>
      throw new UnsupportedOperationException(
        "Component of QName must be URI, Local Name, or Prefix")

  }

  override def equals(other: Any): Boolean = other match {
    case other: QNameValue => qName == other.qName
    case _ => false

  }

  def getSchemaComparable(): Comparable[AnyRef] = new QNameComparable()

  class QNameComparable extends Comparable[AnyRef] {

    def getQNameValue: QNameValue = QNameValue.this

    def compareTo(o: AnyRef): Int =
      if (equals(0)) 0 else SequenceTool.INDETERMINATE_ORDERING

    override def equals(o: Any): Boolean =
      (o.isInstanceOf[QNameComparable] &&
        qName == o.asInstanceOf[QNameComparable].getQNameValue.qName)

    override def hashCode: Int = qName.hashCode

  }

}