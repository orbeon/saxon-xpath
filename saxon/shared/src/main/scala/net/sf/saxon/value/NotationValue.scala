package net.sf.saxon.value

import net.sf.saxon.model.AtomicType
import net.sf.saxon.model.BuiltInAtomicType
import net.sf.saxon.om.{Item, NameChecker, SequenceTool, StructuredQName}
import net.sf.saxon.trans.XPathException


class NotationValue(prefix: String,
                    uri: String,
                    localName: String,
                    check: Boolean)
  extends QualifiedNameValue {

  var lPrefix: String = prefix

  var lUri = uri

  if (check && !NameChecker.isValidNCName(localName)) {
    val err = new XPathException(
      "Malformed local name in NOTATION: '" + localName + '\'')
    err.setErrorCode("FORG0001")
    throw err
  }

  lPrefix = if (lPrefix == null) "" else lPrefix

  lUri = if (lUri == null) "" else lUri

  if (check && uri.isEmpty && lPrefix.length != 0) {
    val err = new XPathException(
      "NOTATION has null namespace but non-empty lPrefix")
    err.setErrorCode("FOCA0002")
    throw err
  }

  qName = new StructuredQName(prefix, uri, localName)

  typeLabel = BuiltInAtomicType.NOTATION

  def this(prefix: String, uri: String, localName: String) = {
    // double check this
    this("", "", "", false)
    qName = new StructuredQName(prefix, uri, localName)
    typeLabel = BuiltInAtomicType.NOTATION
  }

  def this(prefix: String,
           uri: String,
           localName: String,
           typeLabel: AtomicType) = {
    this("", "", "", false)
    qName = new StructuredQName(prefix, uri, localName)
    this.typeLabel = typeLabel
  }

  def this(qName: StructuredQName, typeLabel: AtomicType) = {
    this("", "", "", false)
    if (qName == null) {
      throw new NullPointerException("qName")
    }
    if (typeLabel == null) {
      throw new NullPointerException("typeLabel")
    }
    this.qName = qName
    this.typeLabel = typeLabel
  }

  def copyAsSubType(typeLabel: AtomicType): AtomicValue = {
    val v: NotationValue =
      new NotationValue(getPrefix, getNamespaceURI, getLocalName)
    v.typeLabel = typeLabel
    v
  }

  def getPrimitiveType(): BuiltInAtomicType = BuiltInAtomicType.NOTATION

  override def equals(other: Any): Boolean = other match {
    case other: NotationValue => qName == other.qName
    case _ => false

  }

  def getSchemaComparable(): Comparable[AnyRef] = new NotationComparable().asInstanceOf

  private class NotationComparable extends Comparable[AnyRef] {

    def getNotationValue(): NotationValue = NotationValue.this

    def compareTo(o: AnyRef): Int =
      if (this == o) 0 else SequenceTool.INDETERMINATE_ORDERING

    override def equals(o: Any): Boolean =
      (o.isInstanceOf[NotationComparable] &&
        qName == o.asInstanceOf[NotationComparable].getNotationValue.qName)

    override def hashCode(): Int = qName.hashCode

  }

  override def toString: String = "NOTATION(" + getClarkName + ')'

}
