package net.sf.saxon.s9api

import net.sf.saxon.value.{ExternalObject, ObjectValue}

class XdmExternalObject extends XdmItem {

  //ORBEON: FIXME
  def this(value: Any) {
    this()
    if (value.isInstanceOf[ObjectValue[_]])
      new ObjectValue[Any](value)
    else
      new ObjectValue[Any]
  }

  def getExternalObject: Any = getUnderlyingValue.asInstanceOf[ExternalObject[_]].getObject

  override def toString: String = getExternalObject.toString

  override def equals(other: Any): Boolean = other match {
    case other: XdmExternalObject => getUnderlyingValue == other.getUnderlyingValue
    case _ => false
  }

  override def hashCode(): Int = getUnderlyingValue.hashCode

}
