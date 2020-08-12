package net.sf.saxon.s9api

import net.sf.saxon.value.{ExternalObject, ObjectValue}

class XdmExternalObject extends XdmItem {

  def this(value: Any) {
    this()
    if (value.isInstanceOf[ObjectValue[Any]]) new ObjectValue[Any](value)
    else new ObjectValue[Any]()
  }


  def getExternalObject(): Any = getUnderlyingValue.asInstanceOf[ExternalObject[Any]].getObject

  override def toString(): String = getExternalObject.toString

  override def equals(other: Any): Boolean = other match {
    case other: XdmExternalObject =>
      getUnderlyingValue == other.getUnderlyingValue
    case _ => false

  }

  override def hashCode(): Int = getUnderlyingValue.hashCode

}
