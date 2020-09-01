package net.sf.saxon.value

import java.util.Objects

import net.sf.saxon.model.{ItemType, TypeHierarchy}
import net.sf.saxon.om.Genre
import net.sf.saxon.om.Genre.Genre
import net.sf.saxon.trans.Err


object ObjectValue {

  def displayTypeName(value: AnyRef): String =
    "java-type:" + value.getClass.getName

}

class ObjectValue[T](`object`: T) extends ExternalObject[T] {

  private val value: T =
    Objects.requireNonNull(`object`, "External object cannot wrap a Java null")

  override def getGenre(): Genre = Genre.EXTERNAL

  def getStringValue(): String = value.toString

  def getStringValueCS(): CharSequence = value.toString

  def atomize(): StringValue = new StringValue(getStringValue)

  def getItemType(th: TypeHierarchy): ItemType =
    th.getConfiguration.getJavaExternalObjectType(value.getClass)

  override def effectiveBooleanValue(): Boolean = true

  override def getObject(): T = value

  override def equals(other: Any): Boolean = other match {
    case other: ObjectValue[_] => {
      val o: Any = other.value
      value == o
    }
    case _ => false

  }

  override def hashCode(): Int = value.hashCode

  override def toShortString(): String = {
    val v: String = value.toString
    if (v.startsWith(value.getClass.getName)) {
      v
    } else {
      "(" + value.getClass.getSimpleName + ")" + Err.truncate30(value.toString)
    }
  }

}