package net.sf.saxon.value

import net.sf.saxon.model.ItemType
import net.sf.saxon.model.TypeHierarchy
import net.sf.saxon.om.Genre
import net.sf.saxon.trans.Err
import java.util.Objects

import ObjectValue._
import net.sf.saxon.om.Genre.Genre


object ObjectValue {

  def displayTypeName(value: AnyRef): String =
    "java-type:" + value.getClass.getName

}

class ObjectValue[T](`object`: T) extends ExternalObject[T] {

  private var value: T =
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