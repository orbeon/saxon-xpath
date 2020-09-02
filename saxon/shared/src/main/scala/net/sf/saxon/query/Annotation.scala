package net.sf.saxon.query

import net.sf.saxon.lib.NamespaceConstant

import net.sf.saxon.om.StructuredQName

import net.sf.saxon.value.AtomicValue

import net.sf.saxon.value.NumericValue

import net.sf.saxon.value.StringValue

import java.util.ArrayList

import java.util.List

import Annotation._

import scala.beans.{BeanProperty, BooleanBeanProperty}

object Annotation {

  val UPDATING: StructuredQName =
    new StructuredQName("", NamespaceConstant.XQUERY, "updating")

  val SIMPLE: StructuredQName =
    new StructuredQName("", NamespaceConstant.XQUERY, "simple")

  val PRIVATE: StructuredQName =
    new StructuredQName("", NamespaceConstant.XQUERY, "private")

  val PUBLIC: StructuredQName =
    new StructuredQName("", NamespaceConstant.XQUERY, "public")

  private def annotationParamEqual(a: AtomicValue, b: AtomicValue): Boolean =
    if (a.isInstanceOf[StringValue] && b.isInstanceOf[StringValue]) {
      a.getStringValue == b.getStringValue
    } else if (a.isInstanceOf[NumericValue] && b.isInstanceOf[NumericValue]) {
      a.asInstanceOf[NumericValue]
        .getDoubleValue == b.asInstanceOf[NumericValue].getDoubleValue
    } else {
      false
    }

}

class Annotation(name: StructuredQName) {

  private var qName: StructuredQName = name

  @BeanProperty
  var annotationParameters: List[AtomicValue] = new ArrayList()

  def getAnnotationQName: StructuredQName = qName

  def addAnnotationParameter(value: AtomicValue): Unit = {
    if (annotationParameters == null) {
      annotationParameters = new ArrayList()
    }
    annotationParameters.add(value)
  }

  override def equals(other: Any): Boolean = {
    if (!(other.isInstanceOf[Annotation] && qName == other
      .asInstanceOf[Annotation]
      .qName &&
      getAnnotationParameters.size ==
        other.asInstanceOf[Annotation].getAnnotationParameters.size)) {
      return false
    }
    for (i <- 0 until annotationParameters.size if !annotationParamEqual(
      annotationParameters.get(i),
      other.asInstanceOf[Annotation].annotationParameters.get(i))) {
      return false
    }
    true
  }

  override def hashCode: Int = qName.hashCode ^ annotationParameters.hashCode

}
