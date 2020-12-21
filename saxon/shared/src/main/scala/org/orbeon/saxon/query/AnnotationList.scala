package org.orbeon.saxon.query

import org.orbeon.saxon.lib.FunctionAnnotationHandler
import org.orbeon.saxon.om.StructuredQName

//import scala.collection.compat._
import scala.jdk.CollectionConverters._
import java.util._

import org.orbeon.saxon.utils.Configuration

object AnnotationList {

  var EMPTY: AnnotationList = new AnnotationList(Collections.emptyList())

  def singleton(ann: Annotation): AnnotationList =
    new AnnotationList(Collections.singletonList(ann))

}

class AnnotationList(var list: List[Annotation])
  extends java.lang.Iterable[Annotation] {

  def check(config: Configuration, where: String): Unit = {
    val map: Map[String, List[Annotation]] = groupByNamespace()
    for ((key, value) <- map.asScala) {
      val handler: FunctionAnnotationHandler =
        config.getFunctionAnnotationHandler(key)
      if (handler != null) {
        handler.check(new AnnotationList(value), where)
      }
    }
  }

  private def groupByNamespace(): Map[String, List[Annotation]] = {
    val result: Map[String, List[Annotation]] =
      new HashMap[String, List[Annotation]]()
    for (ann <- list.asScala) {
      val ns: String = ann.getAnnotationQName.getURI
      if (result.containsKey(ns)) {
        result.get(ns).add(ann)
      } else {
        val list: List[Annotation] = new ArrayList[Annotation]()
        list.add(ann)
        result.put(ns, list)
      }
    }
    result
  }

  def filterByNamespace(ns: String): AnnotationList = {
    val out: List[Annotation] = new ArrayList[Annotation]()
    for (ann <- list.asScala if ann.getAnnotationQName.hasURI(ns)) {
      out.add(ann)
    }
    new AnnotationList(out)
  }

  override def iterator: Iterator[Annotation] = list.iterator

  def isEmpty: Boolean = list.isEmpty

  def size(): Int = list.size

  def get(i: Int): Annotation = list.get(i)

  def includes(name: StructuredQName): Boolean =
    list.asScala.find(_.getAnnotationQName == name).map(_ => true).getOrElse(false)

  def includes(localName: String): Boolean =
    list.asScala
      .find(_.getAnnotationQName.getLocalPart == localName)
      .map(_ => true)
      .getOrElse(false)

  override def equals(other: Any): Boolean = other match {
    case other: AnnotationList => list == other.list
    case _ => false

  }

  override def hashCode: Int = list.hashCode

}
