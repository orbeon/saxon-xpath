
package org.orbeon.saxon.lib

import java.util
import java.util.{Collections, HashSet, Set}

import org.orbeon.saxon.lib.XQueryFunctionAnnotationHandler._
import org.orbeon.saxon.model.Affinity
import org.orbeon.saxon.model.Affinity.Affinity
import org.orbeon.saxon.om.StructuredQName
import org.orbeon.saxon.query.{Annotation, AnnotationList}
import org.orbeon.saxon.trans.XPathException

//import scala.collection.compat._
import scala.jdk.CollectionConverters._

object XQueryFunctionAnnotationHandler {

  class DisallowedCombination(var one: StructuredQName,
                              var two: StructuredQName,
                              var errorCode: String,
                              whereVar: String*) {
    var where: util.Set[String] = new HashSet[String](whereVar.length)

    Collections.addAll(where, whereVar: _*)

  }

  private var blackList: Array[DisallowedCombination] = Array(
    new DisallowedCombination(Annotation.SIMPLE, null, "XUST0032", "DV"),
    new DisallowedCombination(Annotation.UPDATING, null, "XUST0032", "DV"),
    new DisallowedCombination(Annotation.PUBLIC, null, "XQST0125", "IF"),
    new DisallowedCombination(Annotation.PRIVATE, null, "XQST0125", "IF"),
    new DisallowedCombination(Annotation.PRIVATE,
      Annotation.PRIVATE,
      "XQST0106",
      "DF"),
    new DisallowedCombination(Annotation.PRIVATE,
      Annotation.PUBLIC,
      "XQST0106",
      "DF"),
    new DisallowedCombination(Annotation.PUBLIC,
      Annotation.PUBLIC,
      "XQST0106",
      "DF"),
    new DisallowedCombination(Annotation.PUBLIC,
      Annotation.PRIVATE,
      "XQST0106",
      "DF"),
    new DisallowedCombination(Annotation.PRIVATE,
      Annotation.PRIVATE,
      "XQST0116",
      "DV"),
    new DisallowedCombination(Annotation.PRIVATE,
      Annotation.PUBLIC,
      "XQST0116",
      "DV"),
    new DisallowedCombination(Annotation.PUBLIC,
      Annotation.PUBLIC,
      "XQST0116",
      "DV"),
    new DisallowedCombination(Annotation.PUBLIC,
      Annotation.PRIVATE,
      "XQST0116",
      "DV"),
    new DisallowedCombination(Annotation.UPDATING,
      Annotation.UPDATING,
      "XUST0033",
      "DF",
      "IF"),
    new DisallowedCombination(Annotation.UPDATING,
      Annotation.SIMPLE,
      "XUST0033",
      "DF",
      "IF"),
    new DisallowedCombination(Annotation.SIMPLE,
      Annotation.SIMPLE,
      "XUST0033",
      "DF",
      "IF"),
    new DisallowedCombination(Annotation.SIMPLE,
      Annotation.UPDATING,
      "XUST0033",
      "DF",
      "IF")
  )

}

class XQueryFunctionAnnotationHandler extends FunctionAnnotationHandler {

  override def check(annotations: AnnotationList, construct: String): Unit = {
    for (i <- 0 until annotations.size) {
      val ann: Annotation = annotations.get(i)
      for (dc <- blackList if dc.one == ann.getAnnotationQName && dc.where
        .contains(construct)) {
        if (dc.two == null) {
          throw new XPathException(
            "Annotation %" + ann.getAnnotationQName.getLocalPart +
              " is not allowed here",
            dc.errorCode)
        } else {
          for (j <- 0 until i) {
            val other: Annotation = annotations.get(j)
            if (dc.two == other.getAnnotationQName) {
              if (dc.two == ann.getAnnotationQName) {
                throw new XPathException(
                  "Annotation %" + ann.getAnnotationQName.getLocalPart +
                    " cannot appear more than once",
                  dc.errorCode)
              } else {
                throw new XPathException(
                  "Annotations %" + ann.getAnnotationQName.getLocalPart +
                    " and " +
                    other.getAnnotationQName.getLocalPart +
                    " cannot appear together",
                  dc.errorCode)
              }
            }
          }
        }
      }
    }
  }

  def getAssertionNamespace(): String = "http://www.w3.org/2012/xquery"

  def satisfiesAssertion(assertion: Annotation, annotationList: AnnotationList)
  : Boolean = // annotation assertions are not defined for this namespace (surprisingly)
    true

  def relationship(firstList: AnnotationList,
                   secondList: AnnotationList): Affinity = Affinity.OVERLAPS

}


