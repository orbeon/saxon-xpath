package net.sf.saxon.lib

import net.sf.saxon.model.Affinity.Affinity
import net.sf.saxon.query.{Annotation, AnnotationList}

trait FunctionAnnotationHandler {

  def getAssertionNamespace(): String

  def check(annotations: AnnotationList, construct: String): Unit

  def satisfiesAssertion(assertion: Annotation,
                         annotationList: AnnotationList): Boolean

  def relationship(firstList: AnnotationList,
                   secondList: AnnotationList): Affinity

}