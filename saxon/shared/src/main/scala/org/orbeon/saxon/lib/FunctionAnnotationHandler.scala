package org.orbeon.saxon.lib

import org.orbeon.saxon.model.Affinity.Affinity
import org.orbeon.saxon.query.{Annotation, AnnotationList}

trait FunctionAnnotationHandler {

  def getAssertionNamespace: String

  def check(annotations: AnnotationList, construct: String): Unit

  def satisfiesAssertion(assertion: Annotation,
                         annotationList: AnnotationList): Boolean

  def relationship(firstList: AnnotationList,
                   secondList: AnnotationList): Affinity

}