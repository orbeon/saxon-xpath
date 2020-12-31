package org.orbeon.saxon.model

import org.orbeon.saxon.utils.Configuration
import org.orbeon.saxon.lib.FunctionAnnotationHandler
import org.orbeon.saxon.om.Function
import org.orbeon.saxon.om.Item
import org.orbeon.saxon.query.Annotation
import org.orbeon.saxon.query.AnnotationList
import AnyFunctionTypeWithAssertions._
import org.orbeon.saxon.utils.Configuration
//import scala.collection.compat._
import scala.jdk.CollectionConverters._

object AnyFunctionTypeWithAssertions {

  private def checkAnnotationAssertions(assertions: AnnotationList,
                                        item: Function,
                                        config: Configuration): Boolean = {
    val annotations: AnnotationList = item.getAnnotations
    for (ann <- assertions.asScala) {
      val handler: FunctionAnnotationHandler =
        config.getFunctionAnnotationHandler(ann.getAnnotationQName.getURI)
      if (handler != null) {
        val ok: Boolean = handler.satisfiesAssertion(ann, annotations)
        if (!ok) {
         return false
        }
      }
    }
    true
  }
}

class AnyFunctionTypeWithAssertions(private var assertions: AnnotationList,
                                    private var config: Configuration) extends AnyFunctionType {

  override def getAnnotationAssertions: AnnotationList = assertions

  override def matches(item: Item, th: TypeHierarchy): Boolean =
    item.isInstanceOf[Function] &&
      checkAnnotationAssertions(assertions,
        item.asInstanceOf[Function],
        th.getConfiguration)

}
