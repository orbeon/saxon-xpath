package net.sf.saxon.model

import net.sf.saxon.utils.Configuration
import net.sf.saxon.lib.FunctionAnnotationHandler
import net.sf.saxon.om.Function
import net.sf.saxon.om.Item
import net.sf.saxon.query.Annotation
import net.sf.saxon.query.AnnotationList
import AnyFunctionTypeWithAssertions._
import net.sf.saxon.utils.Configuration
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
          false
        }
      }
    }
    true
  }
}

class AnyFunctionTypeWithAssertions(private var assertions: AnnotationList,
                                    private var config: Configuration) extends AnyFunctionType {

  override def getAnnotationAssertions(): AnnotationList = assertions

  override def matches(item: Item, th: TypeHierarchy): Boolean =
    item.isInstanceOf[Function] &&
      checkAnnotationAssertions(assertions,
        item.asInstanceOf[Function],
        th.getConfiguration)

}
