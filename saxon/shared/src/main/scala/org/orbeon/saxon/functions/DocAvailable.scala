package org.orbeon.saxon.functions

import org.orbeon.saxon.expr.PackageData

import org.orbeon.saxon.expr.XPathContext

import org.orbeon.saxon.om._

import org.orbeon.saxon.trans.XPathException

import org.orbeon.saxon.value.AtomicValue

import org.orbeon.saxon.value.BooleanValue

class DocAvailable extends SystemFunction {

  private def isDocAvailable(hrefVal: AtomicValue,
                             context: XPathContext): Boolean = {
    if (hrefVal == null) {
      return false
    }
    val href: String = hrefVal.getStringValue
    docAvailable(href, context)
  }

  def call(context: XPathContext, arguments: Array[Sequence]): BooleanValue =
    BooleanValue.get(
      isDocAvailable(arguments(0).head.asInstanceOf[AtomicValue], context))

  def docAvailable(href: String, context: XPathContext): Boolean =
    try {
      val packageData: PackageData = getRetainedStaticContext.getPackageData
      val documentKey: DocumentURI = DocumentFn.computeDocumentKey(
        href,
        getStaticBaseUriString,
        packageData,
        context)
      val pool: DocumentPool = context.getController.getDocumentPool
      if (pool.isMarkedUnavailable(documentKey)) {
        return false
      }
      val doc: TreeInfo = pool.find(documentKey)
      if (doc != null) {
        return true
      }
      val item: Item = DocumentFn.makeDoc(href,
        getStaticBaseUriString,
        packageData,
        null,
        context,
        null,
        silent = true)
      if (item != null) {
        true
      } else {
        pool.markUnavailable(documentKey)
        false
      }
    } catch {
      case e: XPathException => false

    }

}
