package net.sf.saxon.functions

import net.sf.saxon.expr.PackageData

import net.sf.saxon.expr.XPathContext

import net.sf.saxon.om._

import net.sf.saxon.trans.XPathException

import net.sf.saxon.value.AtomicValue

import net.sf.saxon.value.BooleanValue

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
      isDocAvailable(arguments(0).head().asInstanceOf[AtomicValue], context))

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
