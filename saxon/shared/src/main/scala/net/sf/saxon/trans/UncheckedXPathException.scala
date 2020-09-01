////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
package net.sf.saxon.trans




class UncheckedXPathException(private var cause: XPathException)
    extends RuntimeException {

  def getXPathException: XPathException = cause

  override def getMessage(): String = cause.getMessage

}