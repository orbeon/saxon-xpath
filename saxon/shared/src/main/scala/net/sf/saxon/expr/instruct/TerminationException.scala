package net.sf.saxon.expr.instruct

import net.sf.saxon.trans.XPathException


class TerminationException(message: String) extends XPathException(message) {

  this.setErrorCode("XTMM9000")

}