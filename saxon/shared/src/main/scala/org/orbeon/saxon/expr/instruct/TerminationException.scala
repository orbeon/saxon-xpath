package org.orbeon.saxon.expr.instruct

import org.orbeon.saxon.trans.XPathException


class TerminationException(message: String) extends XPathException(message) {

  this.setErrorCode("XTMM9000")

}