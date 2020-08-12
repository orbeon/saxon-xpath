////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
package net.sf.saxon.lib

import net.sf.saxon.model.ValidationFailure

import net.sf.saxon.om.Sequence

import net.sf.saxon.trans.XPathException

import javax.xml.transform.ErrorListener

import javax.xml.transform.TransformerException

import scala.beans.{BeanProperty, BooleanBeanProperty}




/**
  * Interface for reporting validation errors found during validation of an instance document
  * against a schema.
  */
class InvalidityHandlerWrappingErrorListener(
    @BeanProperty var errorListener: ErrorListener)
    extends InvalidityHandler {

  /**
    * At the start of a validation episode, initialize the handler
    *
    * @param systemId optional; may be used to represent the destination of any report produced
    * @throws XPathException if initialization of the invalidity handler fails for any reason
    */
  def startReporting(systemId: String): Unit = {}
// no action
// no action

  def reportInvalidity(failure: Invalidity): Unit = {
    errorListener.error(
      (failure.asInstanceOf[ValidationFailure].makeException()))
  }

  /**
    * Get the value to be associated with a validation exception. May return null.
    * In the case of the InvalidityReportGenerator, this returns the XML document
    * containing the validation report
    *
    * @return a value (or null). This will be the value returned as the value of
    * the variable $err:value during try/catch processing
    */
  def endReporting(): Sequence = null

}

// Copyright (c) 2018-2020 Saxonica Limited
// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is "Incompatible With Secondary Licenses", as defined by the Mozilla Public License, v. 2.0.
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
