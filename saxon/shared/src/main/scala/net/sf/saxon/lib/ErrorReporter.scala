////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
package net.sf.saxon.lib

import net.sf.saxon.s9api.XmlProcessingError

import net.sf.saxon.trans.UncheckedXPathException

import org.xml.sax.ErrorHandler

import javax.xml.transform.ErrorListener




@FunctionalInterface
trait ErrorReporter {

  def report(error: XmlProcessingError): Unit

}

// Copyright (c) 2020 Saxonica Limited
// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is "Incompatible With Secondary Licenses", as defined by the Mozilla Public License, v. 2.0.
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
/**
  * The ErrorReporter is a generic functional interface for reporting errors and
  * warnings.
  *
  * <p>The error or warning is reported in the form of an {@link XmlProcessingError};
  * unlike the JAXP {@link ErrorListener} and {@link ErrorHandler} it is possible
  * to report errors without expensively construction an exception object.</p>
  *
  * <p>The {@code ErrorReporter}'s {@code accept()} method does not throw any checked
  * exceptions. It may however, throw an {@link UncheckedXPathException} indicating
  * that processing is to be terminated without looking for further errors.</p>
  *
  * <p>A warning condition is notified using an <code>XmlProcessingError</code>
  * object whose {@code isWarning()} property is true.</p>
  *
  * @since 10.0
  */
