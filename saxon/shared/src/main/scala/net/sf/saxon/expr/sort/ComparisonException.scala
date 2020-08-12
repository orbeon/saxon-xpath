////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
package net.sf.saxon.expr.sort

import net.sf.saxon.trans.XPathException




class ComparisonException(var cause: XPathException)
    extends ClassCastException {

  override def getMessage(): String = cause.getMessage

  override def getCause(): XPathException = cause

}

// Copyright (c) 2018-2020 Saxonica Limited
// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is "Incompatible With Secondary Licenses", as defined by the Mozilla Public License, v. 2.0.
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
/**
  * A <code>ComparisonException</code> is a <code>ClassCastException</code> that
  * encapsulates an <code>XPathException</code>. It is used because the <code>compareTo</code>
  * method is defined to return <code>ClassCastException</code> when values are not comparable;
  * using a <code>ClassCastException</code> that encapsulates XPath error information enables
  * us to return the correct error code, and to distinguish dynamic errors from type errors.
  */
