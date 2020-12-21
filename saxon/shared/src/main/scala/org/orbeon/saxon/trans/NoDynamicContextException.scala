////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
package org.orbeon.saxon.trans




/**
  * This exception class is used when early (compile-time) evaluation of an expression
  * is attempted, and the expression requires knowledge of the current dateTime or implicit
  * timezone. This exception should be caught internally, and should result in evaluation
  * of the expression being deferred until run-time
  */
class NoDynamicContextException(message: String)
    extends XPathException("Dynamic context missing: " + message)

// Copyright (c) 2018-2020 Saxonica Limited
// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is "Incompatible With Secondary Licenses", as defined by the Mozilla Public License, v. 2.0.
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
