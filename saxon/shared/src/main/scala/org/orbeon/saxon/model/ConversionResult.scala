////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// Copyright (c) 2018-2020 Saxonica Limited
// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is "Incompatible With Secondary Licenses", as defined by the Mozilla Public License, v. 2.0.
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
package org.orbeon.saxon.model

import org.orbeon.saxon.value.AtomicValue


/**
  * This is a marker interface used as the result of methods that convert or cast values from one type
  * to another. It is implemented by AtomicValue, which indicates a successful conversion, and by
  * ValidationFailure, which indicates an unsuccessful conversion. An unsuccessful conversion does not
  * throw an exception because exceptions are expensive and should not be used on success paths. For example
  * when validating a union, conversion failures are to be expected.
  */
trait ConversionResult {
  def asAtomic(): AtomicValue
}
