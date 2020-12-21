////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
package org.orbeon.saxon.regex




/**
  * A precondition that must be true if a regular expression is to match
  */
class RegexPrecondition(op: Operation, fixedPos: Int, minPos: Int) {

  var operation: Operation = op

  var fixedPosition: Int = fixedPos

  var minPosition: Int = minPos

}

// Copyright (c) 2018-2020 Saxonica Limited
// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is "Incompatible With Secondary Licenses", as defined by the Mozilla Public License, v. 2.0.
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
