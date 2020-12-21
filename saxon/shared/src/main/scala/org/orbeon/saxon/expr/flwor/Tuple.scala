////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
package org.orbeon.saxon.expr.flwor

import org.orbeon.saxon.om.Sequence

import org.orbeon.saxon.value.ObjectValue




/**
  * A tuple, as it appears in an XQuery tuple stream handled by extended FLWOR expressions.
  */
class Tuple(members: Array[Sequence])
    extends ObjectValue[Array[Sequence]](members) {

  def getMembers: Array[Sequence] = getObject

}

// Copyright (c) 2018-2020 Saxonica Limited
// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is "Incompatible With Secondary Licenses", as defined by the Mozilla Public License, v. 2.0.
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
