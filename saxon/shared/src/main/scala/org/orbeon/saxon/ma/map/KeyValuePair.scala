////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// Copyright (c) 2018-2020 Saxonica Limited
// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is "Incompatible With Secondary Licenses", as defined by the Mozilla Public License, v. 2.0.
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
package org.orbeon.saxon.ma.map

import org.orbeon.saxon.om.GroundedValue
import org.orbeon.saxon.value.AtomicValue


/**
  * A key and a corresponding value to be held in a Map.
  */
case class KeyValuePair(key: AtomicValue, value: GroundedValue)
