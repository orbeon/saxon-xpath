////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
package org.orbeon.saxon.event

import java.util.Properties




trait ReceiverWithOutputProperties extends Receiver {

  def getOutputProperties: Properties

}

// Copyright (c) 2018-2020 Saxonica Limited
// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is "Incompatible With Secondary Licenses", as defined by the Mozilla Public License, v. 2.0.
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
/**
  * Marker interface attached to a {@link Outputter} that retains
  * serialization properties (typically, a Receiver that forms part
  * of the serialization pipeline).
  */
