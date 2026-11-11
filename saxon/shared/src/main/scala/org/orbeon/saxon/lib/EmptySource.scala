////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// Copyright (c) 2018-2020 Saxonica Limited
// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is "Incompatible With Secondary Licenses", as defined by the Mozilla Public License, v. 2.0.
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
package org.orbeon.saxon.lib

import javax.xml.transform.Source

object EmptySource {
  private val THE_INSTANCE: EmptySource = new EmptySource()
  def getInstance: EmptySource = THE_INSTANCE
}

class EmptySource private () extends Source {
  override def setSystemId(systemId: String): Unit = ()
  override def getSystemId: String = null
}
