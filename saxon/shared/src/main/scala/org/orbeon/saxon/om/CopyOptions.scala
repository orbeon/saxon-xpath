////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
package org.orbeon.saxon.om

import org.orbeon.saxon.event.ReceiverOption




object CopyOptions {

  val ALL_NAMESPACES: Int = 2

  val TYPE_ANNOTATIONS: Int = 4

  val FOR_UPDATE: Int = 8

  def includes(options: Int, option: Int): Boolean =
    (options & option) == option

  def getStartDocumentProperties(copyOptions: Int): Int =
    if (CopyOptions.includes(copyOptions, CopyOptions.FOR_UPDATE))
      ReceiverOption.MUTABLE_TREE
    else ReceiverOption.NONE

}

// Copyright (c) 2018-2020 Saxonica Limited
// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is "Incompatible With Secondary Licenses", as defined by the Mozilla Public License, v. 2.0.
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
