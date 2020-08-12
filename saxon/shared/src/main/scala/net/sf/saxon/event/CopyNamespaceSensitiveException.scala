////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
package net.sf.saxon.event

import net.sf.saxon.trans.XPathException




class CopyNamespaceSensitiveException(message: String)
    extends XPathException(message)

// Copyright (c) 2018-2020 Saxonica Limited
// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is "Incompatible With Secondary Licenses", as defined by the Mozilla Public License, v. 2.0.
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
/**
  * Exception indicating that an attempt was made to copy namespace-sensitive content
  * without copying its associated namespaces
  */
