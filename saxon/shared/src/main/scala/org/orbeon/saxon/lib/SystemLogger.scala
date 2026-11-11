////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// Copyright (c) 2018-2020 Saxonica Limited
// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is "Incompatible With Secondary Licenses", as defined by the Mozilla Public License, v. 2.0.
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
package org.orbeon.saxon.lib

import java.util.logging.Level
import org.orbeon.saxon.lib.Logger._

/**
 * An implementation of Saxon's `Logger` interface that writes all messages through to
 * a supplied `java.util.logging.Logger`.
 */
class SystemLogger(private val log: java.util.logging.Logger) extends Logger {

  private def getLevel(severity: Int): Level =
    severity match {
      case INFO    => Level.INFO
      case WARNING => Level.WARNING
      case ERROR | DISASTER | _ => Level.SEVERE
    }

  override def println(message: String, severity: Int): Unit =
    log.log(getLevel(severity), message)
}
