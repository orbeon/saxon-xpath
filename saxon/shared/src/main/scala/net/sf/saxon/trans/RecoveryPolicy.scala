////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
package net.sf.saxon.trans




object RecoveryPolicy extends Enumeration {

  val RECOVER_SILENTLY: RecoveryPolicy = new RecoveryPolicy()

  val RECOVER_WITH_WARNINGS: RecoveryPolicy = new RecoveryPolicy()

  val DO_NOT_RECOVER: RecoveryPolicy = new RecoveryPolicy()

  class RecoveryPolicy extends Val

  def fromString(s: String): RecoveryPolicy = s match {
    case "recoverSilently" => RECOVER_SILENTLY
    case "recoverWithWarnings" => RECOVER_WITH_WARNINGS
    case "doNotRecover" => DO_NOT_RECOVER
    case _ =>
      throw new IllegalArgumentException(
        "Unrecognized value of RECOVERY_POLICY_NAME = '" + s +
          "'")

  }

  implicit def convertValue(v: Value): RecoveryPolicy =
    v.asInstanceOf[RecoveryPolicy]

}

// Copyright (c) 2018-2020 Saxonica Limited
// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is "Incompatible With Secondary Licenses", as defined by the Mozilla Public License, v. 2.0.
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
