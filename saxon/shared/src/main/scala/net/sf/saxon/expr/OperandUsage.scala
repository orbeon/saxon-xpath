////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
package net.sf.saxon.expr




object OperandUsage extends Enumeration {

  val ABSORPTION: OperandUsage = new OperandUsage()

  val INSPECTION: OperandUsage = new OperandUsage()

  val TRANSMISSION: OperandUsage = new OperandUsage()

  val NAVIGATION: OperandUsage = new OperandUsage()

  class OperandUsage extends Val

  implicit def convertValue(v: Value): OperandUsage =
    v.asInstanceOf[OperandUsage]

}

// Copyright (c) 2018-2020 Saxonica Limited
// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is "Incompatible With Secondary Licenses", as defined by the Mozilla Public License, v. 2.0.
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
