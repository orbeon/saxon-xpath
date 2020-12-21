////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
package org.orbeon.saxon.trans

import LicenseException._

import scala.beans.{BeanProperty, BooleanBeanProperty}




object LicenseException {

  val EXPIRED: Int = 1

  val INVALID: Int = 2

  val NOT_FOUND: Int = 3

  val WRONG_FEATURES: Int = 4

  val CANNOT_READ: Int = 5

  val WRONG_CONFIGURATION: Int = 6

}

/**
  * Exception thrown when there are problems with the license file
  */
class LicenseException(message: String, @BeanProperty var reason: Int)
    extends RuntimeException(message)

// Copyright (c) 2018-2020 Saxonica Limited
// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is "Incompatible With Secondary Licenses", as defined by the Mozilla Public License, v. 2.0.
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
