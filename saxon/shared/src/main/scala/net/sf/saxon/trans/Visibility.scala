////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
package net.sf.saxon.trans




object Visibility extends Enumeration {

  val PUBLIC: Visibility = new Visibility()

  val PRIVATE: Visibility = new Visibility()

  val FINAL: Visibility = new Visibility()

  val ABSTRACT: Visibility = new Visibility()

  val HIDDEN: Visibility = new Visibility()

  class Visibility extends Val {

    def show(): String = super.toString.toLowerCase()

  }

  implicit def convertValue(v: Value): Visibility = v.asInstanceOf[Visibility]

}

// Copyright (c) 2018-2020 Saxonica Limited
// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is "Incompatible With Secondary Licenses", as defined by the Mozilla Public License, v. 2.0.
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
/**
  * Enumeration class giving the different visibility levels defined for components in XSLT 3.0
  */
