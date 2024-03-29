////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
package org.orbeon.saxon.lib

import java.util.Properties




/**
  * Interface allowing localization modules for different languages to be dynamically loaded
  */
abstract class LocalizerFactory {

  def setLanguageProperties(lang: String, properties: Properties): Unit = ()
// no action
// no action

  def getNumberer(language: String, country: String): Numberer

  def copy(): LocalizerFactory = this

}

// Copyright (c) 2018-2020 Saxonica Limited
// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is "Incompatible With Secondary Licenses", as defined by the Mozilla Public License, v. 2.0.
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
