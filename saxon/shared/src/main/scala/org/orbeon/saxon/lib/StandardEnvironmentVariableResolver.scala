////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// Copyright (c) 2018-2020 Saxonica Limited
// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is "Incompatible With Secondary Licenses", as defined by the Mozilla Public License, v. 2.0.
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
package org.orbeon.saxon.lib

import java.{util => ju}


/**
  * Default implementation of the `EnvironmentVariableResolver`. This implementation
  * maps "environment variables" as defined in the XPath context to environment variables
  * delivered by Java using the `System` method.
  */
class StandardEnvironmentVariableResolver extends EnvironmentVariableResolver {

  /**
    * Get the list of available environment variables.
    *
    * @return a set of strings; each such string should be an acceptable argument to the
    *         method `#`
    */
  def getAvailableEnvironmentVariables: ju.Set[String] =
    System.getenv.keySet

  /**
    * Get the value of a specific environment variable
    *
    * @param name the name of the required environment variable
    * @return the value of the named environment variable, or null if the variable is
    *         not defined. The method must not return null if the name is in the list of variables
    *         returned by the method `#`
    */
  def getEnvironmentVariable(name: String): String = System.getenv(name)
}
