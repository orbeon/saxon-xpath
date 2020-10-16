////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// Copyright (c) 2018-2020 Saxonica Limited
// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is "Incompatible With Secondary Licenses", as defined by the Mozilla Public License, v. 2.0.
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
package org.orbeon.saxon.lib

import java.util.Set


/**
  * This interface defines a Saxon plug-in used to resolve calls on the XPath 3.0
  * functions available-environment-variables() and environment-variable(). The standard
  * implementation reads environment variables using the Java method {@link System#getenv()};
  * this can be overridden by a user-provided implementation that resolves environment variables
  * any way it likes.
  */
trait EnvironmentVariableResolver {
  def getAvailableEnvironmentVariables: Set[String]
  def getEnvironmentVariable(name: String): String
}
