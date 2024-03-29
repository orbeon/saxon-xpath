////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
package org.orbeon.saxon.regex




/**
  * Exception thrown to indicate a syntax error in a regular expression.
  * This is a non-checked exception because you should only have problems compiling
  * a regular expression during development.
  * If you are making regular expression programs dynamically then you can catch it
  * if you wish. But should not be forced to.
  *
  * @author <a href="mailto:jonl@muppetlabs.com">Jonathan Locke</a>
  * @author <a href="mailto:gholam@xtra.co.nz">Michael McCallum</a>
  * @version $Id: RESyntaxException.java 518156 2007-03-14 14:31:26Z vgritsenko $
  */
class RESyntaxException /**
  * Constructor.
  *
  * @param s Further description of the syntax error
  */
(s: String)
    extends RuntimeException(s) {

  /**
    * Constructor.
    *
    * @param s      Further description of the syntax error
    * @param offset character position within regex where the error was detected
    */
  /**
    * Constructor.
    *
    * @param s      Further description of the syntax error
    * @param offset character position within regex where the error was detected
    */
  def this(s: String, offset: Int) =
    this(???) /* TODO: Scala does not allow multiple super constructor calls
 * Change this code to call a constructor of the current class instead.
 * For your convenience, here is the invalid super constructor call:
 * }super("Syntax error at char " + offset + " in regular expression: " +
  s)
 */

}

// Copyright (c) 2018-2020 Saxonica Limited
// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is "Incompatible With Secondary Licenses", as defined by the Mozilla Public License, v. 2.0.
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
/*
 * Licensed to the Apache Software Foundation (ASF) under one or more
 * contributor license agreements.  See the NOTICE file distributed with
 * this work for additional information regarding copyright ownership.
 * The ASF licenses this file to You under the Apache License, Version 2.0
 * (the "License"); you may not use this file except in compliance with
 * the License.  You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
