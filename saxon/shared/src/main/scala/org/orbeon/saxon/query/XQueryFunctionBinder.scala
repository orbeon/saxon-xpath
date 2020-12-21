////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
package org.orbeon.saxon.query

import org.orbeon.saxon.functions.FunctionLibrary

import org.orbeon.saxon.om.StructuredQName




trait XQueryFunctionBinder extends FunctionLibrary {

  /*@Nullable*/

  def getDeclaration(functionName: StructuredQName,
                     staticArgs: Int): XQueryFunction

}

// Copyright (c) 2018-2020 Saxonica Limited
// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is "Incompatible With Secondary Licenses", as defined by the Mozilla Public License, v. 2.0.
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
/**
  * XQueryFunctionBinder is an extension of the FunctionLibrary interface used for function libraries
  * that contain user-written XQuery functions. It provides a method that allows the XQueryFunction
  * with a given name and arity to be located.
  */
