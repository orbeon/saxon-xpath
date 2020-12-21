////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// Copyright (c) 2018-2020 Saxonica Limited
// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is "Incompatible With Secondary Licenses", as defined by the Mozilla Public License, v. 2.0.
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
package org.orbeon.saxon.functions.registry

import org.orbeon.saxon.functions._
import org.orbeon.saxon.functions.registry.BuiltInFunctionSet._
import org.orbeon.saxon.model.BuiltInAtomicType

object UseWhen30FunctionSet {
  val getInstance: UseWhen30FunctionSet = new UseWhen30FunctionSet
}


/**
 * Function signatures (and pointers to implementations) of the functions available for use
 * in static expressions (including use-when expressions) in XSLT 3.0 stylesheets
 */
class UseWhen30FunctionSet extends BuiltInFunctionSet {

  locally {
    addXPathFunctions()
    register("available-system-properties",
             0,
             () => new AvailableSystemProperties,
             BuiltInAtomicType.QNAME,
      BuiltInFunctionSet.STAR,
      BuiltInFunctionSet.LATE)
    register("element-available",
             1,
      () => new ElementAvailable,
             BuiltInAtomicType.BOOLEAN,
      BuiltInFunctionSet.ONE,
      BuiltInFunctionSet.NS).arg(0, BuiltInAtomicType.STRING, ONE, null)
    register("function-available",
             1,
             () => new FunctionAvailable,
             BuiltInAtomicType.BOOLEAN,
             ONE,
             NS | LATE).arg(0, BuiltInAtomicType.STRING, ONE, null)
    register("function-available",
             2,
      () => new FunctionAvailable,
             BuiltInAtomicType.BOOLEAN,
             ONE,
             NS | LATE)
      .arg(0, BuiltInAtomicType.STRING, ONE, null)
      .arg(1, BuiltInAtomicType.INTEGER, ONE, null)
    register("system-property",
             1,
             () => new SystemProperty,
             BuiltInAtomicType.STRING,
             ONE,
             NS | LATE).arg(0, BuiltInAtomicType.STRING, ONE, null)
    register("type-available",
             1,
      () => new TypeAvailable,
             BuiltInAtomicType.BOOLEAN,
             ONE,
             NS).arg(0, BuiltInAtomicType.STRING, ONE, null)
  }

   def addXPathFunctions(): Unit =
    importFunctionSet(XPath31FunctionSet.getInstance)
}
