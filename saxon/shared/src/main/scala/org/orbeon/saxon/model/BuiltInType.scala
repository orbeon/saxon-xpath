////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// Copyright (c) 2018-2020 Saxonica Limited
// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is "Incompatible With Secondary Licenses", as defined by the Mozilla Public License, v. 2.0.
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
package org.orbeon.saxon.model

import java.util.{HashMap, Map}

import org.orbeon.saxon.om.StandardNames
import org.orbeon.saxon.z.IntHashMap


/**
  * This non-instantiable class acts as a register of Schema objects containing all the built-in types:
  * that is, the types defined in the "xs" namespace.
 *
  * Previously called BuiltInSchemaFactory; but its original function has largely been moved to the two
  * classes `BuiltInAtomicType` and `BuiltInListType`<
  */
object BuiltInType {

  private val lookup: IntHashMap[SchemaType] = new IntHashMap[SchemaType](100)

  private val lookupByLocalName: Map[String, SchemaType] =
    new HashMap[String, SchemaType](100)

  register(StandardNames.XS_ANY_SIMPLE_TYPE, AnySimpleType)
  register(StandardNames.XS_ANY_TYPE,        AnyType.getInstance)
  register(StandardNames.XS_UNTYPED,         Untyped.getInstance)
  register(StandardNames.XS_ERROR,           ErrorType)

  def getSchemaType(fingerprint: Int): SchemaType = {
    var st = lookup.get(fingerprint)
    if (st == null) {
      // this means the method has been called before doing the static initialization of BuiltInAtomicType
      // or BuiltInListType. So force it now
      if (BuiltInAtomicType.DOUBLE == null || BuiltInListType.NMTOKENS == null) {
        // no action, except to force the initialization to run
      }
      st = lookup.get(fingerprint)
    }
    st
  }

  def getSchemaTypeByLocalName(name: String): SchemaType = {
    var st = lookupByLocalName.get(name)
    if (st == null) {
      // this means the method has been called before doing the static initialization of BuiltInAtomicType
      // or BuiltInListType. So force it now
      if (BuiltInAtomicType.DOUBLE == null || BuiltInListType.NMTOKENS == null) {
        // no action, except to force the initialization to run
      }
      st = lookupByLocalName.get(name)
    }
    st
  }

  def register(fingerprint: Int, `type`: SchemaType): Unit = {
    lookup.put(fingerprint, `type`)
    lookupByLocalName.put(`type`.getName, `type`)
  }
}
