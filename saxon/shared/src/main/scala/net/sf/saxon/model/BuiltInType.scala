////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
package net.sf.saxon.model

import net.sf.saxon.om.StandardNames

import net.sf.saxon.z.IntHashMap

import java.util.HashMap

import java.util.Map




object BuiltInType {

  private var lookup: IntHashMap[SchemaType] = new IntHashMap[SchemaType](100)

  private var lookupByLocalName: Map[String, SchemaType] =
    new HashMap[String, SchemaType](100)

  register(StandardNames.XS_ANY_SIMPLE_TYPE, AnySimpleType.getInstance)

  register(StandardNames.XS_ANY_TYPE, AnyType.getInstance)

  register(StandardNames.XS_UNTYPED, Untyped.getInstance)

  register(StandardNames.XS_ERROR, ErrorType.getInstance)

  def getSchemaType(fingerprint: Int): SchemaType = {
    var st: SchemaType = lookup.get(fingerprint)
    if (st == null) {
// or BuiltInListType. So force it now
      if (BuiltInAtomicType.DOUBLE == null || BuiltInListType.NMTOKENS == null) {}
// no action, except to force the initialization to run
// no action, except to force the initialization to run
      st = lookup.get(fingerprint)
    }
// this means the method has been called before doing the static initialization of BuiltInAtomicType
// this means the method has been called before doing the static initialization of BuiltInAtomicType
    st
  }

  def getSchemaTypeByLocalName(name: String): SchemaType = {
    var st: SchemaType = lookupByLocalName.get(name)
    if (st == null) {
// or BuiltInListType. So force it now
      if (BuiltInAtomicType.DOUBLE == null || BuiltInListType.NMTOKENS == null) {}
// no action, except to force the initialization to run
// no action, except to force the initialization to run
      st = lookupByLocalName.get(name)
    }
// this means the method has been called before doing the static initialization of BuiltInAtomicType
// this means the method has been called before doing the static initialization of BuiltInAtomicType
    st
  }

  def register(fingerprint: Int, `type`: SchemaType): Unit = {
    lookup.put(fingerprint, `type`)
    lookupByLocalName.put(`type`.getName, `type`)
  }

}

// Copyright (c) 2018-2020 Saxonica Limited
// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is "Incompatible With Secondary Licenses", as defined by the Mozilla Public License, v. 2.0.
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
/**
  * This non-instantiable class acts as a register of Schema objects containing all the built-in types:
  * that is, the types defined in the "xs" namespace.
  * <p>Previously called BuiltInSchemaFactory; but its original function has largely been moved to the two
  * classes {@link BuiltInAtomicType} and {@link BuiltInListType}</p>
  */
