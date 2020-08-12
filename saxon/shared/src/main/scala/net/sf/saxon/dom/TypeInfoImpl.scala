////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
package net.sf.saxon.dom

import net.sf.saxon.utils.Configuration

import net.sf.saxon.model.AnyType

import net.sf.saxon.model.SchemaType

import org.w3c.dom.TypeInfo




class TypeInfoImpl(private var config: Configuration, `type`: SchemaType)
    extends TypeInfo {

  private var schemaType: SchemaType = `type`

  def getTypeName(): String = schemaType.getStructuredQName.getLocalPart

  def getTypeNamespace(): String = schemaType.getStructuredQName.getURI

  def isDerivedFrom(typeNamespaceArg: String,
                    typeNameArg: String,
                    derivationMethod: Int): Boolean = {
    val base: SchemaType = schemaType.getBaseType
    val fingerprint: Int =
      config.getNamePool.allocateFingerprint(typeNamespaceArg, typeNameArg)
    if (derivationMethod == 0 ||
        (derivationMethod & schemaType.getDerivationMethod) !=
          0) {
      if (base.getFingerprint == fingerprint) {
        true
      } else if (base.isInstanceOf[Any]) {
        false
      } else {
        new TypeInfoImpl(config, base)
          .isDerivedFrom(typeNamespaceArg, typeNameArg, derivationMethod)
      }
    }
    false
  }
// Note: if derivationMethod is RESTRICTION, this interpretation requires every step to be derived
// by restriction. An alternative interpretation is that at least one step must be derived by restriction.
// Note: if derivationMethod is RESTRICTION, this interpretation requires every step to be derived
// by restriction. An alternative interpretation is that at least one step must be derived by restriction.

}

// Copyright (c) 2018-2020 Saxonica Limited
// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is "Incompatible With Secondary Licenses", as defined by the Mozilla Public License, v. 2.0.
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
/**
  * This class implements the DOM TypeInfo interface as a wrapper over the Saxon SchemaType
  * interface.
  */
