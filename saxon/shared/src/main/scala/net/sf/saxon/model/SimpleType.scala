////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
package net.sf.saxon.model

import net.sf.saxon.lib.ConversionRules

import net.sf.saxon.om.AtomicSequence

import net.sf.saxon.om.NamespaceResolver




trait SimpleType extends SchemaType {

  def isAtomicType(): Boolean

  /**
    * Test whether this Simple Type is a list type
    *
    * @return true if this is a list type
    */
  def isListType(): Boolean

  def isUnionType(): Boolean

  def isBuiltInType(): Boolean

  /*@Nullable*/

  def getBuiltInBaseType(): SchemaType

  def getTypedValue(value: CharSequence,
                    resolver: NamespaceResolver,
                    rules: ConversionRules): AtomicSequence

  /*@Nullable*/

  def validateContent(value: CharSequence,
                      nsResolver: NamespaceResolver,
                      rules: ConversionRules): ValidationFailure

  def isNamespaceSensitive(): Boolean

  def getWhitespaceAction(): Int

  def preprocess(input: CharSequence): CharSequence

  def postprocess(input: CharSequence): CharSequence

}

// Copyright (c) 2018-2020 Saxonica Limited
// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is "Incompatible With Secondary Licenses", as defined by the Mozilla Public License, v. 2.0.
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
/**
  * This interface represents a simple type, which may be a built-in simple type, or
  * a user-defined simple type.
  */
