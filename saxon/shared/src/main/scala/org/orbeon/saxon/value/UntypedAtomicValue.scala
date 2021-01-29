////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// Copyright (c) 2018-2020 Saxonica Limited
// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is "Incompatible With Secondary Licenses", as defined by the Mozilla Public License, v. 2.0.
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
package org.orbeon.saxon.value

import org.orbeon.saxon.model.{AtomicType, BuiltInAtomicType}

/**
  * An Untyped Atomic value. This inherits from StringValue for implementation convenience, even
  * though an untypedAtomic value is not a String in the data model type hierarchy.
  */
object UntypedAtomicValue {
  val ZERO_LENGTH_UNTYPED: UntypedAtomicValue = new UntypedAtomicValue("")
}

class UntypedAtomicValue(value: CharSequence)
  extends StringValue(value, BuiltInAtomicType.UNTYPED_ATOMIC) {

  /*@NotNull*/
  override def copyAsSubType(typeLabel: AtomicType): AtomicValue = {
    if (typeLabel != BuiltInAtomicType.UNTYPED_ATOMIC)
      throw new UnsupportedOperationException
    this
  }

  /*@NotNull*/
  override def getPrimitiveType: BuiltInAtomicType = BuiltInAtomicType.UNTYPED_ATOMIC
  override def getStringValueCS: CharSequence = value
  override def toShortString: String = "u" + super.toShortString
}
