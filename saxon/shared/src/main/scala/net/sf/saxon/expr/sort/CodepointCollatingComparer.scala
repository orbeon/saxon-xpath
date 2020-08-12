////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
package net.sf.saxon.expr.sort

import net.sf.saxon.expr.XPathContext

import net.sf.saxon.lib.StringCollator

import net.sf.saxon.value.AtomicValue

import net.sf.saxon.value.StringValue

import CodepointCollatingComparer._




object CodepointCollatingComparer {

  private var collator: CodepointCollator = CodepointCollator.getInstance

  private var THE_INSTANCE: CodepointCollatingComparer =
    new CodepointCollatingComparer()

  def getInstance(): CodepointCollatingComparer = THE_INSTANCE

}

class CodepointCollatingComparer private () extends AtomicComparer {

  def getCollator(): StringCollator = collator

  def provideContext(context: XPathContext): AtomicComparer = this

  def compareAtomicValues(a: AtomicValue, b: AtomicValue): Int = {
    if (a == null) {
      (if (b == null) 0 else -1)
    } else if (b == null) {
      +1
    }
    val as: StringValue = a.asInstanceOf[StringValue]
    val bs: StringValue = b.asInstanceOf[StringValue]
//if (as.containsSurrogatePairs() || bs.containsSurrogatePairs()) {
    CodepointCollator.compareCS(as.getStringValueCS, bs.getStringValueCS)
  }
//} else {
// optimize to use UTF-16 binary comparison
//    return as.getStringValue().compareTo(bs.getStringValue());
//}
//} else {
// optimize to use UTF-16 binary comparison
//    return as.getStringValue().compareTo(bs.getStringValue());
//}

  def comparesEqual(a: AtomicValue, b: AtomicValue): Boolean =
    a.asInstanceOf[StringValue].codepointEquals(b.asInstanceOf[StringValue])

  /**
    * Create a string representation of this AtomicComparer that can be saved in a compiled
    * package and used to reconstitute the AtomicComparer when the package is reloaded
    *
    * @return a string representation of the AtomicComparer
    */
  def save(): String = "CCC"

}

// Copyright (c) 2018-2020 Saxonica Limited
// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is "Incompatible With Secondary Licenses", as defined by the Mozilla Public License, v. 2.0.
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
/**
  * An AtomicComparer used for comparing strings, untypedAtomic values, and URIs using the Unicode codepoint
  * collation.
  * A CodepointCollatingComparer is used when it is known in advance that both operands will be of these
  * types, and when the collation is the unicode codepoint collation.
  * This enables all conversions and promotions to be bypassed: the string values of both operands
  * are simply extracted and passed to the collator for comparison.
  * <p>The difference between using this class and using the underlying CodepointCollator directly is that
  * the compare() method in this class expects two instances of AtomicValue as its operands, whereas the
  * underlying class expects two instances of java.lang.String. This class makes use of the extra information
  * held in the wrapping StringValue object, specifically, the knowledge of whether the string contains
  * surrogate pairs.</p>
  *
  */
