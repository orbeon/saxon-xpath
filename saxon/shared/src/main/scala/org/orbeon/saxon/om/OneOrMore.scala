////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// Copyright (c) 2018-2020 Saxonica Limited
// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is "Incompatible With Secondary Licenses", as defined by the Mozilla Public License, v. 2.0.
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
/**
  * A value that is a sequence containing one or more items. The main use is in declarations of reflexive extension
  * functions, where declaring an argument of type &lt;OneOrMore&lt;IntegerValue&gt;&gt; triggers automatic type
  * checking in the same way as for a native XSLT/XQuery function declaring the type as xs:integer+.
  */

package org.orbeon.saxon.om

import org.orbeon.saxon.value.SequenceExtent


object OneOrMore {

  def makeOneOrMore(sequence: Sequence): OneOrMore[Item] = {
    val content: List[Item] = List[Item]()
    sequence.iterate().forEachOrFail(item => (content:+item))
    if (content.isEmpty) {
      throw new IllegalArgumentException
    }
    new OneOrMore(content)
  }
}

class OneOrMore[T <: Item](content: Array[Item]) extends SequenceExtent(content) {

  if (content.length == 0)
    throw new IllegalArgumentException

  def this(content: List[T]) = {
    this(content.toArray[Item])

    if (content.isEmpty)
      throw new IllegalArgumentException
  }
}
