////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
package net.sf.saxon.ma.map

import net.sf.saxon.model.FunctionItemType

import net.sf.saxon.value.SequenceType




/**
  * An instance of this class represents a specific tuple item type, for example
  * tuple(x as xs:double, y as element(employee)).
  *
  * Tuple types are a Saxon extension introduced in Saxon 9.8. The syntax for constructing
  * a tuple type requires Saxon-PE or higher, but the supporting code is included in
  * Saxon-HE for convenience.
  *
  * Extended in 10.0 to distinguish extensible vs non-extensible tuple types. Extensible tuple
  * types permit fields other than those listed to appear; non-extensible tuple types do not.
  */
trait TupleType extends FunctionItemType {

  def getFieldNames: java.lang.Iterable[String]

  def getFieldType(field: String): SequenceType

  def isExtensible: Boolean

}

// Copyright (c) 2018-2020 Saxonica Limited
// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is "Incompatible With Secondary Licenses", as defined by the Mozilla Public License, v. 2.0.
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// Copyright (c) 2018-2020 Saxonica Limited
