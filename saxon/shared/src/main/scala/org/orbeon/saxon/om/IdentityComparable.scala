////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// Copyright (c) 2018-2020 Saxonica Limited
// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is "Incompatible With Secondary Licenses", as defined by the Mozilla Public License, v. 2.0.
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
package org.orbeon.saxon.om


/**
  * The IdentityComparable class provides a way to compare objects for strong equality.
  * In some cases it may test for Java-level object identity, but this is not essential. For example,
  * with date/time values it checks that the values are not only equal according to the XPath rules,
  * but also have the same timezone (or absence of a timezone).
  */
trait IdentityComparable {

  /**
    * Determine whether two IdentityComparable objects are identical. This is a stronger
    * test than equality (even schema-equality); for example two dateTime values are not identical unless
    * they are in the same timezone.
    *
    * @param other the value to be compared with
    * @return true if the two values are indentical, false otherwise
    */
  def isIdentical(other: IdentityComparable): Boolean

  def identityHashCode(): Int
}
