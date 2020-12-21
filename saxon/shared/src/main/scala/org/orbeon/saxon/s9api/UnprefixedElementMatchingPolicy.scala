////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// Copyright (c) 2018-2020 Saxonica Limited
// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is "Incompatible With Secondary Licenses", as defined by the Mozilla Public License, v. 2.0.
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
package org.orbeon.saxon.s9api


/**
  * An enumeration defining possible strategies for resolving unprefixed element names appearing
  * as name tests in the steps of a path expression or XSLT match pattern
  */
object UnprefixedElementMatchingPolicy extends Enumeration {

  val DEFAULT_NAMESPACE: UnprefixedElementMatchingPolicy =
    new UnprefixedElementMatchingPolicy()

  val ANY_NAMESPACE: UnprefixedElementMatchingPolicy =
    new UnprefixedElementMatchingPolicy()

  val DEFAULT_NAMESPACE_OR_NONE: UnprefixedElementMatchingPolicy =
    new UnprefixedElementMatchingPolicy()

  class UnprefixedElementMatchingPolicy extends Val

  implicit def convertValue(v: Value): UnprefixedElementMatchingPolicy =
    v.asInstanceOf[UnprefixedElementMatchingPolicy]
}
