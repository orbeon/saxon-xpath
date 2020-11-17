////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// Copyright (c) 2018-2020 Saxonica Limited
// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is "Incompatible With Secondary Licenses", as defined by the Mozilla Public License, v. 2.0.
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
package org.orbeon.saxon.expr

import org.orbeon.saxon.tree.util.FastStringBuffer
import org.orbeon.saxon.value.Cardinality


/**
  * This class contains constants identifying dependencies that an XPath expression
  * might have on its context.
  */
object StaticProperty {

  val DEPENDS_ON_CURRENT_ITEM        : Int = 1
  val DEPENDS_ON_CONTEXT_ITEM        : Int = 1 << 1
  val DEPENDS_ON_POSITION            : Int = 1 << 2
  val DEPENDS_ON_LAST                : Int = 1 << 3
  val DEPENDS_ON_CONTEXT_DOCUMENT    : Int = 1 << 4
  val DEPENDS_ON_CURRENT_GROUP       : Int = 1 << 5
  val DEPENDS_ON_REGEX_GROUP         : Int = 1 << 6
  val DEPENDS_ON_LOCAL_VARIABLES     : Int = 1 << 7
  val DEPENDS_ON_USER_FUNCTIONS      : Int = 1 << 8
  val DEPENDS_ON_ASSIGNABLE_GLOBALS  : Int = 1 << 9
  val DEPENDS_ON_RUNTIME_ENVIRONMENT : Int = 1 << 10
  val DEPENDS_ON_STATIC_CONTEXT      : Int = 1 << 11
  val DEPENDS_ON_XSLT_CONTEXT        : Int = DEPENDS_ON_CURRENT_ITEM | DEPENDS_ON_CURRENT_GROUP | DEPENDS_ON_REGEX_GROUP | DEPENDS_ON_ASSIGNABLE_GLOBALS
  val DEPENDS_ON_FOCUS               : Int = DEPENDS_ON_CONTEXT_ITEM | DEPENDS_ON_POSITION | DEPENDS_ON_LAST | DEPENDS_ON_CONTEXT_DOCUMENT
  val DEPENDS_ON_NON_DOCUMENT_FOCUS  : Int = DEPENDS_ON_CONTEXT_ITEM | DEPENDS_ON_POSITION | DEPENDS_ON_LAST
  val ALLOWS_ZERO                    : Int = 1 << 13
  val ALLOWS_ONE                     : Int = 1 << 14
  val ALLOWS_MANY                    : Int = 1 << 15
  val CARDINALITY_MASK               : Int = ALLOWS_ZERO | ALLOWS_ONE | ALLOWS_MANY
  val ALLOWS_ONE_OR_MORE             : Int = ALLOWS_ONE | ALLOWS_MANY
  val ALLOWS_ZERO_OR_MORE            : Int = ALLOWS_ZERO | ALLOWS_ONE | ALLOWS_MANY
  val ALLOWS_ZERO_OR_ONE             : Int = ALLOWS_ZERO | ALLOWS_ONE
  val EXACTLY_ONE                    : Int = ALLOWS_ONE
  val EMPTY                          : Int = ALLOWS_ZERO

  def getCardinalityCode(cardinality: Int): Int =
    (cardinality & CARDINALITY_MASK) >> 13

  val CONTEXT_DOCUMENT_NODESET       : Int = 1 << 16
  val ORDERED_NODESET                : Int = 1 << 17
  val REVERSE_DOCUMENT_ORDER         : Int = 1 << 18
  val PEER_NODESET                   : Int = 1 << 19
  val SUBTREE_NODESET                : Int = 1 << 20
  val ATTRIBUTE_NS_NODESET           : Int = 1 << 21
  val ALL_NODES_NEWLY_CREATED        : Int = 1 << 22
  val NO_NODES_NEWLY_CREATED         : Int = 1 << 23
  val SINGLE_DOCUMENT_NODESET        : Int = 1 << 24
  val HAS_SIDE_EFFECTS               : Int = 1 << 25
  val NOT_UNTYPED_ATOMIC             : Int = 1 << 26
  val ALL_NODES_UNTYPED              : Int = 1 << 27

  val DEPENDENCY_MASK: Int = DEPENDS_ON_CONTEXT_DOCUMENT | DEPENDS_ON_CONTEXT_ITEM |
      DEPENDS_ON_CURRENT_GROUP |
      DEPENDS_ON_REGEX_GROUP |
      DEPENDS_ON_CURRENT_ITEM |
      DEPENDS_ON_FOCUS |
      DEPENDS_ON_LOCAL_VARIABLES |
      DEPENDS_ON_USER_FUNCTIONS |
      DEPENDS_ON_ASSIGNABLE_GLOBALS |
      DEPENDS_ON_RUNTIME_ENVIRONMENT |
      DEPENDS_ON_STATIC_CONTEXT |
      HAS_SIDE_EFFECTS

  val SPECIAL_PROPERTY_MASK: Int = CONTEXT_DOCUMENT_NODESET | ORDERED_NODESET | REVERSE_DOCUMENT_ORDER |
      PEER_NODESET |
      SUBTREE_NODESET |
      ATTRIBUTE_NS_NODESET |
      SINGLE_DOCUMENT_NODESET |
      NO_NODES_NEWLY_CREATED |
      HAS_SIDE_EFFECTS |
      NOT_UNTYPED_ATOMIC |
      ALL_NODES_UNTYPED |
      ALL_NODES_NEWLY_CREATED

  val NODESET_PROPERTIES: Int = CONTEXT_DOCUMENT_NODESET | ORDERED_NODESET | REVERSE_DOCUMENT_ORDER |
      PEER_NODESET |
      SUBTREE_NODESET |
      ATTRIBUTE_NS_NODESET |
      SINGLE_DOCUMENT_NODESET |
      ALL_NODES_UNTYPED

  // For diagnostic display of static properties
  def display(props: Int): String = {
    val s: FastStringBuffer = new FastStringBuffer(128)
    s.append("D(")
    if ((props & DEPENDS_ON_CURRENT_ITEM) != 0) {
      s.append("U")
    }
    if ((props & DEPENDS_ON_CONTEXT_ITEM) != 0) {
      s.append("C")
    }
    if ((props & DEPENDS_ON_POSITION) != 0) {
      s.append("P")
    }
    if ((props & DEPENDS_ON_LAST) != 0) {
      s.append("L")
    }
    if ((props & DEPENDS_ON_CONTEXT_DOCUMENT) != 0) {
      s.append("D")
    }
    if ((props & DEPENDS_ON_LOCAL_VARIABLES) != 0) {
      s.append("V")
    }
    if ((props & DEPENDS_ON_ASSIGNABLE_GLOBALS) != 0) {
      s.append("A")
    }
    if ((props & DEPENDS_ON_REGEX_GROUP) != 0) {
      s.append("R")
    }
    if ((props & DEPENDS_ON_RUNTIME_ENVIRONMENT) != 0) {
      s.append("E")
    }
    if ((props & DEPENDS_ON_STATIC_CONTEXT) != 0) {
      s.append("S")
    }
    s.append(") C(")
    val m: Boolean = Cardinality.allowsMany(props)
    val z: Boolean = Cardinality.allowsZero(props)
    if (m && z) {
      s.append("*")
    } else if (m) {
      s.append("+")
    } else if (z) {
      s.append("?")
    } else {
      s.append("1")
    }
    s.append(") S(")
    if ((props & HAS_SIDE_EFFECTS) != 0) {
      s.append("E")
    }
    if ((props & NO_NODES_NEWLY_CREATED) != 0) {
      s.append("N")
    }
    if ((props & NOT_UNTYPED_ATOMIC) != 0) {
      s.append("T")
    }
    if ((props & ORDERED_NODESET) != 0) {
      s.append("O")
    }
    if ((props & PEER_NODESET) != 0) {
      s.append("P")
    }
    if ((props & REVERSE_DOCUMENT_ORDER) != 0) {
      s.append("R")
    }
    if ((props & SINGLE_DOCUMENT_NODESET) != 0) {
      s.append("S")
    }
    if ((props & SUBTREE_NODESET) != 0) {
      s.append("D")
    }
    s.append(")")
    s.toString
  }
}
