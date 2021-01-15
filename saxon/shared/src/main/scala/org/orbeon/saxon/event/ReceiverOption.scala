////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// Copyright (c) 2018-2020 Saxonica Limited
// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is "Incompatible With Secondary Licenses", as defined by the Mozilla Public License, v. 2.0.
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
package org.orbeon.saxon.event


/**
  * ReceiverOption defines a set of constants representing boolean flags, which can be used in
  * calls to methods on the Receiver interface.
  */
object ReceiverOption {

  val NONE                               : Int = 0
  val DISABLE_ESCAPING                   : Int = 0x01
  val DISABLE_CHARACTER_MAPS             : Int = 0x02
  val NO_SPECIAL_CHARS                   : Int = 0x04
  val DEFAULTED_VALUE                    : Int = 0x08
  val NILLED_ELEMENT                     : Int = 0x10
  val REJECT_DUPLICATES                  : Int = 0x20
  val NAMESPACE_OK                       : Int = 0x40
  val DISINHERIT_NAMESPACES              : Int = 0x80
  val USE_NULL_MARKERS                   : Int = 0x100
  val NILLABLE_ELEMENT                   : Int = 0x200
  val WHOLE_TEXT_NODE                    : Int = 0x400
  val IS_ID                              : Int = 0x800
  val IS_IDREF                           : Int = 0x1000
  val ID_IDREF_CHECKED                   : Int = 0x2000
  val TERMINATE                          : Int = 0x4000
  val MUTABLE_TREE                       : Int = 0x8000
  val REFUSE_NAMESPACES                  : Int = 0x10000
  val BEQUEATH_INHERITED_NAMESPACES_ONLY : Int = 0x20000
  val HAS_CHILDREN                       : Int = 0x40000

  /**
    * Flag set on append() to indicate that all in-scope namespaces should be copied
    */
  val ALL_NAMESPACES: Int = 0x80000

  /**
    * Flag set on attribute() to indicate that there is no need to check for duplicate attributes
    */
  val NOT_A_DUPLICATE: Int = 0x100000

  /**
    * Flag set on characters() to indicate that the text node is a separator space between atomic values
    */
  val SEPARATOR: Int = 0x100000

  def contains(options: Int, option: Int): Boolean = (options & option) != 0
}
