////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
package net.sf.saxon.dom

import org.w3c.dom.DOMException

import DOMExceptionImpl._


object DOMExceptionImpl {

  /**
   * @since DOM Level 2
   */
  /**
   * @since DOM Level 2
   */
  val INVALID_STATE_ERR: Short = 11

  /**
   * @since DOM Level 2
   */
  /**
   * @since DOM Level 2
   */
  val SYNTAX_ERR: Short = 12

  /**
   * @since DOM Level 2
   */
  /**
   * @since DOM Level 2
   */
  val INVALID_MODIFICATION_ERR: Short = 13

  /**
   * @since DOM Level 2
   */
  /**
   * @since DOM Level 2
   */
  val NAMESPACE_ERR: Short = 14

  /**
   * @since DOM Level 2
   */
  /**
   * @since DOM Level 2
   */
  val INVALID_ACCESS_ERR: Short = 15

}

/**
 * DOM operations only raise exceptions in "exceptional" circumstances,
 * i.e., when an operation is impossible to perform (either for logical
 * reasons, because data is lost, or  because the implementation has become
 * unstable). In general, DOM methods return specific error values in ordinary
 * processing situations, such as out-of-bound errors when using
 * <code>NodeList</code> .
 * <p> Implementations may raise other exceptions under other circumstances.
 * For example, implementations may raise an implementation-dependent
 * exception if a <code>null</code> argument is passed.
 * <p>See also the <a href='http://www.w3.org/TR/2000/CR-DOM-Level-2-20000510'>Document Object Model (DOM) Level 2 Specification</a>.
 */
class DOMExceptionImpl(code: Short, message: String)
  extends DOMException(code, message) {

  var codShort: Short = _

}

// Copyright (c) 2018-2020 Saxonica Limited
// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is "Incompatible With Secondary Licenses", as defined by the Mozilla Public License, v. 2.0.
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
