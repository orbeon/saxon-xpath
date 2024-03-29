////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
package org.orbeon.saxon.expr.flwor

import org.orbeon.saxon.om.{NamespaceResolver, StructuredQName}
import org.orbeon.saxon.s9api.Location
import org.orbeon.saxon.trace.Traceable

import scala.beans.BeanProperty




/**
  * A "trace" clause in a FLWOR expression, added by a TraceCodeInjector
  */
class ClauseInfo(@BeanProperty var clause: Clause) extends Traceable {

  private var nsResolver: NamespaceResolver = _

  def getLocation: Location = clause.getLocation

  /**
    * Get a name identifying the object of the expression, for example a function name, template name,
    * variable name, key name, element name, etc. This is used only where the name is known statically.
    *
    * @return the QName of the object declared or manipulated by this instruction or expression
    */
  def getObjectName: StructuredQName = {
    val vars: Array[LocalVariableBinding] = clause.getRangeVariables
    if (vars != null && vars.length > 0) {
      vars(0).getVariableQName
    } else {
      null
    }
  }

  def getNamespaceResolver: NamespaceResolver = nsResolver

  def setNamespaceResolver(nsResolver: NamespaceResolver): Unit = {
    this.nsResolver = nsResolver
  }

  /**
    * Get the system identifier (URI) of the source stylesheet or query module containing
    * the instruction. This will generally be an absolute URI. If the system
    * identifier is not known, the method may return null. In some cases, for example
    * where XML external entities are used, the correct system identifier is not
    * always retained.
    *
    * @return the URI of the containing module
    */
  def getSystemId: String = clause.getLocation.getSystemId

  /**
    * Get the line number of the instruction in the source stylesheet module.
    * If this is not known, or if the instruction is an artificial one that does
    * not relate to anything in the source code, the value returned may be -1.
    *
    * @return the line number of the expression within the containing module
    */
  def getLineNumber: Int = clause.getLocation.getLineNumber

  /**
    * Return the public identifier for the current document event.
    * <p>The return value is the public identifier of the document
    * entity or of the external parsed entity in which the markup
    * triggering the event appears.</p>
    *
    * @return A string containing the public identifier, or
    *         null if none is available.
    * @see #getSystemId
    */
  def getPublicId: String = null

  /**
    * Return the column number where the current document event ends.
    * This is one-based number of Java <code>char</code> values since
    * the last line end.
    * <p><strong>Warning:</strong> The return value from the method
    * is intended only as an approximation for the sake of diagnostics;
    * it is not intended to provide sufficient information
    * to edit the character content of the original XML document.
    * For example, when lines contain combining character sequences, wide
    * characters, surrogate pairs, or bi-directional text, the value may
    * not correspond to the column in a text editor's display. </p>
    * <p>The return value is an approximation of the column number
    * in the document entity or external parsed entity where the
    * markup triggering the event appears.</p>
    * <p>If possible, the SAX driver should provide the line position
    * of the first character after the text associated with the document
    * event.  The first column in each line is column 1.</p>
    *
    * @return The column number, or -1 if none is available.
    * @see #getLineNumber
    */
  def getColumnNumber: Int = -1

}

// Copyright (c) 2018-2020 Saxonica Limited
// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is "Incompatible With Secondary Licenses", as defined by the Mozilla Public License, v. 2.0.
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
