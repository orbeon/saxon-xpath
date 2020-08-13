////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
package net.sf.saxon.expr.parser

import net.sf.saxon.s9api.Location

import org.xml.sax.Locator

import javax.xml.transform.SourceLocator

import Loc._

import scala.beans.{BeanProperty, BooleanBeanProperty}


object Loc {

  val NONE: Loc = new Loc(null, -1, -1)

  def makeFromSax(loc: Locator): Loc =
    new Loc(loc.getSystemId, loc.getLineNumber, loc.getColumnNumber)

  def isUnknown(location: Location): Boolean =
    location == null ||
      (location.getSystemId == null || location.getSystemId.isEmpty) &&
        location.getLineNumber == -1

}

class Loc extends Location {
  var systemId: String = _
  var lineNumber: Int = _
  var columnNumber: Int = -1

  def this(loc: SourceLocator) {
    this()
    this.systemId = loc.getSystemId
    this.lineNumber = loc.getLineNumber
    this.columnNumber = loc.getColumnNumber
  }

  def this(systemId: String, lineNumber: Int, columnNumber: Int) = {
    this()
    this.systemId = systemId
    this.lineNumber = lineNumber
    this.columnNumber = columnNumber
  }

  /*@Nullable*/

  def getPublicId(): String = null

  /**
   * Get an immutable copy of this Location object. By default Location objects may be mutable, so they
   * should not be saved for later use. The result of this operation holds the same location information,
   * but in an immutable form.
   */
  def saveLocation(): Location = this

  override def getSystemId(): String = systemId

  override def getLineNumber(): Int = lineNumber

  override def getColumnNumber(): Int = columnNumber
}

// Copyright (c) 2018-2020 Saxonica Limited
// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is "Incompatible With Secondary Licenses", as defined by the Mozilla Public License, v. 2.0.
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
/**
 * Class to hold details of the location of an expression, of an error in a source file, etc.
 * The object is immutable. Previous names: ExpressionLocation, ExplicitLocation.
 */
