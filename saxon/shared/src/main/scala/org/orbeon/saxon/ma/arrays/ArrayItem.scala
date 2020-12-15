////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// Copyright (c) 2018-2020 Saxonica Limited
// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is "Incompatible With Secondary Licenses", as defined by the Mozilla Public License, v. 2.0.
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
package org.orbeon.saxon.ma.arrays

import org.orbeon.saxon.expr.StaticProperty
import org.orbeon.saxon.model.TypeHierarchy
import org.orbeon.saxon.om.Genre.Genre
import org.orbeon.saxon.om.{Function, Genre, GroundedValue}
import org.orbeon.saxon.value.SequenceType
import org.orbeon.saxon.z.IntSet

import scala.util.control.Breaks._


/**
 * Interface supported by different implementations of an XDM array item
 */
trait ArrayItem extends Function {

  var SINGLE_ARRAY_TYPE: SequenceType =
    SequenceType.makeSequenceType(
      ArrayItemType.ANY_ARRAY_TYPE,
      StaticProperty.EXACTLY_ONE
    )

  def get(index: Int): GroundedValue
  def put(index: Int, newValue: GroundedValue): ArrayItem
  def arrayLength(): Int
  def isEmpty: Boolean
  def members: Iterable[GroundedValue]
  def concat(other: ArrayItem): ArrayItem
  def remove(index: Int): ArrayItem
  def removeSeveral(positions: IntSet): ArrayItem
  def subArray(start: Int, end: Int): ArrayItem
  def insert(position: Int, member: GroundedValue): ArrayItem
  def getMemberType(th: TypeHierarchy): SequenceType

  /**
   * Provide a short string showing the contents of the item, suitable
   * for use in error messages
   *
   * @return a depiction of the item suitable for use in error messages
   */
  override def toShortString: String = {
    val sb = new StringBuilder
    sb.append("array{")
    var count = 0
    breakable {
      for (member <- members) {
        if ({
          count += 1
          count - 1
        } > 2) {
          sb.append(" ...")
          break()
        }
        sb.append(member.toShortString)
        sb.append(", ")
      }
    }
    sb.append("}")
    sb.toString
  }

  /**
   * Get the genre of this item
   *
   * @return the genre: specifically, { @link Genre#ARRAY}.
   */
  override def getGenre: Genre = Genre.ARRAY
}
