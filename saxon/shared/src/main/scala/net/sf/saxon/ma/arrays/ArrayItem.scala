////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
package net.sf.saxon.ma.arrays

import net.sf.saxon.expr.StaticProperty
import net.sf.saxon.model.TypeHierarchy
import net.sf.saxon.om.Function
import net.sf.saxon.om.Genre
import net.sf.saxon.om.Genre.Genre
import net.sf.saxon.om.GroundedValue
import net.sf.saxon.value.SequenceType
import net.sf.saxon.z.IntSet
import scala.util.control.Breaks._

/**
 * Interface supported by different implementations of an XDM array item
 */
trait ArrayItem extends Function {

  var SINGLE_ARRAY_TYPE: SequenceType = SequenceType.makeSequenceType(
    ArrayItemType.ANY_ARRAY_TYPE,
    StaticProperty.EXACTLY_ONE)

  def get(index: Int): GroundedValue

  def put(index: Int, newValue: GroundedValue): ArrayItem

  def arrayLength(): Int

  def isEmpty: Boolean

  def members(): Iterable[GroundedValue]

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
    val sb: StringBuilder = new StringBuilder()
    sb.append("array{")
    var count: Int = 0
    breakable {
      for (member <- members()) {
        if ( {
          count += 1;
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

// Copyright (c) 2018-2020 Saxonica Limited
// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is "Incompatible With Secondary Licenses", as defined by the Mozilla Public License, v. 2.0.
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// Copyright (c) 2014-2020 Saxonica Limited
