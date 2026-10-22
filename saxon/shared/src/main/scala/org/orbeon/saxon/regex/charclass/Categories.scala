////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// Copyright (c) 2018-2020 Saxonica Limited
// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is "Incompatible With Secondary Licenses", as defined by the Mozilla Public License, v. 2.0.
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
package org.orbeon.saxon.regex.charclass

import org.orbeon.saxon.event.Builder
import org.orbeon.saxon.lib.{ParseOptions, Validation}
import org.orbeon.saxon.model.Type
import org.orbeon.saxon.om.{AxisInfo, NodeInfo}
import org.orbeon.saxon.pattern.{NameTest, NodeKindTest}
import org.orbeon.saxon.serialize.charcode.XMLCharacterData
import org.orbeon.saxon.regex.UnicodeData
import org.orbeon.saxon.utils.Configuration

import java.util

//import scala.collection.compat._
import java.util.function.IntPredicate
import java.util.{ArrayList, HashMap}

import javax.xml.transform.stream.StreamSource
import org.orbeon.saxon.tree.tiny.TinyElementImpl
import org.orbeon.saxon.z._

import scala.jdk.CollectionConverters._

/**
 * Data for Regular expression character categories. The data is in an XML file derived from the Unicode
 * database (In Saxon 9.6, this is based on Unicode 6.2.0). Since Saxon 9.4,
 * we no longer make use of Java's support for character categories since there are too many differences
 * from Unicode.
 */
object Categories {

  object Category {

    private def extent(predicate: IntPredicate): IntSet =
      predicate match {
        case intSetPred: IntSetPredicate =>
          intSetPred.getIntSet
        case _ =>
          null
      }
  }

  /**
   * A Category is a CharacterClass represented in a regular expression as \p{Xx}.
   * The label Xx is retained, and can be used to determine whether or not two
   * categories are disjoint.
   */
  class Category(private var label: String,
                 private var predicate: java.util.function.IntPredicate)
    extends CharacterClass {

    def test(value: Int): Boolean = predicate.test(value)

    def isDisjoint(other: CharacterClass): Boolean =
      other match {
        case category: Category =>
          val majorCat0  = label.charAt(0)
          val otherLabel = category.label
          val majorCat1  = otherLabel.charAt(0)
          majorCat0 != majorCat1 ||
            (label.length > 1 && otherLabel.length > 1 && label != otherLabel)
        case _: InverseCharacterClass =>
          other.isDisjoint(this)
        case scClass: SingletonCharacterClass =>
          ! test(scClass.codepoint)
        case _: IntSetCharacterClass =>
          val intSet = other.getIntSet
          if (intSet.size > 100)
            return false
          val ii = intSet.iterator
          while (ii.hasNext)
            if (test(ii.next()))
              return false
          true
        case _ =>
          false
      }

    def getIntSet: IntSet = Category.extent(predicate)
  }

  private var CATEGORIES: util.HashMap[String, Category] = null

  def build(): Unit = {
    CATEGORIES = new util.HashMap(30)
    for (entry <- UnicodeData.categoriesData.split(";")) {
      val colon = entry.indexOf(':')
      val cat = entry.substring(0, colon)
      val rangesStr = entry.substring(colon + 1)
      val irs = new IntRangeSet
      for (rangeStr <- rangesStr.split(",")) {
        val dash = rangeStr.indexOf('-')
        val from = java.lang.Integer.parseInt(rangeStr.substring(0, dash), 16)
        val to = java.lang.Integer.parseInt(rangeStr.substring(dash + 1), 16)
        irs.addRange(from, to)
      }
      CATEGORIES.put(cat, new Category(cat, new IntSetPredicate(irs)))
    }
    val c = "CLMNPSZ"
    for (i <- 0 until c.length) {
      val ch = c.charAt(i)
      var ip: IntPredicate = null
      for ((key, value) <- CATEGORIES.asScala if key.charAt(0) == ch)
        ip = if (ip == null) value else ip.or(value)
      val label = ch.toString
      CATEGORIES.put(label, new Category(label, ip))
    }
  }

  val ESCAPE_s   : CharacterClass          = new IntSetCharacterClass(IntArraySet.make(Array(9, 10, 13, 32), 4))
  val ESCAPE_S   : CharacterClass          = new InverseCharacterClass(ESCAPE_s)
  val ESCAPE_i   : PredicateCharacterClass = new PredicateCharacterClass(value => XMLCharacterData.isNCNameStart11(value) || value == ':')
  val ESCAPE_I   : CharacterClass          = new InverseCharacterClass(ESCAPE_i)
  val ESCAPE_c   : PredicateCharacterClass = new PredicateCharacterClass(value => XMLCharacterData.isNCName11(value) || value == ':')
  val ESCAPE_C   : CharacterClass          = new InverseCharacterClass(ESCAPE_c)
  val ESCAPE_d   : Category                = getCategory("Nd")
  val ESCAPE_D   : CharacterClass          = new InverseCharacterClass(ESCAPE_d)
  val CATEGORY_P : Category                = getCategory("P")
  val CATEGORY_Z : Category                = getCategory("Z")
  val CATEGORY_C : Category                = getCategory("C")
  val ESCAPE_w   : PredicateCharacterClass = new PredicateCharacterClass(value => ! (CATEGORY_P.test(value) || CATEGORY_Z.test(value) || CATEGORY_C.test(value)))

  val ESCAPE_W: CharacterClass = new InverseCharacterClass(ESCAPE_w)

  def getCategory(cat: String): Category = synchronized {
    if (CATEGORIES == null)
      build()
    CATEGORIES.get(cat)
  }
}
