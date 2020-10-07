package org.orbeon.saxon.serialize

import org.orbeon.saxon.om.StructuredQName

import org.orbeon.saxon.serialize.charcode.UTF16CharacterSet

import org.orbeon.saxon.trace.ExpressionPresenter

import org.orbeon.saxon.tree.tiny.CompressedWhitespace

import org.orbeon.saxon.tree.util.FastStringBuffer

import org.orbeon.saxon.value.Whitespace

import org.orbeon.saxon.z.IntHashMap

import org.orbeon.saxon.z.IntIterator

import scala.beans.{BeanProperty, BooleanBeanProperty}
//import scala.collection.compat._
import scala.jdk.CollectionConverters._
import scala.util.control.Breaks._

class CharacterMap(@BeanProperty var name: StructuredQName,
                   map: IntHashMap[String]) {

  private var charMap: IntHashMap[String] = map

  private var min: Int = java.lang.Integer.MAX_VALUE

  private var max: Int = 0

  private var mapsWhitespace: Boolean = false

  init()

  def this(list: java.lang.Iterable[CharacterMap]) = {
    this(null, null)
    charMap = new IntHashMap[String](64)
    for (map <- list.asScala) {
      val keys: IntIterator = map.charMap.keyIterator
      while (keys.hasNext) {
        val next: Int = keys.next
        charMap.put(next, map.charMap.get(next))
      }
    }
    init()
  }

  private def init(): Unit = {
    val keys: IntIterator = charMap.keyIterator
    while (keys.hasNext) {
      val next: Int = keys.next
      if (next < min) {
        min = next
      }
      if (next > max) {
        max = next
      }
      if (!mapsWhitespace && Whitespace.isWhitespace(next).asInstanceOf[Boolean]) {
        mapsWhitespace = true
      }
    }
    if (min > 0xD800) {
      min = 0xD800
    }
  }

  def map(in: CharSequence, insertNulls: Boolean): CharSequence = {
    if (!mapsWhitespace && in.isInstanceOf[CompressedWhitespace]) {
      return in
    }
    var move: Boolean = false
    var i: Int = 0
    breakable {
      while (i < in.length) {
        val c: Char = in.charAt({
          i += 1;
          i - 1
        })
        if (c >= min && c <= max) {
          move = true
          break()
        }
      }
    }
    if (!move) {
      return in
    }
    val buffer: FastStringBuffer = new FastStringBuffer(in.length * 2)
    var k: Int = 0
    while (k < in.length) {
      val c: Char = in.charAt({
        k += 1;
        k - 1
      })
      if (c >= min && c <= max) {
        if (UTF16CharacterSet.isHighSurrogate(c)) {
          val d: Char = in.charAt({
            k += 1;
            k - 1
          })
          val s: Int = UTF16CharacterSet.combinePair(c, d)
          val rep: String = charMap.get(s)
          if (rep == null) {
            buffer.cat(c)
            buffer.cat(d)
          } else {
            if (insertNulls) {
              buffer.cat(0.toChar)
              buffer.append(rep)
              buffer.cat(0.toChar)
            } else {
              buffer.append(rep)
            }
          }
        } else {
          val rep: String = charMap.get(c)
          if (rep == null) {
            buffer.cat(c)
          } else {
            if (insertNulls) {
              buffer.cat(0.toChar)
              buffer.append(rep)
              buffer.cat(0.toChar)
            } else {
              buffer.append(rep)
            }
          }
        }
      } else {
        buffer.cat(c)
      }
    }
    buffer
  }

  def export(out: ExpressionPresenter): Unit = {
    out.startElement("charMap")
    out.emitAttribute("name", name)
    var iter: IntIterator = charMap.keyIterator
    while (iter.hasNext) {
      val c: Int = iter.next
      val s: String = charMap.get(c)
      out.startElement("m")
      out.emitAttribute("c", c.toString)
      out.emitAttribute("s", s)
      out.endElement()
    }
    out.endElement()
  }

}
