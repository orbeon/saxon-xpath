package net.sf.saxon.serialize.codenorm

import net.sf.saxon.utils.Configuration

import net.sf.saxon.serialize.charcode.UTF16CharacterSet

import net.sf.saxon.trans.XPathException

import net.sf.saxon.tree.util.FastStringBuffer

import Normalizer._

object Normalizer {

  def make(form: Int, config: Configuration): Normalizer = synchronized {
    if (data == null) {
      data = UnicodeDataParserFromXML.build(config)
    }
    new Normalizer(form)
  }

  val COMPATIBILITY_MASK: Int = 1

  val COMPOSITION_MASK: Int = 2

  val D: Int = 0

  val C: Int = COMPOSITION_MASK

  val KD: Int = COMPATIBILITY_MASK

  val KC: Int = (COMPATIBILITY_MASK + COMPOSITION_MASK).toByte

  val NO_ACTION: Int = 8

  private def setCharAt(target: FastStringBuffer,
                        offset: Int,
                        ch32: Int): Unit = {
    if (ch32 < 65536) {
      if (UTF16CharacterSet.isHighSurrogate(target.charAt(offset))) {
        target.setCharAt(offset, ch32.toChar)
        target.removeCharAt(offset + 1)
      } else {
        target.setCharAt(offset, ch32.toChar)
      }
    } else {
      if (UTF16CharacterSet.isHighSurrogate(target.charAt(offset))) {
        target.setCharAt(offset, UTF16CharacterSet.highSurrogate(ch32))
        target.setCharAt(offset + 1, UTF16CharacterSet.lowSurrogate(ch32))
      } else {
        target.setCharAt(offset, UTF16CharacterSet.highSurrogate(ch32))
        target.insert(offset + 1, UTF16CharacterSet.lowSurrogate(ch32))
      }
    }
  }

  private var data: NormalizerData = null

}

class Normalizer private(private var form: Int) {

  def normalize(source: CharSequence): CharSequence = {
    if (form == NO_ACTION || source.length == 0) {
      source
    }
    val target: FastStringBuffer = new FastStringBuffer(source.length + 8)
    internalDecompose(source, target)
    if ((form & COMPOSITION_MASK) != 0) {
      internalCompose(target)
    }
    target
  }

  private def internalDecompose(source: CharSequence,
                                target: FastStringBuffer): Unit = {
    val buffer: FastStringBuffer = new FastStringBuffer(8)
    val canonical: Boolean = (form & COMPATIBILITY_MASK) == 0
    var ch32: Int = 0
    var i: Int = 0
    while (i < source.length) {
      buffer.setLength(0)
      ch32 = source.charAt({
        i += 1; i - 1
      })
      if (ch32 < 128) {
        target.cat(ch32.toChar)
        //continue
      }
      if (UTF16CharacterSet.isHighSurrogate(ch32)) {
        val low: Char = source.charAt({
          i += 1; i - 1
        })
        ch32 = UTF16CharacterSet.combinePair(ch32.toChar, low)
      }
      data.getRecursiveDecomposition(canonical, ch32, buffer)
      var ch: Int = 0
      var j: Int = 0
      while (j < buffer.length) {
        ch = buffer.charAt({
          j += 1; j - 1
        })
        if (UTF16CharacterSet.isHighSurrogate(ch)) {
          val low: Char = buffer.charAt({
            j += 1; j - 1
          })
          ch = UTF16CharacterSet.combinePair(ch.toChar, low)
        }
        val chClass: Int = data.getCanonicalClass(ch)
        var k: Int = target.length
        if (chClass != 0) {
          var ch2: Int = 0
          while (k > 0) {
            var step: Int = 1
            ch2 = target.charAt(k - 1)
            if (UTF16CharacterSet.isSurrogate(ch2)) {
              step = 2
              val high: Char = target.charAt(k - 2)
              ch2 = UTF16CharacterSet.combinePair(high, ch2.toChar)
            }
            if (data.getCanonicalClass(ch2) <= chClass) //break
            k -= step
          }
        }
        if (ch < 65536) {
          target.insert(k, ch.toChar)
        } else {
          target.insertWideChar(k, ch)
        }
      }
    }
  }

  private def internalCompose(target: FastStringBuffer): Unit = {
    var starterPos: Int = 0
    var starterCh: Int = target.charAt(0)
    var compPos: Int = 1
    if (UTF16CharacterSet.isHighSurrogate(starterCh)) {
      starterCh = UTF16CharacterSet.combinePair(starterCh.toChar, target.charAt(1))
      compPos += 1
    }
    var lastClass: Int = data.getCanonicalClass(starterCh)
    if (lastClass != 0) lastClass = 256
    var oldLen: Int = target.length
    var ch: Int = 0
    var decompPos: Int = compPos
    while (decompPos < target.length) {
      ch = target.charAt({
        decompPos += 1; decompPos - 1
      })
      if (UTF16CharacterSet.isHighSurrogate(ch)) {
        ch = UTF16CharacterSet.combinePair(ch.toChar, target.charAt({
          decompPos += 1;
          decompPos - 1
        }))
      }
      val chClass: Int = data.getCanonicalClass(ch)
      val composite: Int = data.getPairwiseComposition(starterCh, ch)
      if (composite != NormalizerData.NOT_COMPOSITE && (lastClass < chClass || lastClass == 0)) {
        setCharAt(target, starterPos, composite)
        starterCh = composite
      } else {
        if (chClass == 0) {
          starterPos = compPos
          starterCh = ch
        }
        lastClass = chClass
        setCharAt(target, compPos, ch)
        if (target.length != oldLen) {
          decompPos += target.length - oldLen
          oldLen = target.length
        }
        compPos += (if (ch < 65536) 1 else 2)
      }
    }
    target.setLength(compPos)
  }

}
