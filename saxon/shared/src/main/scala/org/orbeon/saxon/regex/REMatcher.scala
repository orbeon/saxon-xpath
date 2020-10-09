package org.orbeon.saxon.regex

import java.util

import org.orbeon.saxon.tree.util.FastStringBuffer

import scala.util.control.Breaks._
//import scala.collection.compat._
import org.orbeon.saxon.regex.REMatcher.State

import scala.jdk.CollectionConverters._

object REMatcher {
  private[regex] val MAX_PAREN = 16

  class State() {
    private[regex] var parenCount = 0
    private[regex] var startn: Array[Int] = null
    private[regex] var endn: Array[Int] = null
    parenCount = 0
    startn = new Array[Int](3)
    startn(2) = -1
    startn(1) = startn(2)
    startn(0) = startn(1)
    endn = new Array[Int](3)
    endn(2) = -1
    endn(1) = endn(2)
    endn(0) = endn(1)

    def this(s: REMatcher.State) {
      this()
      parenCount = s.parenCount
      startn = util.Arrays.copyOf(s.startn, s.startn.length)
      endn = util.Arrays.copyOf(s.endn, s.endn.length)
    }
  }

}


class REMatcher(val progrm: REProgram) {

  setProgram(progrm)

  var program: REProgram = _

  var search: UnicodeString = _

  var history: History = new History()

  var maxParen: Int = REMatcher.MAX_PAREN

  var captureState: State = new State()

  var startBackref: Array[Int] = _

  var endBackref: Array[Int] = _

  var operation: Operation = _

  var anchoredMatch: Boolean = _

  def setProgram(program: REProgram): Unit = {
    this.program = program
    if (program != null && program.maxParens != -1) {
      this.operation = program.operation
      this.maxParen = program.maxParens
    }
    else this.maxParen = REMatcher.MAX_PAREN
  }


  def getProgram = program


  def getParenCount = captureState.parenCount


  def getParen(which: Int): UnicodeString = {
    var start: Int = 0
    start = getParenStart(which)
    if (which < captureState.parenCount && start >= 0) return search.uSubstring(start, getParenEnd(which))
    null
  }


  final def getParenStart(which: Int): Int = {
    if (which < captureState.startn.length) return captureState.startn(which)
    -1
  }


  final def getParenEnd(which: Int): Int = {
    if (which < captureState.endn.length) return captureState.endn(which)
    -1
  }


  final def setParenStart(which: Int, i: Int) = {
    while ( {
      which > captureState.startn.length - 1
    }) {
      val s2 = new Array[Int](captureState.startn.length * 2)
      System.arraycopy(captureState.startn, 0, s2, 0, captureState.startn.length)
      util.Arrays.fill(s2, captureState.startn.length, s2.length, -1)
      captureState.startn = s2
    }
    captureState.startn(which) = i
  }


  final def setParenEnd(which: Int, i: Int) = {
    while ( {
      which > captureState.endn.length - 1
    }) {
      val e2 = new Array[Int](captureState.endn.length * 2)
      System.arraycopy(captureState.endn, 0, e2, 0, captureState.endn.length)
      util.Arrays.fill(e2, captureState.endn.length, e2.length, -1)
      captureState.endn = e2
    }
    captureState.endn(which) = i
  }


  def clearCapturedGroupsBeyond(pos: Int) = {
    for (i <- 0 until captureState.startn.length) {
      if (captureState.startn(i) >= pos) captureState.endn(i) = captureState.startn(i)
    }
    if (startBackref != null) for (i <- 0 until startBackref.length) {
      if (startBackref(i) >= pos) endBackref(i) = startBackref(i)
    }
  }


  def matchAt(i: Int, anchored: Boolean): Boolean = {
    captureState.parenCount = 1
    anchoredMatch = anchored
    setParenStart(0, i)

    if ((program.optimizationFlags & REProgram.OPT_HASBACKREFS) != 0) {
      startBackref = new Array[Int](maxParen)
      endBackref = new Array[Int](maxParen)
    }

    var idx = 0
    val iter = operation.iterateMatches(this, i)
    if (iter.hasNext) {
      idx = iter.next
      setParenEnd(0, idx)
      return true
    }

    captureState.parenCount = 0
    false
  }


  def anchoredMatch(search: UnicodeString) = {
    this.search = search
    matchAt(0, anchored = true)
  }


  def `match`(search: UnicodeString, i: Int): Boolean = {
    var z = i
    this.search = search

    captureState = new REMatcher.State

    if ((program.optimizationFlags & REProgram.OPT_HASBOL).equals(REProgram.OPT_HASBOL)) {
      if (!program.flags.isMultiLine) return z == 0 && checkPreconditions(z) && matchAt(z, anchored = false)

      var nl = z
      if (matchAt(nl, anchored = false)) return true
      while ( {
        true
      }) {
        nl = search.uIndexOf('\n', nl) + 1
        if (nl >= search.uLength || nl <= 0) return false
        else if (matchAt(nl, anchored = false)) return true
      }
    }

    val actualLength = search.uLength - z
    if (actualLength < program.minimumLength) return false

    if (program.prefix == null) {
      if (program.initialCharClass != null) {
        val pred = program.initialCharClass
        while ( {
          !search.isEnd(i)
        }) {
          if (pred.test(search.uCharAt(i))) if (matchAt(i, anchored = false)) return true
          z += 1
        }
        return false
      }

      if (!checkPreconditions(z)) return false

      while ( {
        !search.isEnd(z - 1)
      }) {
        if (matchAt(i, anchored = false)) return true
        z += 1
      }
      false
    }
    else {
      val prefix = program.prefix
      val prefixLength = prefix.uLength
      val ignoreCase = program.flags.isCaseIndependent
      while ( {
        !search.isEnd(i + prefixLength - 1)
      }) {
        var prefixOK = true
        if (ignoreCase) {
          var j = z
          var k = 0
          breakable {
            while (k < prefixLength) {
              if (!equalCaseBlind(search.uCharAt(j), prefix.uCharAt(k))) {
                prefixOK = false
                break()
              }
              j += 1
              k += 1
            }
          }
        }
        else {
          var j = z
          var k = 0
          breakable {
            while (k < prefixLength) {
              if (search.uCharAt(j) != prefix.uCharAt(k)) {
                prefixOK = false
                break()
              }
              j += 1
              k += 1
            }
          }
        }

        if (prefixOK) {
          if (matchAt(z, anchored = false)) return true
        }
        z += 1
      }
      false
    }
  }


  private def checkPreconditions(start: Int): Boolean = {

    for (condition <- program.preconditions.asScala) {
      if (condition.fixedPosition != -1) {
        val `match` = condition.operation.iterateMatches(this, condition.fixedPosition).hasNext
        if (!`match`) return false
      }
      else {
        var i = start
        if (i < condition.minPosition) i = condition.minPosition
        var found = false
        breakable {
          while (!search.isEnd(i)) {
            if ((condition.fixedPosition == -1 || condition.fixedPosition == i) && condition.operation.iterateMatches(this, i).hasNext) {
              found = true
              break()
            }
            i += 1
          }
        }
        if (!found) return false
      }
    }
    true
  }


  def `match`(search: String): Boolean = {
    val uString = UnicodeString.makeUnicodeString(search)
    `match`(uString, 0)
  }


  def split(s: UnicodeString): util.List[UnicodeString] = {
    val v = new util.ArrayList[UnicodeString]

    var pos = 0
    val len = s.uLength

    while ( {
      pos < len && `match`(s, pos)
    }) {
      val start = getParenStart(0)

      var newpos = getParenEnd(0)

      if (newpos == pos) {
        v.add(s.uSubstring(pos, start + 1))
        newpos += 1
      }
      else v.add(s.uSubstring(pos, start))

      pos = newpos
    }

    val remainder = s.uSubstring(pos, len)
    v.add(remainder)

    v
  }


  def replace(in: UnicodeString, replacement: UnicodeString): FastStringBuffer = {
    val sb = new FastStringBuffer(in.uLength * 2)
    var pos = 0
    val len = in.uLength
    while ( {
      pos < len && `match`(in, pos)
    }) {
      for (i <- pos until getParenStart(0)) {
        sb.appendWideChar(in.uCharAt(i))
      }
      if (!program.flags.isLiteral) {
        val maxCapture = program.maxParens - 1
        var i = 0
        while ( {
          i < replacement.uLength
        }) {
          var ch = replacement.uCharAt(i)
          if (ch == '\\') {
            ch = replacement.uCharAt({
              i += 1;
              i
            })
            if (ch == '\\' || ch == '$') sb.cat(ch.toChar)
            else throw new RESyntaxException("Invalid escape '" + ch + "' in replacement string")
          }
          else if (ch == '$') {
            ch = replacement.uCharAt({
              i += 1;
              i
            })
            if (!(ch >= '0' && ch <= '9')) throw new RESyntaxException("$ in replacement string must be followed by a digit")
            var n = ch - '0'
            if (maxCapture <= 9) if (maxCapture >= n) {
              val captured = getParen(n)
              if (captured != null) for (j <- 0 until captured.uLength) {
                sb.appendWideChar(captured.uCharAt(j))
              }
            }
            else {

            }
            else {
              breakable {
                while (true) {
                  if ( {
                    i += 1;
                    i
                  } >= replacement.uLength) break()
                  ch = replacement.uCharAt(i)
                  if (ch >= '0' && ch <= '9') {
                    val m = n * 10 + (ch - '0')
                    if (m > maxCapture) {
                      i -= 1
                      break()
                    }
                    else n = m
                  }
                  else {
                    i -= 1
                    break()
                  }
                }
              }
              val captured = getParen(n)
              if (captured != null) for (j <- 0 until captured.uLength) {
                sb.appendWideChar(captured.uCharAt(j))
              }
            }
          }
          else sb.appendWideChar(ch)
          i += 1
        }
      }
      else {
        for (i <- 0 until replacement.uLength) {
          sb.appendWideChar(replacement.uCharAt(i))
        }
      }

      var newpos = getParenEnd(0)

      if (newpos == pos) newpos += 1

      pos = newpos
    }

    for (i <- pos until len) {
      sb.appendWideChar(in.uCharAt(i))
    }

    sb.condense
  }


  def replaceWith(in: UnicodeString, replacer: CharSequence => CharSequence) = {
    val sb = new FastStringBuffer(in.uLength * 2)
    var pos = 0
    val len = in.uLength
    while ( {
      pos < len && `match`(in, pos)
    }) {
      for (i <- pos until getParenStart(0)) {
        sb.appendWideChar(in.uCharAt(i))
      }
      val matchingSubstring = in.subSequence(getParenStart(0), getParenEnd(0))
      val replacement = replacer(matchingSubstring)
      sb.append(replacement)
      var newpos = getParenEnd(0)
      if (newpos == pos) newpos += 1
      pos = newpos
    }
    for (i <- pos until len) {
      sb.appendWideChar(in.uCharAt(i))
    }
    sb.condense()
  }


  private[regex] def isNewline(i: Int) = search.uCharAt(i) == '\n'


  private[regex] def equalCaseBlind(c1: Int, c2: Int): Boolean = {
    if (c1 == c2) return true
    for (v <- CaseVariants.getCaseVariants(c2)) {
      if (c1 == v) return true
    }
    false
  }

  def getCaptureState: State = new REMatcher.State(captureState)

  def resetState(state: REMatcher.State): Unit = captureState = new REMatcher.State(state)
}