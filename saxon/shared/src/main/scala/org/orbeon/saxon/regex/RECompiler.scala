package org.orbeon.saxon.regex


import org.orbeon.saxon.regex.charclass._
import org.orbeon.saxon.tree.util.FastStringBuffer
import org.orbeon.saxon.value.Whitespace
import org.orbeon.saxon.z._

import java.util.function.IntPredicate
import java.{util => ju}
import scala.util.control.Breaks
import scala.util.control.Breaks._


object RECompiler {

  val NODE_NORMAL = 0
  val NODE_TOPLEVEL = 2

  private val TRACING = false

  def trace(base: Operation): Operation =
    if (TRACING && !base.isInstanceOf[Operation.OpTrace])
      new Operation.OpTrace(base)
    else
      base

  private def isAsciiDigit(ch: Int) = ch >= '0' && ch <= '9'

  def makeUnion(p1: CharacterClass, p2: CharacterClass): CharacterClass = {
    if (p1 eq EmptyCharacterClass.getInstance)
      return p2
    if (p2 eq EmptyCharacterClass.getInstance)
      return p1
    val is1 = p1.getIntSet
    val is2 = p2.getIntSet
    if (is1 == null || is2 == null)
      new PredicateCharacterClass(p1.or(p2))
    else
      new IntSetCharacterClass(is1.union(is2))
  }

  def makeDifference(p1: CharacterClass, p2: CharacterClass): CharacterClass = {
    if (p1 eq EmptyCharacterClass.getInstance)
      return p1
    if (p2 eq EmptyCharacterClass.getInstance)
      return p1
    val is1 = p1.getIntSet
    val is2 = p2.getIntSet
    if (is1 == null || is2 == null)
      new PredicateCharacterClass(new IntExceptPredicate(p1, p2))
    else
      new IntSetCharacterClass(is1.except(is2))
  }

  def makeComplement(p1: CharacterClass): CharacterClass = p1 match {
    case clazz: InverseCharacterClass => clazz.getComplement
    case _                            => new InverseCharacterClass(p1)
  }

  private def makeSequence(o1: Operation, o2: Operation): Operation = o1 match {
    case opSequence1: Operation.OpSequence =>
      o2 match {
        case opSequence2: Operation.OpSequence =>
          val l1 = opSequence1.getOperations
          val l2 = opSequence2.getOperations
          l1.addAll(l2)
          return o1
        case _                                 =>
      }
      val l1 = opSequence1.getOperations
      l1.add(o2)
      o1
    case _ =>
      o2 match {
      case opSequence2: Operation.OpSequence =>
        val l2 = opSequence2.getOperations
        l2.add(0, o1)
        o2
      case _ =>
        val list = new ju.ArrayList[Operation](4)
        list.add(o1)
        list.add(o2)
        trace(new Operation.OpSequence(list))
    }
  }


  def noAmbiguity(op0: Operation, op1: Operation, caseBlind: Boolean, reluctant: Boolean): Boolean = {
    if (op1.isInstanceOf[Operation.OpEndProgram]) return !reluctant
    if (op1.isInstanceOf[Operation.OpBOL] || op1.isInstanceOf[Operation.OpEOL]) return true
    if (op1.isInstanceOf[Operation.OpRepeat] && op1.asInstanceOf[Operation.OpRepeat].min == 0) return false
    val c0 = op0.getInitialCharacterClass(caseBlind)
    val c1 = op1.getInitialCharacterClass(caseBlind)
    c0.isDisjoint(c1)
  }
}


class RECompiler {

  var pattern: UnicodeString = _
  var len: Int = _
  var idx: Int = _
  var capturingOpenParenCount: Int = _
  var bracketMin: Int = _
  var bracketMax: Int = _
  var isXPath: Boolean = true
  var isXPath30: Boolean = true
  var isXSD11: Boolean = false
  var captures: IntHashSet = new IntHashSet()
  var hasBackReferences: Boolean = false
  var reFlags: REFlags = _
  var warnings: ju.List[String] = _

  def setFlags(flags: REFlags): Unit = {
    this.reFlags = flags
    isXPath = flags.isAllowsXPath20Extensions
    isXPath30 = flags.isAllowsXPath30Extensions
    isXSD11 = flags.isAllowsXSD11Syntax
  }

  def setWarning(s: String): Unit = {
    if (warnings == null) warnings = new ju.ArrayList[String](4)
    warnings.add(s)
  }

  def getWarnings: ju.List[String] =
    if (warnings == null)
      ju.Collections.emptyList[String]
    else
      warnings

  @throws[Error]
  def internalError(): Unit = throw new Error("Internal error!")

  @throws[RESyntaxException]
  def syntaxError(s: String): Unit = throw new RESyntaxException(s, idx)

  @throws[RESyntaxException]
  def bracket(): Unit = {
    if (idx >= len || pattern.uCharAt({
      idx += 1
      idx - 1
    }) != '{') internalError()

    if (idx >= len || !RECompiler.isAsciiDigit(pattern.uCharAt(idx))) syntaxError("Expected digit")

    val number = new FastStringBuffer(16)
    while ({
      idx < len && RECompiler.isAsciiDigit(pattern.uCharAt(idx))
    }) number.cat(pattern.uCharAt({
      idx += 1
      idx - 1
    }).toChar)
    try
      bracketMin = number.toString.toInt
    catch {
      case _: NumberFormatException =>
        syntaxError("Expected valid number")
    }

    if (idx >= len)
      syntaxError("Expected comma or right bracket")

    if (pattern.uCharAt(idx) == '}') {
      idx += 1
      bracketMax = bracketMin
      return
    }

    if (idx >= len || pattern.uCharAt({
      idx += 1
      idx - 1
    }) != ',') syntaxError("Expected comma")
    if (idx >= len) syntaxError("Expected comma or right bracket")

    if (pattern.uCharAt(idx) == '}') {
      idx += 1
      bracketMax = Integer.MAX_VALUE
      return
    }
    if (idx >= len || !RECompiler.isAsciiDigit(pattern.uCharAt(idx)))
      syntaxError("Expected digit")

    number.setLength(0)
    while ({
      idx < len && RECompiler.isAsciiDigit(pattern.uCharAt(idx))
    }) number.cat(pattern.uCharAt({
      idx += 1
      idx - 1
    }).toChar)
    try bracketMax = number.toString.toInt
    catch {
      case e: NumberFormatException =>
        syntaxError("Expected valid number")
    }

    if (bracketMax < bracketMin)
      syntaxError("Bad range")

    if (idx >= len || pattern.uCharAt({
      idx += 1
      idx - 1
    }) != '}')
      syntaxError("Missing close brace")
  }


  @throws[RESyntaxException]
  def escape(inSquareBrackets: Boolean): CharacterClass = {
    if (pattern.uCharAt(idx) != '\\')
      internalError()

    if (idx + 1 == len)
      syntaxError("Escape terminates string")

    idx += 2
    val escapeChar = pattern.uCharAt(idx - 1)
    escapeChar match {
      case 'n' =>
        return new SingletonCharacterClass('\n')
      case 'r' =>
        return new SingletonCharacterClass('\r')
      case 't' =>
        return new SingletonCharacterClass('\t')
      case '\\' | '|' | '.' | '-' | '^' | '?' |
           '*' | '+' | '{' | '}' | '(' | ')' | '[' | ']' =>
        return new SingletonCharacterClass(escapeChar)
      case '$' =>
        if (isXPath) return new SingletonCharacterClass(escapeChar)
        else syntaxError("In XSD, '$' must not be escaped")
      case 's' =>
        return Categories.ESCAPE_s
      case 'S' =>
        return Categories.ESCAPE_S
      case 'i' =>
        return Categories.ESCAPE_i
      case 'I' =>
        return Categories.ESCAPE_I
      case 'c' =>
        return Categories.ESCAPE_c
      case 'C' =>
        return Categories.ESCAPE_C
      case 'd' =>
        return Categories.ESCAPE_d
      case 'D' =>
        return Categories.ESCAPE_D
      case 'w' =>
        return Categories.ESCAPE_w
      case 'W' =>
        return Categories.ESCAPE_W
      case 'p' | 'P' =>
        if (idx == len)
          syntaxError("Expected '{' after \\" + escapeChar)
        if (pattern.uCharAt(idx) != '{')
          syntaxError("Expected '{' after \\" + escapeChar)
        val close = pattern.uIndexOf('}', {
          idx += 1
          idx - 1
        })
        if (close == -1) syntaxError("No closing '}' after \\" + escapeChar)
        val block = pattern.uSubstring(idx, close)
        if (block.uLength == 1 || block.uLength == 2) {
          val primary = Categories.getCategory(block.toString)
          if (primary == null) syntaxError("Unknown character category " + block.toString)
          idx = close + 1
          if (escapeChar == 'p') return primary
          else return RECompiler.makeComplement(primary)
        }
        else if (block.toString.startsWith("Is")) {
          val blockName = block.toString.substring(2)
          val uniBlock = UnicodeBlocks.getBlock(blockName)
          if (uniBlock == null) {
            if (reFlags.isAllowUnknownBlockNames) {
              setWarning("Unknown Unicode block: " + blockName)
              idx = close + 1
              return EmptyCharacterClass.getComplement
            }
            else syntaxError("Unknown Unicode block: " + blockName)
          }
          idx = close + 1
          val primary = new IntSetCharacterClass(uniBlock)
          if (escapeChar == 'p') return primary
          else return RECompiler.makeComplement(primary)
        }
        else syntaxError("Unknown character category: " + block)
      case '0' =>
        syntaxError("Octal escapes not allowed")
      case '1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9' =>
        if (inSquareBrackets) syntaxError("Backreference not allowed within character class")
        else if (isXPath) {
          var backRef = escapeChar - '0'
          breakable {
            while (idx < len) {
              val c1 = "0123456789".indexOf(pattern.uCharAt(idx))
              if (c1 < 0) break()
              else {
                val backRef2 = backRef * 10 + c1
                if (backRef2 > (capturingOpenParenCount - 1)) break()
                else {
                  backRef = backRef2
                  idx += 1
                }
              }
            }
          }
          if (!captures.contains(backRef)) {
            val explanation = if (backRef > (capturingOpenParenCount - 1)) "(no such group)"
            else "(group not yet closed)"
            syntaxError("invalid backreference \\" + backRef + " " + explanation)
          }
          hasBackReferences = true
          return new BackReference(backRef)
        }
        else syntaxError("digit not allowed after \\")
      case _ =>

        syntaxError("Escape character '" + escapeChar.toChar + "' not allowed")
    }
    null
  }


  class BackReference(val number: Int) extends SingletonCharacterClass(number) {
  }


  @throws[RESyntaxException]
  def parseCharacterClass : CharacterClass = {
    if (pattern.uCharAt(idx) != '[') internalError()

    if ((idx + 1) >= len || pattern.uCharAt({
      idx += 1
      idx
    }) == ']') syntaxError("Missing ']'")

    var simpleChar: Int = 0

    var positive: Boolean = true

    var definingRange: Boolean = false

    var rangeStart: Int = -1

    var rangeEnd: Int = 0

    var range: IntRangeSet = new IntRangeSet()

    var addend: CharacterClass = null

    var subtrahend: CharacterClass = null

    if (thereFollows("^")) if (thereFollows("^-[")) syntaxError("Nothing before subtraction operator")
    else if (thereFollows("^]")) syntaxError("Empty negative character group")
    else {
      positive = false
      idx += 1
    }
    else if (thereFollows("-[")) syntaxError("Nothing before subtraction operator")
    while (idx < len && pattern.uCharAt(idx) != ']') {
      val ch = pattern.uCharAt(idx)
      simpleChar = -1
      ch match {
        case '[' =>
          syntaxError("Unescaped '[' within square brackets")
        case '\\' =>

          val cc = escape(true)
          if (cc.isInstanceOf[SingletonCharacterClass])
            simpleChar = cc.asInstanceOf[SingletonCharacterClass].getCodepoint
          else {
            if (definingRange) syntaxError("Multi-character escape cannot follow '-'")
            else if (addend == null) addend = cc
            else addend = RECompiler.makeUnion(addend, cc)
          }
        case '-' =>
          if (thereFollows("-[")) {
            idx += 1
            subtrahend = parseCharacterClass
            if (!thereFollows("]")) syntaxError("Expected closing ']' after subtraction")
          }
          else if (thereFollows("-]")) {
            simpleChar = '-'
            idx += 1
          }
          else if (rangeStart >= 0) {
            definingRange = true
            idx += 1
          }
          else if (definingRange) syntaxError("Bad range")
          else if (thereFollows("--") && !thereFollows("--[")) syntaxError("Unescaped hyphen as start of range")
          else if (!isXSD11 && pattern.uCharAt(idx - 1) != '[' && pattern.uCharAt(idx - 1) != '^' && !thereFollows("]") && !thereFollows("-[")) syntaxError("In XSD 1.0, hyphen is allowed only at the beginning or end of a positive character group")
          else {
            simpleChar = '-'
            idx += 1
          }
        case _ =>
          simpleChar = ch
          idx += 1
      }

      if (definingRange) {
        rangeEnd = simpleChar

        if (rangeStart > rangeEnd) {
          syntaxError("Bad character range: start > end")


        }
        range.addRange(rangeStart, rangeEnd)
        if (reFlags.isCaseIndependent) {
          if (rangeStart == 'a' && rangeEnd == 'z') {
            range.addRange('A', 'Z')
            for (v <- CaseVariants.ROMAN_VARIANTS.indices) {
              range.add(CaseVariants.ROMAN_VARIANTS(v))
            }
          }
          else if (rangeStart == 'A' && rangeEnd == 'Z') {
            range.addRange('a', 'z')
            for (v <- CaseVariants.ROMAN_VARIANTS.indices) {
              range.add(CaseVariants.ROMAN_VARIANTS(v))
            }
          }
          else for (k <- rangeStart to rangeEnd) {
            val variants = CaseVariants.getCaseVariants(k)
            for (variant <- variants) {
              range.add(variant)
            }
          }
        }

        definingRange = false
        rangeStart = -1
      }
      else {
        if (thereFollows("-")) if (thereFollows("-[")) range.add(simpleChar)
        else if (thereFollows("-]")) range.add(simpleChar)
        else if (thereFollows("--[")) range.add(simpleChar)
        else if (thereFollows("--")) syntaxError("Unescaped hyphen cannot act as end of range")
        else rangeStart = simpleChar
        else {
          range.add(simpleChar)
          if (reFlags.isCaseIndependent) {
            val variants = CaseVariants.getCaseVariants(simpleChar)
            for (variant <- variants) {
              range.add(variant)
            }
          }
        }
      }
    }

    if (idx == len) syntaxError("Unterminated character class")
    idx += 1
    var result: CharacterClass = new IntSetCharacterClass(range)
    if (addend != null) result = RECompiler.makeUnion(result, addend)
    if (!positive) result = RECompiler.makeComplement(result)
    if (subtrahend != null) result = RECompiler.makeDifference(result, subtrahend)
    result
  }


  private def thereFollows(s: String) = idx + s.length <= len && pattern.uSubstring(idx, idx + s.length).toString == s


  @throws[RESyntaxException]
  def parseAtom: Operation = {
    var lenAtom = 0
    val fsb = new FastStringBuffer(FastStringBuffer.C64)
    val atomLoop = new Breaks
    atomLoop.breakable {
      while (idx < len) {
        if ((idx + 1) < len) {
          var c = pattern.uCharAt(idx + 1)
          if (pattern.uCharAt(idx) == '\\') {
            val idxEscape = idx
            escape(false)
            if (idx < len) c = pattern.uCharAt(idx)
            idx = idxEscape
          }
          c match {
            case '{' | '?' | '*' | '+' =>
              if (lenAtom != 0) atomLoop.break()
          }
        }

        pattern.uCharAt(idx) match {
          case ']' | '.' | '[' | '(' | ')' | '|' =>
            atomLoop.break()
          case '{' | '?' | '*' | '+' =>
            if (lenAtom == 0) {
              syntaxError("No expression before quantifier")
            }
            atomLoop.break()
          case '}' =>
            syntaxError("Unescaped right curly brace")
            atomLoop.break()
          case '\\' =>
            val idxBeforeEscape = idx
            val charClass = escape(false)
            if (charClass.isInstanceOf[RECompiler#BackReference] || !charClass.isInstanceOf[IntValuePredicate]) {
              idx = idxBeforeEscape
              atomLoop.break()
            }
            fsb.appendWideChar(charClass.asInstanceOf[IntValuePredicate].getTarget)
            lenAtom += 1
          case '^' | '$' =>
            if (isXPath) atomLoop.break()
          case _ =>
            fsb.appendWideChar(pattern.uCharAt({
              idx += 1
              idx - 1
            }))
            lenAtom += 1
        }
      }
    }

    if (fsb.isEmpty)
      internalError()

    RECompiler.trace(new Operation.OpAtom(UnicodeString.makeUnicodeString(fsb.condense)))
  }


  @throws[RESyntaxException]
  def parseTerminal(flags: Array[Int]): Operation = {
    pattern.uCharAt(idx) match {
      case '$' =>
        if (isXPath) {
          idx += 1
          return RECompiler.trace(new Operation.OpEOL)
        }
      case '^' =>
        if (isXPath) {
          idx += 1
          return RECompiler.trace(new Operation.OpBOL)
        }
      case '.' =>
        idx += 1
        var predicate: IntPredicate = null
        if (reFlags.isSingleLine)
          predicate = IntSetPredicate.ALWAYS_TRUE
        else
          predicate = (value: Int) => value != '\n' && value != '\r'
        return RECompiler.trace(new Operation.OpCharClass(predicate))
      case '[' =>
        val range = parseCharacterClass
        return RECompiler.trace(new Operation.OpCharClass(range))
      case '(' =>
        return parseExpr(flags)
      case ')' =>
        syntaxError("Unexpected closing ')'")
      case '|' =>
        internalError()
      case ']' =>
        syntaxError("Unexpected closing ']'")
      case 0 =>
        syntaxError("Unexpected end of input")
      case '?' | '+' |'{' |'*' =>
        syntaxError("No expression before quantifier")
      case '\\' =>
        val idxBeforeEscape = idx
        val esc = escape(false)
        esc match {
          case reference: RECompiler#BackReference =>
            val backreference = reference.getCodepoint
            if (capturingOpenParenCount <= backreference)
              syntaxError("Bad backreference")
            RECompiler.trace(new Operation.OpBackReference(backreference))
          case _: IntSingletonSet =>
            idx = idxBeforeEscape
          case _ =>
            RECompiler.trace(new Operation.OpCharClass(esc))
        }
      case _ =>
    }
    // Default/fall-through
    parseAtom
  }

  @throws[RESyntaxException]
  def piece(flags: Array[Int]): Operation = {
    val terminalFlags = Array(RECompiler.NODE_NORMAL)

    val ret = parseTerminal(terminalFlags)

    flags(0) |= terminalFlags(0)

    if (idx >= len) return ret
    var greedy = true
    var quantifierType = pattern.uCharAt(idx)
    quantifierType match {
      case '?' | '*' | '+' =>
        idx += 1
      case '{' =>
        if (quantifierType == '{') bracket()
        if (ret.isInstanceOf[Operation.OpBOL] || ret.isInstanceOf[Operation.OpEOL]) {
          if (quantifierType == '?' || quantifierType == '*' || (quantifierType == '{' && bracketMin == 0)) return new Operation.OpNothing
          else quantifierType = 0
        }
        if (ret.matchesEmptyString == Operation.MATCHES_ZLS_ANYWHERE) if (quantifierType == '?') {
          quantifierType = 0
        }
        else if (quantifierType == '+') {
          quantifierType = '*'
        }
        else if (quantifierType == '{') {
          quantifierType = '*'
        }
    }

    if (idx < len && pattern.uCharAt(idx) == '?') {
      if (!isXPath) syntaxError("Reluctant quantifiers are not allowed in XSD")
      idx += 1
      greedy = false
    }
    var min = 1
    var max = 1
    quantifierType match {
      case '{' =>
        min = this.bracketMin
        max = this.bracketMax
      case '?' =>
        min = 0
        max = 1
      case '+' =>
        min = 1
        max = Integer.MAX_VALUE
      case '*' =>
        min = 0
        max = Integer.MAX_VALUE
    }
    var result: Operation = null
    if (max == 0) result = new Operation.OpNothing
    else if (min == 1 && max == 1) return ret
    else if (greedy) {
      if (ret.getMatchLength == -1) result = RECompiler.trace(new Operation.OpRepeat(ret, min, max, true))
      else result = new Operation.OpGreedyFixed(ret, min, max, ret.getMatchLength)
    }
    else if (ret.getMatchLength == -1) result = new Operation.OpRepeat(ret, min, max, false)
    else result = new Operation.OpReluctantFixed(ret, min, max, ret.getMatchLength)
    RECompiler.trace(result)
  }


  @throws[RESyntaxException]
  def parseBranch: Operation = {
    var current: Operation = null
    val quantifierFlags = new Array[Int](1)
    while ( {
      idx < len && pattern.uCharAt(idx) != '|' && pattern.uCharAt(idx) != ')'
    }) {
      quantifierFlags(0) = RECompiler.NODE_NORMAL
      val op = piece(quantifierFlags)
      if (current == null) current = op
      else current = RECompiler.makeSequence(current, op)
    }

    if (current == null) return new Operation.OpNothing
    current
  }


  @throws[RESyntaxException]
  private def parseExpr(compilerFlags: Array[Int]) = {
    var paren = -1
    var group = 0
    val branches = new ju.ArrayList[Operation]
    val closeParens = capturingOpenParenCount
    var capturing = true
    if ((compilerFlags(0) & RECompiler.NODE_TOPLEVEL) == 0 && pattern.uCharAt(idx) == '(') {
      if (idx + 2 < len && pattern.uCharAt(idx + 1) == '?' && pattern.uCharAt(idx + 2) == ':') {
        if (!isXPath30) syntaxError("Non-capturing groups allowed only in XPath3.0")
        paren = 2
        idx += 3
        capturing = false
      }
      else {
        paren = 1
        idx += 1
        group = capturingOpenParenCount
        capturingOpenParenCount += 1
      }
    }
    compilerFlags(0) &= ~RECompiler.NODE_TOPLEVEL
    branches.add(parseBranch)
    while (idx < len && pattern.uCharAt(idx) == '|') {
      idx += 1
      branches.add(parseBranch)
    }
    var op: Operation = null
    if (branches.size == 1) op = branches.get(0)
    else op = new Operation.OpChoice(branches)

    if (paren > 0) {
      if (idx < len && pattern.uCharAt(idx) == ')') idx += 1
      else syntaxError("Missing close paren")
      if (capturing) {
        op = new Operation.OpCapture(op, group)
        captures.add(closeParens)
      }
      else {

      }
    }
    else op = RECompiler.makeSequence(op, new Operation.OpEndProgram)

    op
  }


  @throws[RESyntaxException]
  def compile(pattern: UnicodeString): REProgram = {
    var patternStr = pattern
    this.pattern = patternStr
    len = patternStr.uLength
    idx = 0
    capturingOpenParenCount = 1
    if (reFlags.isLiteral) {

      val ret = new Operation.OpAtom(this.pattern)
      val endNode = new Operation.OpEndProgram
      val seq = RECompiler.makeSequence(ret, endNode)
      new REProgram(seq, capturingOpenParenCount, reFlags)
    }
    else {
      if (reFlags.isAllowWhitespace) {

        val sb = new FastStringBuffer(patternStr.uLength)
        var nesting = 0
        var astral = false
        var escaped = false
        for (i <- 0 until patternStr.uLength) {
          val ch = patternStr.uCharAt(i)
          if (ch > 65535) astral = true
          if (ch == '\\' && !escaped) {
            escaped = true
            sb.appendWideChar(ch)
          }
          else if (ch == '[' && !escaped) {
            nesting += 1
            escaped = false
            sb.appendWideChar(ch)
          }
          else if (ch == ']' && !escaped) {
            nesting -= 1
            escaped = false
            sb.appendWideChar(ch)
          }
          else if (nesting == 0 && Whitespace.isWhitespace(ch)) {

          }
          else {
            escaped = false
            sb.appendWideChar(ch)
          }
        }
        if (astral) patternStr = new GeneralUnicodeString(sb)
        else patternStr = new BMPString(sb)
        this.pattern = patternStr
        this.len = patternStr.uLength
      }

      val compilerFlags = Array(RECompiler.NODE_TOPLEVEL)

      val exp = parseExpr(compilerFlags)

      if (idx != len) {
        if (patternStr.uCharAt(idx) == ')') syntaxError("Unmatched close paren")
        syntaxError("Unexpected input remains")
      }
      val program = new REProgram(exp, capturingOpenParenCount, reFlags)
      if (hasBackReferences) program.optimizationFlags |= REProgram.OPT_HASBACKREFS
      program
    }
  }
}