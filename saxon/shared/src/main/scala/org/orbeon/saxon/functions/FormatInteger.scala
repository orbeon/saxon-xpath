package org.orbeon.saxon.functions

import java.util.function.IntPredicate
import java.util.{ArrayList, List}

import org.orbeon.saxon.expr.number._
import org.orbeon.saxon.expr.parser.{ContextItemStaticInfo, ExpressionVisitor}
import org.orbeon.saxon.expr.{Expression, Literal, XPathContext}
import org.orbeon.saxon.functions.FormatInteger._
import org.orbeon.saxon.lib.Numberer
import org.orbeon.saxon.om.Sequence
import org.orbeon.saxon.regex.charclass.Categories
import org.orbeon.saxon.regex.{ARegularExpression, RegularExpression, UnicodeString}
import org.orbeon.saxon.trans.XPathException
import org.orbeon.saxon.tree.util.FastStringBuffer
import org.orbeon.saxon.utils.Configuration
import org.orbeon.saxon.value.{IntegerValue, StringValue}
import org.orbeon.saxon.z.{IntHashSet, IntSet}

import scala.util.control.Breaks._

object FormatInteger {

  private var badHashPattern: RegularExpression = _
  private var modifierPattern: RegularExpression = _
  private var decimalDigitPattern: RegularExpression = _

  val preface: String = "In the picture string for format-integer, "

  locally {
    badHashPattern = new ARegularExpression("((\\d+|\\w+)#+.*)|(#+[^\\d]+)",
      "",
      "XP20",
      null,
      null)
    modifierPattern = new ARegularExpression("([co](\\(.*\\))?)?[at]?", "", "XP20", null, null)
    decimalDigitPattern = new ARegularExpression(
      "^((\\p{Nd}|#|[^\\p{N}\\p{L}])+?)$",
      "",
      "XP20",
      null,
      null)
  }

  def getPicSeparators(pic: String): NumericGroupFormatter = {
    val picExpanded: UnicodeString = UnicodeString.makeUnicodeString(pic)
    val groupingPositions: IntSet = new IntHashSet(5)
    val separatorList: List[Integer] = new ArrayList[Integer]()
    var groupingPosition: Int = 0
    var firstGroupingPos: Int = 0
    var lastGroupingPos: Int = 0
    var regularCheck: Boolean = true
    var zeroDigit: Int = -1
    if (badHashPattern.matches(pic)) {
      throw new XPathException(
        preface +
          "the picture is not valid (it uses '#' where disallowed)",
        "FODF1310")
    }
    var i: Int = picExpanded.uLength - 1
    while (i >= 0) {
      val codePoint: Int = picExpanded.uCharAt(i)
      java.lang.Character.getType(codePoint) match {
        case java.lang.Character.DECIMAL_DIGIT_NUMBER =>
          if (zeroDigit == -1) {
            zeroDigit = Alphanumeric.getDigitFamily(codePoint)
          } else {
            if (zeroDigit != Alphanumeric.getDigitFamily(codePoint)) {
              throw new XPathException(
                preface +
                  "the picture mixes digits from different digit families",
                "FODF1310")
            }
          }
          groupingPosition += 1
        case java.lang.Character.LETTER_NUMBER |
             java.lang.Character.OTHER_NUMBER |
             java.lang.Character.UPPERCASE_LETTER |
             java.lang.Character.LOWERCASE_LETTER |
             java.lang.Character.MODIFIER_LETTER |
             java.lang.Character.OTHER_LETTER =>
        case _ =>
          if (i == picExpanded.uLength - 1) {
            throw new XPathException(
              preface + "the picture cannot end with a separator",
              "FODF1310")
          }
          if (codePoint == '#') {
            groupingPosition += 1
            if (i != 0) {
              java.lang.Character.getType(picExpanded.uCharAt(i - 1)) match {
                case java.lang.Character.DECIMAL_DIGIT_NUMBER |
                     java.lang.Character.LETTER_NUMBER |
                     java.lang.Character.OTHER_NUMBER |
                     java.lang.Character.UPPERCASE_LETTER |
                     java.lang.Character.LOWERCASE_LETTER |
                     java.lang.Character.MODIFIER_LETTER |
                     java.lang.Character.OTHER_LETTER =>
                  throw new XPathException(
                    preface +
                      "the picture cannot contain alphanumeric character(s) before character '#'",
                    "FODF1310")

              }
            }
          } else {
            val added: Boolean = groupingPositions.add(groupingPosition)
            if (!added) {
              throw new XPathException(
                preface + "the picture contains consecutive separators",
                "FODF1310")
            }
            separatorList.add(codePoint)
            if (groupingPositions.size == 1) {
              firstGroupingPos = groupingPosition
            } else {
              if (groupingPosition != firstGroupingPos * groupingPositions.size) {
                regularCheck = false
              }
              if (separatorList.get(0) != codePoint) {
                regularCheck = false
              }
            }
            if (i == 0) {
              throw new XPathException(
                preface + "the picture cannot begin with a separator",
                "FODF1310")
            }
            lastGroupingPos = groupingPosition
          }

      }
      {
        i -= 1
        i + 1
      }
    }
    if (regularCheck && groupingPositions.size >= 1) {
      if (picExpanded.uLength - lastGroupingPos - groupingPositions.size >
        firstGroupingPos) {
        regularCheck = false
      }
    }
    val adjustedPic: UnicodeString =
      extractSeparators(picExpanded, groupingPositions)
    if (groupingPositions.isEmpty) {
      new RegularGroupFormatter(0, "", adjustedPic)
    }
    if (regularCheck) {
      if (separatorList.isEmpty) {
        new RegularGroupFormatter(0, "", adjustedPic)
      } else {
        val sb = new FastStringBuffer(4)
        sb.appendWideChar(separatorList.get(0))
        new RegularGroupFormatter(firstGroupingPos, sb.toString, adjustedPic)
      }
    } else {
      new IrregularGroupFormatter(groupingPositions,
        separatorList,
        adjustedPic)
    }
  }

  private def extractSeparators(arr: UnicodeString,
                                excludePositions: IntSet): UnicodeString = {
    val fsb = new FastStringBuffer(arr.uLength)
    for (i <- 0 until arr.uLength
         if NumberFormatter.isLetterOrDigit(arr.uCharAt(i))) {
      fsb.appendWideChar(arr.uCharAt(i))
    }
    UnicodeString.makeUnicodeString(fsb)
  }

}

class FormatInteger extends SystemFunction with StatefulSystemFunction {

  private var formatter: IntegerValue => String = null

  override def makeOptimizedFunctionCall(visitor: ExpressionVisitor, contextInfo: ContextItemStaticInfo,
                                         arguments: Expression*): Expression = {
    var opt: Boolean = true
    if (! arguments(1).isInstanceOf[Literal])
      opt = false
    if (arguments.length == 3 && ! arguments(2).isInstanceOf[Literal])
      opt = false
    if (! opt)
      super.makeOptimizedFunctionCall(visitor, contextInfo, arguments: _*)

    val config = visitor.getConfiguration
    val language =
      if (arguments.length == 3)
        arguments(2).asInstanceOf[Literal].value.getStringValue
      else
        config.getDefaultLanguage
    val numb = config.makeNumberer(language, null)
    formatter = makeFormatter(
      numb,
      arguments(1).asInstanceOf[Literal].value.getStringValue)
    super.makeOptimizedFunctionCall(visitor, contextInfo, arguments: _*)
  }

  def call(context: XPathContext, arguments: Array[Sequence]): StringValue =
    formatInteger(
      arguments(0).head.asInstanceOf[IntegerValue],
      arguments(1).head.asInstanceOf[StringValue],
      if (arguments.length == 2) null
      else arguments(2).head.asInstanceOf[StringValue],
      context
    )

  private def formatInteger(num: IntegerValue,
                            picture: StringValue,
                            language: StringValue,
                            context: XPathContext): StringValue = {
    val config: Configuration = context.getConfiguration
    if (num == null) {
      StringValue.EMPTY_STRING
    }
    var localFormatter = formatter
    if (localFormatter == null) {
      var languageVal: String = null
      languageVal =
        if (language != null) language.getStringValue
        else config.getDefaultLanguage
      val numb: Numberer = config.makeNumberer(languageVal, null)
      localFormatter = makeFormatter(numb, picture.getStringValue)
    }
    new StringValue(localFormatter.apply(num))
  }

  private def makeFormatter(numb: Numberer, pic: String): IntegerValue => String = {
    if (pic.isEmpty) {
      throw new XPathException(preface + "the picture cannot be empty",
        "FODF1310")
    }
    var primaryToken: String = null
    var modifier: String = null
    var parenthetical: String = null
    val lastSemicolon: Int = pic.lastIndexOf(';')
    if (lastSemicolon >= 0) {
      primaryToken = pic.substring(0, lastSemicolon)
      if (primaryToken.isEmpty) {
        throw new XPathException(
          preface + "the primary format token cannot be empty",
          "FODF1310")
      }
      modifier =
        if (lastSemicolon < pic.length - 1) pic.substring(lastSemicolon + 1)
        else ""
      if (!modifierPattern.matches(modifier)) {
        throw new XPathException(preface + "the modifier is invalid",
          "FODF1310")
      }
    } else {
      primaryToken = pic
      modifier = ""
    }
    val ordinal: Boolean = modifier.startsWith("o")
    val alphabetic: Boolean = modifier.endsWith("a")
    val leftParen: Int = modifier.indexOf('(')
    val rightParen: Int = modifier.lastIndexOf(')')
    parenthetical =
      if (leftParen < 0) "" else modifier.substring(leftParen + 1, rightParen)
    val letterValue: String = if (alphabetic) "alphabetic" else "traditional"
    val ordinalValue: String =
      if (ordinal) if ("" == parenthetical) "yes" else parenthetical else ""
    val primary: UnicodeString = UnicodeString.makeUnicodeString(primaryToken)
    val isDecimalDigit: IntPredicate = Categories.getCategory("Nd")
    var isDecimalDigitPattern: Boolean = false
    breakable {
      for (i <- 0 until primary.uLength
           if isDecimalDigit.test(primary.uCharAt(i))) {
        isDecimalDigitPattern = true
        break()
      }
    }
    if (isDecimalDigitPattern) {
      if (!decimalDigitPattern.matches(primaryToken)) {
        throw new XPathException(
          preface +
            "the primary format token contains a decimal digit but does not " +
            "meet the rules for a decimal digit pattern",
          "FODF1310")
      }
      val picGroupFormat: NumericGroupFormatter = getPicSeparators(
        primaryToken)
      val adjustedPicture: UnicodeString = picGroupFormat.getAdjustedPicture
      num => {
        val s = numb.format(num.abs().longValue,
          adjustedPicture,
          picGroupFormat,
          letterValue,
          ordinalValue)
        if (num.signum() < 0) "-" + s else s
      }
    } else {
      val token: UnicodeString = UnicodeString.makeUnicodeString(primaryToken)
      num => {
        val s: String = numb.format(num.abs().longValue,
          token,
          null,
          letterValue,
          ordinalValue)
        if (num.signum() < 0) "-" + s else s
      }
    }
  }

  override def copy(): SystemFunction = {
    val fi2: FormatInteger = SystemFunction
      .makeFunction(getFunctionName.getLocalPart,
        getRetainedStaticContext,
        getArity)
      .asInstanceOf[FormatInteger]
    fi2.formatter = formatter
    fi2
  }

}
