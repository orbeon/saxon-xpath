package net.sf.saxon.functions

import net.sf.saxon.expr._

import net.sf.saxon.om.Item

import net.sf.saxon.om.Sequence

import net.sf.saxon.om.StructuredQName

import net.sf.saxon.s9api.HostLanguage

import net.sf.saxon.trans.DecimalFormatManager

import net.sf.saxon.trans.DecimalSymbols

import net.sf.saxon.trans.XPathException

import net.sf.saxon.tree.tiny.CharSlice

import net.sf.saxon.tree.util.FastStringBuffer

import net.sf.saxon.value._

import java.math.BigDecimal

import java.math.RoundingMode

import java.util.ArrayList

import java.util.Arrays

import java.util.List

import FormatNumber._
import scala.util.control.Breaks._

object FormatNumber {

  private def getSubPictures(picture: String,
                             dfs: DecimalSymbols): Array[SubPicture] = {
    val picture4: Array[Int] = StringValue.expand(picture)
    val pics: Array[SubPicture] = Array.ofDim[SubPicture](2)
    if (picture4.length == 0) {
      val err = new XPathException(
        "format-number() picture is zero-length")
      err.setErrorCode("FODF1310")
      throw err
    }
    var sep: Int = -1
    for (c <- 0 until picture4.length
         if picture4(c) == dfs.getPatternSeparator) {
      if (c == 0) {
        grumble("first subpicture is zero-length")
      } else if (sep >= 0) {
        grumble("more than one pattern separator")
      } else if (sep == picture4.length - 1) {
        grumble("second subpicture is zero-length")
      }
      sep = c
    }
    if (sep < 0) {
      pics(0) = new SubPicture(picture4, dfs)
      pics(1) = null
    } else {
      val pic0: Array[Int] = Array.ofDim[Int](sep)
      System.arraycopy(picture4, 0, pic0, 0, sep)
      val pic1: Array[Int] = Array.ofDim[Int](picture4.length - sep - 1)
      System.arraycopy(picture4, sep + 1, pic1, 0, picture4.length - sep - 1)
      pics(0) = new SubPicture(pic0, dfs)
      pics(1) = new SubPicture(pic1, dfs)
    }
    pics
  }

  private def formatNumber(number: NumericValue,
                           subPictures: Array[SubPicture],
                           dfs: DecimalSymbols): CharSequence = {
    var absN: NumericValue = number
    var pic: SubPicture = null
    var minusSign: String = ""
    var signum: Int = number.signum()
    if (signum == 0 && number.isNegativeZero) {
      signum = -1
    }
    if (signum < 0) {
      absN = number.negate()
      if (subPictures(1) == null) {
        pic = subPictures(0)
        minusSign = "" + unicodeChar(dfs.getMinusSign)
      } else {
        pic = subPictures(1)
      }
    } else {
      pic = subPictures(0)
    }
    pic.format(absN, dfs, minusSign)
  }

  private def grumble(s: String): Unit = {
    throw new XPathException("format-number picture: " + s, "FODF1310")
  }

  def adjustToDecimal(value: Double, precision: Int): BigDecimal = {
    val zeros: String = if (precision == 1) "00000" else "000000000"
    val nines: String = if (precision == 1) "99999" else "999999999"
    val initial: BigDecimal = BigDecimal.valueOf(value)
    var trial: BigDecimal = null
    val fsb: FastStringBuffer = new FastStringBuffer(FastStringBuffer.C16)
    BigDecimalValue.decimalToString(initial, fsb)
    val s: String = fsb.toString
    val start: Int = if (s.charAt(0) == '-') 1 else 0
    val p: Int = s.indexOf(".")
    var i: Int = s.lastIndexOf(zeros)
    if (i > 0) {
      if (p < 0 || i < p) {
        val sb: FastStringBuffer = new FastStringBuffer(s.length)
        sb.append(s.substring(0, i))
        for (n <- i until s.length) {
          sb.cat(if (s.charAt(n) == '.') '.' else '0')
        }
        trial = new BigDecimal(sb.toString)
      } else {
        trial = new BigDecimal(s.substring(0, i))
      }
    } else {
      i = s.indexOf(nines)
      if (i >= 0) {
        if (i == start) {
          val sb: FastStringBuffer = new FastStringBuffer(s.length + 1)
          if (start == 1) {
            sb.cat('-')
          }
          sb.cat('1')
          for (n <- start until s.length) {
            sb.cat(if (s.charAt(n) == '.') '.' else '0')
          }
          trial = new BigDecimal(sb.toString)
        } else {
          while (i >= 0 && (s.charAt(i) == '9' || s.charAt(i) == '.')) {
            i -= 1
          }
          if (i < 0 || s.charAt(i) == '-') {
            return initial
          } else if (p < 0 || i < p) {
            val sb: FastStringBuffer = new FastStringBuffer(s.length)
            sb.append(s.substring(0, i))
            sb.cat((s.charAt(i).toInt + 1).toChar)
            for (n <- i until s.length) {
              sb.cat(if (s.charAt(n) == '.') '.' else '0')
            }
            trial = new BigDecimal(sb.toString)
          } else {
            val s2: String = s.substring(0, i) + (s.charAt(i).toInt + 1).toChar
            trial = new BigDecimal(s2)
          }
        }
      }
    }
    if (trial != null &&
      (if (precision == 1) trial.floatValue() == value
      else trial.doubleValue() == value)) {
      trial
    } else {
      initial
    }
  }

  private class SubPicture(pic: Array[Int], dfs: DecimalSymbols) {

    var minWholePartSize: Int = 0

    var maxWholePartSize: Int = 0

    var minFractionPartSize: Int = 0

    var maxFractionPartSize: Int = 0

    var minExponentSize: Int = 0

    var scalingFactor: Int = 0

    var isPercent: Boolean = false

    var isPerMille: Boolean = false

    var prefix: String = ""

    var suffix: String = ""

    var wholePartGroupingPositions: Array[Int] = null

    var fractionalPartGroupingPositions: Array[Int] = null

    var regular: Boolean = _

    var is31: Boolean = true

    val percentSign: Int = dfs.getPercent

    val perMilleSign: Int = dfs.getPerMille

    val decimalSeparator: Int = dfs.getDecimalSeparator

    val groupingSeparator: Int = dfs.getGroupingSeparator

    val digitSign: Int = dfs.getDigit

    val zeroDigit: Int = dfs.getZeroDigit

    val exponentSeparator: Int = dfs.getExponentSeparator

    var wholePartPositions: List[Integer] = null

    var fractionalPartPositions: List[Integer] = null

    var foundDigit: Boolean = false

    var foundDecimalSeparator: Boolean = false

    var foundExponentSeparator: Boolean = false

    var foundExponentSeparator2: Boolean = false

    breakable {
      for (ch <- pic
           if ch == digitSign || ch == zeroDigit || isInDigitFamily(ch,
             zeroDigit)) {
        foundDigit = true
        break()
      }
    }

    if (!foundDigit) {
      grumble("subpicture contains no digit or zero-digit sign")
    }

    var phase: Int = 0

    for (c <- pic) {
      if (c == percentSign || c == perMilleSign) {
        if (isPercent || isPerMille) {
          grumble(
            "Cannot have more than one percent or per-mille character in a sub-picture")
        }
        isPercent = c == percentSign
        isPerMille = c == perMilleSign
        phase match {
          case 0 => prefix += unicodeChar(c)
          case 1 | 2 | 3 | 4 | 5 =>
            if (foundExponentSeparator) {
              grumble(
                "Cannot have exponent-separator as well as percent or per-mille character in a sub-picture")
            }
          case 6 =>
            phase = 6
            suffix += unicodeChar(c)

        }
      } else if (c == digitSign) {
        phase match {
          case 0 | 1 =>
            phase = 1
            maxWholePartSize += 1
          case 2 =>
            grumble(
              "Digit sign must not appear after a zero-digit sign in the integer part of a sub-picture")
          case 3 | 4 =>
            phase = 4
            maxFractionPartSize += 1
          case 5 =>
            grumble(
              "Digit sign must not appear in the exponent part of a sub-picture")
          case 6 =>
            if (foundExponentSeparator2) {
              grumble(
                "There must only be one exponent separator in a sub-picture")
            } else {
              grumble(
                "Passive character must not appear between active characters in a sub-picture")
            }

        }
      } else if (c == zeroDigit || isInDigitFamily(c, zeroDigit)) {
        phase match {
          case 0 | 1 | 2 =>
            phase = 2
            minWholePartSize += 1
            maxWholePartSize += 1
          case 3 => minFractionPartSize += 1
            maxFractionPartSize += 1
          case 4 =>
            grumble(
              "Zero digit sign must not appear after a digit sign in the fractional part of a sub-picture")
          case 5 => minExponentSize += 1
          case 6 =>
            if (foundExponentSeparator2) {
              grumble(
                "There must only be one exponent separator in a sub-picture")
            } else {
              grumble(
                "Passive character must not appear between active characters in a sub-picture")
            }

        }
      } else if (c == decimalSeparator) {
        if (foundDecimalSeparator) {
          grumble("There must only be one decimal separator in a sub-picture")
        }
        phase match {
          case 0 | 1 | 2 =>
            phase = 3
            foundDecimalSeparator = true
          case 3 | 4 | 5 =>
            if (foundExponentSeparator) {
              grumble(
                "Decimal separator must not appear in the exponent part of a sub-picture")
            }
          case 6 =>
            grumble(
              "Decimal separator cannot come after a character in the suffix")

        }
      } else if (c == groupingSeparator) {
        phase match {
          case 0 | 1 | 2 =>
            if (wholePartPositions == null) {
              wholePartPositions = new ArrayList(3)
            }
            if (wholePartPositions.contains(maxWholePartSize)) {
              grumble(
                "Sub-picture cannot contain adjacent grouping separators")
            }
            wholePartPositions.add(maxWholePartSize)
          case 3 | 4 =>
            if (maxFractionPartSize == 0) {
              grumble(
                "Grouping separator cannot be adjacent to decimal separator")
            }
            if (fractionalPartPositions == null) {
              fractionalPartPositions = new ArrayList[Integer](3)
            }
            if (fractionalPartPositions.contains(maxFractionPartSize)) {
              grumble(
                "Sub-picture cannot contain adjacent grouping separators")
            }
            fractionalPartPositions.add(maxFractionPartSize)
          case 5 =>
            if (foundExponentSeparator) {
              grumble(
                "Grouping separator must not appear in the exponent part of a sub-picture")
            }
          case 6 =>
            grumble("Grouping separator found in suffix of sub-picture")

        }
      } else if (c == exponentSeparator) {
        phase match {
          case 0 => prefix += unicodeChar(c)
          case 1 | 2 | 3 | 4 =>
            phase = 5
            foundExponentSeparator = true
          case 5 =>
            if (foundExponentSeparator) {
              foundExponentSeparator2 = true
              phase = 6
              suffix += unicodeChar(exponentSeparator)
            }
          case 6 => suffix += unicodeChar(c)

        }
      } else {
        phase match {
          case 0 => prefix += unicodeChar(c)
          case 1 | 2 | 3 | 4 | 5 =>
            if (minExponentSize == 0 && foundExponentSeparator) {
              phase = 6
              suffix += unicodeChar(exponentSeparator)
              suffix += unicodeChar(c)
            }
          case 6 =>
            phase = 6
            suffix += unicodeChar(c)

        }
      }
    }

    if (maxWholePartSize == 0 && maxFractionPartSize == 0) {
      grumble("Mantissa contains no digit or zero-digit sign")
    }

    if (minWholePartSize == 0 && maxFractionPartSize == 0) {
      if (minExponentSize != 0) {
        minFractionPartSize = 1
        maxFractionPartSize = 1
      } else {
        minWholePartSize = 1
      }
    }

    if (minExponentSize != 0 && minWholePartSize == 0 && maxWholePartSize != 0) {
      minWholePartSize = 1
    }

    if (minWholePartSize == 0 && minFractionPartSize == 0) {
      minFractionPartSize = 1
    }

    if (wholePartPositions != null) {
      val n: Int = wholePartPositions.size
      wholePartGroupingPositions = Array.ofDim[Int](n)
      for (i <- 0 until n) {
        wholePartGroupingPositions(i) = maxWholePartSize - wholePartPositions
          .get(n - i - 1)
      }
      if (n == 1) {
        regular = wholePartGroupingPositions(0) * 2 >= maxWholePartSize
      } else if (n > 1) {
        regular = true
        val first: Int = wholePartGroupingPositions(0)
        breakable {
          for (i <- 1 until n
               if wholePartGroupingPositions(i) != (i + 1) * first) {
            regular = false
            break()
          }
        }
        if (regular &&
          (maxWholePartSize - wholePartGroupingPositions(n - 1) >
            first)) {
          regular = false
        }
        if (regular) {
          wholePartGroupingPositions = Array.ofDim[Int](1)
          wholePartGroupingPositions(0) = first
        }
      }
      if (wholePartGroupingPositions(0) == 0) {
        grumble(
          "Cannot have a grouping separator at the end of the integer part")
      }
    }

    if (fractionalPartPositions != null) {
      val n: Int = fractionalPartPositions.size
      fractionalPartGroupingPositions = Array.ofDim[Int](n)
      for (i <- 0 until n) {
        fractionalPartGroupingPositions(i) = fractionalPartPositions.get(i)
      }
    }

    def format(value: NumericValue,
               dfs: DecimalSymbols,
               minusSign: String): CharSequence = {
      var numValue = value
      if (numValue.isNaN) {
        dfs.getNaN
      }
      var multiplier: Int = 1
      if (isPercent) {
        multiplier = 100
      } else if (isPerMille) {
        multiplier = 1000
      }
      if (multiplier != 1) {
        try numValue = ArithmeticExpression
          .compute(numValue, Calculator.TIMES, new Int64Value(multiplier), null)
          .asInstanceOf[NumericValue]
        catch {
          case e: XPathException =>
            numValue = new DoubleValue(java.lang.Double.POSITIVE_INFINITY)

        }
      }
      if ((numValue.isInstanceOf[DoubleValue] || numValue
        .isInstanceOf[FloatValue]) &&
        java.lang.Double.isInfinite(numValue.getDoubleValue)) {
        minusSign + prefix + dfs.getInfinity + suffix
      }
      val sb: FastStringBuffer = new FastStringBuffer(FastStringBuffer.C16)
      if (numValue.isInstanceOf[DoubleValue] || numValue.isInstanceOf[FloatValue]) {
        val dec: BigDecimal = adjustToDecimal(numValue.getDoubleValue, 2)
        formatDecimal(dec, sb)
      } else if (numValue.isInstanceOf[IntegerValue]) {
        if (minExponentSize != 0) {
          formatDecimal(numValue.asInstanceOf[IntegerValue].getDecimalValue, sb)
        } else {
          formatInteger(numValue, sb)
        }
      } else if (numValue.isInstanceOf[BigDecimalValue]) {
        formatDecimal(numValue.asInstanceOf[BigDecimalValue].getDecimalValue, sb)
      }
      var ib: Array[Int] = StringValue.expand(sb)
      var ibused: Int = ib.length
      var point: Int = sb.indexOf('.')
      if (point == -1) {
        point = sb.length
      } else {
        ib(point) = dfs.getDecimalSeparator
        if (maxFractionPartSize == 0) {
          ibused -= 1
        }
      }
      if (dfs.getZeroDigit != '0') {
        val newZero: Int = dfs.getZeroDigit
        for (i <- 0 until ibused) {
          val c: Int = ib(i)
          if (c >= '0' && c <= '9') {
            ib(i) = c - '0' + newZero
          }
        }
      }
      if (dfs.getExponentSeparator != 'e') {
        val expS: Int = sb.indexOf('e')
        if (expS != -1) {
          ib(expS) = dfs.getExponentSeparator
        }
      }
      if (wholePartGroupingPositions != null) {
        if (regular) {
          val g: Int = wholePartGroupingPositions(0)
          var p: Int = point - g
          while (p > 0) {
            ib = insert(ib, {
              ibused += 1;
              ibused - 1
            },
              dfs.getGroupingSeparator,
              p)
            p -= g
          }
        } else {
          for (wholePartGroupingPosition <- wholePartGroupingPositions) {
            val p: Int = point - wholePartGroupingPosition
            if (p > 0) {
              ib = insert(ib, {
                ibused += 1;
                ibused - 1
              },
                dfs.getGroupingSeparator,
                p)
            }
          }
        }
      }
      if (fractionalPartGroupingPositions != null) {
        breakable {
          for (i <- 0 until fractionalPartGroupingPositions.length) {
            val p: Int = point + 1 + fractionalPartGroupingPositions(i) + i
            if (p < ibused) {
              ib = insert(ib, {
                ibused += 1;
                ibused - 1
              },
                dfs.getGroupingSeparator,
                p)
            } else {
              break()
            }
          }
        }
      }
      val res: FastStringBuffer = new FastStringBuffer(
        prefix.length + minusSign.length + suffix.length + ibused)
      res.append(minusSign)
      res.append(prefix)
      for (i <- 0 until ibused) {
        res.appendWideChar(ib(i))
      }
      res.append(suffix)
      res
    }

    private def formatDecimal(dval: BigDecimal, fsb: FastStringBuffer): Unit = {
      var exponent: Int = 0
      var decValue = dval
      if (minExponentSize == 0) {
        decValue = decValue.setScale(maxFractionPartSize, RoundingMode.HALF_EVEN)
      } else {
        exponent = decValue.precision() - decValue.scale() - scalingFactor
        decValue = decValue.movePointLeft(exponent)
        decValue = decValue.setScale(maxFractionPartSize, RoundingMode.HALF_EVEN)
      }
      BigDecimalValue.decimalToString(decValue, fsb)
      val point: Int = fsb.indexOf('.')
      var intDigits: Int = 0
      if (point >= 0) {
        var zz: Int = maxFractionPartSize - minFractionPartSize
        breakable {
          while (zz > 0) if (fsb.charAt(fsb.length - 1) == '0') {
            fsb.setLength(fsb.length - 1)
            zz -= 1
          } else {
            break()
          }
        }
        intDigits = point
        if (fsb.charAt(fsb.length - 1) == '.') {
          fsb.setLength(fsb.length - 1)
        }
      } else {
        intDigits = fsb.length
        if (minFractionPartSize > 0) {
          fsb.cat('.')
          for (i <- 0 until minFractionPartSize) {
            fsb.cat('0')
          }
        }
      }
      if (minWholePartSize == 0 && intDigits == 1 && fsb.charAt(0) == '0') {
        fsb.removeCharAt(0)
      } else {
        fsb.prependRepeated('0', minWholePartSize - intDigits)
      }
      if (minExponentSize != 0) {
        fsb.cat('e')
        val exp: IntegerValue =
          IntegerValue.makeIntegerValue(exponent).asInstanceOf[IntegerValue]
        var expStr: String = exp.toString
        val first: Char = expStr.charAt(0)
        if (first == '-') {
          fsb.cat('-')
          expStr = expStr.substring(1)
        }
        val length: Int = expStr.length
        if (length < minExponentSize) {
          val zz: Int = minExponentSize - length
          for (i <- 0 until zz) {
            fsb.cat('0')
          }
        }
        fsb.append(expStr)
      }
    }

    private def formatInteger(value: NumericValue,
                              fsb: FastStringBuffer): Unit = {
      if (!(minWholePartSize == 0 && value.compareTo(0) == 0)) {
        fsb.cat(value.getStringValueCS)
        val leadingZeroes: Int = minWholePartSize - fsb.length
        fsb.prependRepeated('0', leadingZeroes)
      }
      if (minFractionPartSize != 0) {
        fsb.cat('.')
        for (i <- 0 until minFractionPartSize) {
          fsb.cat('0')
        }
      }
    }

  }

  private def unicodeChar(ch: Int): CharSequence = {
    var cH = ch
    if (cH < 65536) {
      "" + cH.toChar
    } else {
      cH -= 65536
      val sb: Array[Char] = Array.ofDim[Char](2)
      sb(0) = ((cH / 1024) + 55296).toChar
      sb(1) = ((cH % 1024) + 56320).toChar
      new CharSlice(sb, 0, 2)
    }
  }

  private def insert(array: Array[Int],
                     used: Int,
                     value: Int,
                     position: Int): Array[Int] = {
    var arr = array
    if (used + 1 > array.length) {
      arr = Arrays.copyOf(arr, used + 10)
    }
    System.arraycopy(arr, position, arr, position + 1, used - position)
    arr(position) = value
    arr
  }

  private def isInDigitFamily(ch: Int, zeroDigit: Int): Boolean =
    ch >= zeroDigit && ch < zeroDigit + 10

  def formatExponential(value: DoubleValue): String =
    try {
      val dfs: DecimalSymbols = new DecimalSymbols(HostLanguage.XSLT, 31)
      dfs.setInfinity("INF")
      val pics: Array[SubPicture] =
        getSubPictures("0.0##########################e0", dfs)
      formatNumber(value, pics, dfs).toString
    } catch {
      case e: XPathException => value.getStringValue

    }

}

class FormatNumber extends SystemFunction with Callable {

  private var decimalFormatName: StructuredQName = _

  private var picture: String = _

  private var decimalSymbols: DecimalSymbols = _

  private var subPictures: Array[SubPicture] = _

  override def fixArguments(arguments: Expression*): Expression = {
    if (arguments(1).isInstanceOf[Literal] &&
      (arguments.length == 2 || arguments(2).isInstanceOf[Literal])) {
      val dfm: DecimalFormatManager =
        getRetainedStaticContext.getDecimalFormatManager
      assert(dfm != null)
      picture = arguments(1).asInstanceOf[Literal].value.getStringValue
      if (arguments.length == 3 && !Literal.isEmptySequence(arguments(2))) {
        try {
          val lexicalName: String = arguments(2)
            .asInstanceOf[Literal]
            .value
            .getStringValue
          decimalFormatName = StructuredQName.fromLexicalQName(
            lexicalName,
            useDefault = false,
            allowEQName = true,
            getRetainedStaticContext)
        } catch {
          case e: XPathException => {
            val err = new XPathException(
              "Invalid decimal format name. " + e.getMessage)
            err.setErrorCode("FODF1280")
            throw err
          }

        }
      }
      if (decimalFormatName == null) {
        decimalSymbols = dfm.getDefaultDecimalFormat
      } else {
        decimalSymbols = dfm.getNamedDecimalFormat(decimalFormatName)
        if (decimalSymbols == null) {
          throw new XPathException(
            "Decimal format " + decimalFormatName.getDisplayName +
              " has not been defined",
            "FODF1280")
        }
      }
      subPictures = getSubPictures(picture, decimalSymbols)
    }
    null
  }

  def call(context: XPathContext, arguments: Array[Sequence]): StringValue = {
    val numArgs: Int = arguments.length
    val dfm: DecimalFormatManager =
      getRetainedStaticContext.getDecimalFormatManager
    var dfs: DecimalSymbols = null
    var av0: AtomicValue = arguments(0).head.asInstanceOf[AtomicValue]
    if (av0 == null) {
      av0 = DoubleValue.NaN
    }
    val number: NumericValue = av0.asInstanceOf[NumericValue]
    if (picture != null) {
      val result: CharSequence =
        formatNumber(number, subPictures, decimalSymbols)
      new StringValue(result)
    } else {
      if (numArgs == 2) {
        dfs = dfm.getDefaultDecimalFormat
      } else {
        val arg2: Item = arguments(2).head
        if (arg2 == null) {
          dfs = dfm.getDefaultDecimalFormat
        } else {
          val lexicalName: String = arg2.getStringValue
          dfs = getNamedDecimalFormat(dfm, lexicalName)
        }
      }
      val format: String = arguments(1).head.getStringValue
      val pics: Array[SubPicture] = getSubPictures(format, dfs)
      new StringValue(formatNumber(number, pics, dfs))
    }
  }

  def getNamedDecimalFormat(dfm: DecimalFormatManager,
                            lexicalName: String): DecimalSymbols = {
    var dfs: DecimalSymbols = null
    var qName: StructuredQName = null
    try qName = StructuredQName.fromLexicalQName(lexicalName,
      useDefault = false,
      allowEQName = true,
      getRetainedStaticContext)
    catch {
      case e: XPathException => {
        val err = new XPathException(
          "Invalid decimal format name. " + e.getMessage)
        err.setErrorCode("FODF1280")
        throw err
      }

    }
    dfs = dfm.getNamedDecimalFormat(qName)
    if (dfs == null) {
      val err = new XPathException(
        "format-number function: decimal-format '" + lexicalName +
          "' is not defined")
      err.setErrorCode("FODF1280")
      throw err
    }
    dfs
  }

}
