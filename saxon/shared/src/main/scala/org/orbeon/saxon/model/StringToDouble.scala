package org.orbeon.saxon.model

import org.orbeon.saxon.trans.Err
import org.orbeon.saxon.value.DoubleValue
import org.orbeon.saxon.value.Whitespace

import scala.util.control.Breaks._

object StringToDouble {
  val getInstance: StringToDouble = new StringToDouble
  private val powers = Array[Double](1, 10, 100, 1000, 10000, 100000, 1000000, 10000000, 100000000)
}

class StringToDouble extends StringConverter {

  @throws[NumberFormatException]
  def stringToNumber(s: CharSequence): Double = {
    val len = s.length
    var containsDisallowedChars = false
    var containsWhitespace = false
    if (len < 9) {
      var useJava = false
      var num = 0L
      var dot = -1
      var lastDigit = -1
      var onlySpaceAllowed = false
      breakable {
        for (i <- 0 until len) {
          val c = s.charAt(i)
          c match {
            case ' ' | '\n' | '\t' | '\r' =>
              containsWhitespace = true
              if (lastDigit != -1)
                onlySpaceAllowed = true
            case '0' | '1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9' =>
              if (onlySpaceAllowed)
                throw new NumberFormatException("Numeric value contains embedded whitespace")
              lastDigit = i
              num = num * 10 + (c - '0')
            case '.' =>
              if (onlySpaceAllowed) throw new NumberFormatException("Numeric value contains embedded whitespace")
              if (dot != -1) throw new NumberFormatException("Only one decimal point allowed")
              dot = i
            case 'x' | 'X' | 'f' | 'F' | 'd' | 'D' | 'n' | 'N' =>
              containsDisallowedChars = true
              useJava = true
              break()
            case _ => useJava = true
          }
        }
      }
      if (!useJava)
        if (lastDigit == -1) {
          throw new NumberFormatException("String to double conversion: no digits found")
        } else if (dot == -1 || dot > lastDigit) {
          return num.toDouble
        } else {
          val afterPoint = lastDigit - dot
          return num.toDouble / StringToDouble.powers(afterPoint)
        }
    } else {
      breakable {
        for (i <- 0 until len) {
          val c = s.charAt(i)
          c match {
            case ' ' | '\n' | '\t' | '\r' =>
              containsWhitespace = true
            case '0' | '1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9' | '.' | 'e' | 'E' | '+' | '-' =>
            case _ =>
              containsDisallowedChars = true
              break()
          }
        }
      }
    }

    val n =
      if (containsWhitespace)
        Whitespace.trimWhitespace(s).toString
      else
        s.toString

    if ("INF" == n)
      Double.PositiveInfinity
    else if ("+INF" == n)
      // Allowed in XSD 1.1 but not in XSD 1.0
      signedPositiveInfinity
    else if ("-INF" == n)
      Double.NegativeInfinity
    else if ("NaN" == n)
      Double.NaN
    else {
      // reject strings containing characters such as (x, f, d) allowed in Java but not in XPath,
      // and other representations of NaN and Infinity such as 'Infinity'
      if (containsDisallowedChars)
        throw new NumberFormatException("invalid floating point value: " + s)
      else
        n.toDouble
    }
  }

  def signedPositiveInfinity: Double =
    throw new NumberFormatException("the float/double value '+INF' is not allowed under XSD 1.0")

  override def convertString(input: CharSequence): ConversionResult = try {
    val d = stringToNumber(input)
    new DoubleValue(d)
  } catch {
    case e: NumberFormatException =>
      new ValidationFailure("Cannot convert string " + Err.wrap(input, Err.VALUE) + " to double")
  }
}