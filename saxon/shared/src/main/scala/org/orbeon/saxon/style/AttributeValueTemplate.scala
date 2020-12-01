////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// Copyright (c) 2018-2020 Saxonica Limited
// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is "Incompatible With Secondary Licenses", as defined by the Mozilla Public License, v. 2.0.
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
package org.orbeon.saxon.style

import java.{util => ju}

import org.orbeon.saxon.expr.parser.{RetainedStaticContext, Token, XPathParser}
import org.orbeon.saxon.expr._
import org.orbeon.saxon.functions.SystemFunction
import org.orbeon.saxon.model.BuiltInAtomicType
import org.orbeon.saxon.trans.XPathException
import org.orbeon.saxon.value.{Cardinality, IntegerValue, StringValue}

import scala.util.control.Breaks.{break, breakable}


/**
 * This class represents an attribute value template. The class allows an AVT to be parsed, and
 * can construct an Expression that returns the effective value of the AVT.
 *
 * This is an abstract class that is never instantiated, it contains static methods only.
 */
object AttributeValueTemplate {
  /**
   * Static factory method to create an AVT from an XSLT string representation. The
   * method is also used for text value templates.
   *
   * @param avt the attribute value template (or TVT) as written.
   * @param env the static context
   * @return an expression that computes the value of the attribute / content, as a string.
   *         In the case of a TVT this must be further processed to create a text node.
   */
  def make(avt: String, env: StaticContext): Expression = {

    val languageLevel = env.getXPathVersion
    val components = new ju.ArrayList[Expression](5)

    var i0 = 0
    var i1 = 0
    var i8 = 0
    var i9 = 0
    val len = avt.length
    var last = 0

    breakable {
      while (last < len) {

        i0 = avt.indexOf("{", last)
        i1 = avt.indexOf("{{", last)
        i8 = avt.indexOf("}", last)
        i9 = avt.indexOf("}}", last)

        if ((i0 < 0 || len < i0) && (i8 < 0 || len < i8)) { // found end of string
          addStringComponent(components, avt, last, len)
          break()
        } else if (i8 >= 0 && (i0 < 0 || i8 < i0)) { // found a "}"
          if (i8 != i9) { // a "}" that isn't a "}}"
            val err = new XPathException("Closing curly brace in attribute value template \"" + avt.substring(0, len) + "\" must be doubled")
            err.setErrorCode("XTSE0370")
            err.setIsStaticError(true)
            throw err
          }
          addStringComponent(components, avt, last, i8 + 1)
          last = i8 + 2
        } else if (i1 >= 0 && i1 == i0) { // found a doubled "{{"
          addStringComponent(components, avt, last, i1 + 1)
          last = i1 + 2
        }
        else if (i0 >= 0) { // found a single "{"
          if (i0 > last)
            addStringComponent(components, avt, last, i0)
          var exp: Expression = null
          val parser = env.getConfiguration.newExpressionParser("XP", updating = false, languageLevel)
          //parser.setDefaultContainer(container);
          parser.setLanguage(XPathParser.ParsedLanguage.XPATH, 31)
          parser.setAllowAbsentExpression(allowEmpty = true)
          exp = parser.parse(avt, i0 + 1, Token.RCURLY, env)
          exp.setRetainedStaticContext(env.makeRetainedStaticContext)
          exp = exp.simplify()
          last = parser.getTokenizer.currentTokenStartOffset + 1

//            if (env.isInstanceOf[ExpressionContext] && env.asInstanceOf[ExpressionContext].getStyleElement.isInstanceOf[XSLAnalyzeString] && isIntegerOrIntegerPair(exp))
//              env.issueWarning("Found {" + showIntegers(exp) + "} in regex attribute: perhaps {{" + showIntegers(exp) + "}} was intended? (The attribute is an AVT, so curly braces should be doubled)", exp.getLocation)
          if (env.isInBackwardsCompatibleMode)
            components.add(makeFirstItem(exp, env))
          else
            components.add(makeSimpleContentConstructor(exp, new StringLiteral(StringValue.SINGLE_SPACE), env).simplify)
        } else
          throw new IllegalStateException("Internal error parsing AVT")
      }
    }

    val result =
      if (components.isEmpty)
        new StringLiteral(StringValue.EMPTY_STRING)
      else { // is it a single component?
        if (components.size == 1)
          components.get(0).simplify()
        else { // otherwise, return an expression that concatenates the components
          val args = new Array[Expression](components.size)
          components.toArray(args)
          val fn = SystemFunction.makeCall("concat", new RetainedStaticContext(env), args: _*)
          fn.simplify()
        }
      }

    result.setLocation(env.getContainingLocation)
    result
  }

  /**
   * Used to detect warning condition when braces are undoubled in the regex attribute of xsl:analyze-string
   *
   * @param exp an expression
   * @return true if the expression is an integer literal or a pair of two integer literals
   */
  private def isIntegerOrIntegerPair(exp: Expression) =
    exp match {
      case literal: Literal =>
        val `val` = literal.getValue
        if (`val`.isInstanceOf[IntegerValue])
          true
        else if (`val`.getLength == 2)
          `val`.itemAt(0).isInstanceOf[IntegerValue] && `val`.itemAt(1).isInstanceOf[IntegerValue]
        else
          false
      case _ =>
        false
    }

  // ORBEON: Unused.
//  /**
//   * Used to report warning condition when braces are undoubled in the regex attribute of xsl:analyze-string
//   *
//   * @param exp an expression
//   * @return string representation of an integer literal or a pair of two integer literals
//   */
//  private def showIntegers(exp: Nothing): String = {
//    exp match {
//      case literal: Literal =>
//        val `val` = literal.getValue
//        if (`val`.isInstanceOf[IntegerValue])
//          return `val`.toString
//        else if (`val`.getLength eq 2)
//          if (`val`.itemAt(0).isInstanceOf[IntegerValue] && `val`.itemAt(1).isInstanceOf[IntegerValue])
//            return `val`.itemAt(0).toString + "," + `val`.itemAt(1).toString
//      case _ =>
//    }
//    ""
//  }

  private def addStringComponent(components: ju.List[Expression], avt: String, start: Int, `end`: Int): Unit =
    if (start < `end`)
      components.add(new StringLiteral(avt.substring(start, `end`)))

  /**
   * Make an expression that extracts the first item of a sequence, after atomization
   */
  /*@NotNull*/
  def makeFirstItem(_exp: Expression, env: StaticContext): Expression = {

    var exp = _exp

    if (Literal.isEmptySequence(exp))
      return exp
    val th = env.getConfiguration.getTypeHierarchy
    if (! exp.getItemType.isPlainType)
      exp = Atomizer.makeAtomizer(exp, null)
    if (Cardinality.allowsMany(exp.getCardinality))
      exp = FirstItemExpression.makeFirstItemExpression(exp)
    if (! th.isSubType(exp.getItemType, BuiltInAtomicType.STRING)) {
      exp = new AtomicSequenceConverter(exp, BuiltInAtomicType.STRING)
      exp.asInstanceOf[AtomicSequenceConverter].allocateConverterStatically(env.getConfiguration, allowNull = false)
    }
    exp
  }

  // ORBEON: Copied from `XSLLeafNodeConstructor`, which we don't want to include fully.
  /**
   * Construct an expression that implements the rules of "constructing simple content":
   * given an expression to select the base sequence, and an expression to compute the separator,
   * build an (unoptimized) expression to produce the value of the node as a string.
   *
   * @param _select   the expression that selects the base sequence
   * @param separator the expression that computes the separator
   * @param env       the static context
   * @return an expression that returns a string containing the string value of the constructed node
   */
  def makeSimpleContentConstructor(_select: Expression, separator: Expression, env: StaticContext): Expression = {

    var select = _select

    var rsc = select.getLocalRetainedStaticContext
    if (rsc == null)
      rsc = env.makeRetainedStaticContext
    // Merge adjacent text nodes
    select = AdjacentTextNodeMerger.makeAdjacentTextNodeMerger(select)
    // Atomize the result
    select = Atomizer.makeAtomizer(select, null)
    // Convert each atomic value to a string
    select = new AtomicSequenceConverter(select, BuiltInAtomicType.STRING)
    select.setRetainedStaticContext(rsc)
    select.asInstanceOf[AtomicSequenceConverter].allocateConverterStatically(env.getConfiguration, allowNull = false)
    // Join the resulting strings with a separator
    if (select.getCardinality != StaticProperty.EXACTLY_ONE)
      select = SystemFunction.makeCall("string-join", rsc, select, separator)
    // All that's left for the instruction to do is to construct the right kind of node
    select
  }
}
