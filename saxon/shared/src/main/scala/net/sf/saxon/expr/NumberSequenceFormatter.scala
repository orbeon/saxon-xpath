////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
package net.sf.saxon.expr

import net.sf.saxon.utils.Configuration

import scala.jdk.CollectionConverters._

import net.sf.saxon.expr.number.NumberFormatter

import net.sf.saxon.expr.parser.ContextItemStaticInfo

import net.sf.saxon.expr.parser.ExpressionTool

import net.sf.saxon.expr.parser.ExpressionVisitor

import net.sf.saxon.expr.parser.RebindingMap

import net.sf.saxon.functions.Number_1

import net.sf.saxon.lib.ConversionRules

import net.sf.saxon.lib.Numberer

import net.sf.saxon.model._

import net.sf.saxon.om.SequenceIterator

import net.sf.saxon.trace.ExpressionPresenter

import net.sf.saxon.trans.XPathException

import net.sf.saxon.value._

import java.math.BigInteger

import java.util.ArrayList

import java.util.List

import scala.util.control.Breaks._


class NumberSequenceFormatter(value: Expression,
                              format: Expression,
                              groupSize: Expression,
                              groupSeparator: Expression,
                              letterValue: Expression,
                              ordinal: Expression,
                              startAt: Expression,
                              lang: Expression,
                              private var formatter: NumberFormatter,
                              private var backwardsCompatible: Boolean)
  extends Expression {

  private var valueOp: Operand = _

  private var formatOp: Operand = _

  private var groupSizeOp: Operand = _

  private var groupSeparatorOp: Operand = _

  private var letterValueOp: Operand = _

  private var ordinalOp: Operand = _

  private var startAtOp: Operand =
    new Operand(this, startAt, OperandRole.SINGLE_ATOMIC)

  private var langOp: Operand = _

  private var numberer: Numberer = null

  if (value != null) {
    valueOp = new Operand(this, value, OperandRole.SINGLE_ATOMIC)
  }

  if (format != null) {
    formatOp = new Operand(this, format, OperandRole.SINGLE_ATOMIC)
  }

  if (groupSize != null) {
    groupSizeOp = new Operand(this, groupSize, OperandRole.SINGLE_ATOMIC)
  }

  if (groupSeparator != null) {
    groupSeparatorOp =
      new Operand(this, groupSeparator, OperandRole.SINGLE_ATOMIC)
  }

  if (letterValue != null) {
    letterValueOp = new Operand(this, letterValue, OperandRole.SINGLE_ATOMIC)
  }

  if (ordinal != null) {
    ordinalOp = new Operand(this, ordinal, OperandRole.SINGLE_ATOMIC)
  }

  //}
  if (lang != null) {
    langOp = new Operand(this, lang, OperandRole.SINGLE_ATOMIC)
  }

  if (formatter == null && format.isInstanceOf[StringLiteral]) {
    this.formatter = new NumberFormatter()
    this.formatter.prepare(format.asInstanceOf[StringLiteral].getStringValue)
  }

  /**
   * Simplify an expression. This performs any static optimization (by rewriting the expression
   * as a different expression). The default implementation simplifies its operands.
   *
   * @return the simplified expression (or the original if unchanged, or if modified in-situ)
   * @throws XPathException if an error is discovered during expression
   *                        rewriting
   */
  override def simplify(): Expression = {
    if (valueOp != null &&
      !valueOp.getChildExpression.getItemType.isPlainType) {
      valueOp.setChildExpression(
        Atomizer.makeAtomizer(valueOp.getChildExpression, null))
    }
    preallocateNumberer(getConfiguration)
    super.simplify()
  }

  def preallocateNumberer(config: Configuration): Unit = {
    if (langOp == null) {
      numberer = config.makeNumberer(null, null)
    } else {
      if (langOp.getChildExpression.isInstanceOf[StringLiteral]) {
        val language: String =
          langOp.getChildExpression.asInstanceOf[StringLiteral].getStringValue
        if (!language.isEmpty) {
          val vf: ValidationFailure =
            StringConverter.StringToLanguage.INSTANCE.validate(language)
          if (vf != null) {
            langOp.setChildExpression(
              new StringLiteral(StringValue.EMPTY_STRING))
            throw new XPathException(
              "The lang attribute must be a valid language code",
              "XTDE0030")
          }
        }
        numberer = config.makeNumberer(language, null)
      }
    }
    // else we allocate a numberer at run-time
    // else we allocate a numberer at run-time
  }

  override def operands(): java.lang.Iterable[Operand] =
    operandSparseList(valueOp,
      formatOp,
      groupSizeOp,
      groupSeparatorOp,
      letterValueOp,
      ordinalOp,
      startAtOp,
      langOp)

  private def isFixed(op: Operand): Boolean =
    op == null || op.getChildExpression.isInstanceOf[Literal]

  private def hasFixedOperands(): Boolean =
    operands().asScala.find(!isFixed(_)).map(_ => false).getOrElse(true)

  override def optimize(visitor: ExpressionVisitor,
                        contextInfo: ContextItemStaticInfo): Expression = {
    optimizeChildren(visitor, contextInfo)
    if (hasFixedOperands()) {
      val `val`: StringValue = evaluateItem(visitor.makeDynamicContext())
      val literal: StringLiteral = new StringLiteral(`val`)
      ExpressionTool.copyLocationInfo(this, literal)
      literal
    } else {
      this
    }
  }

  /*@NotNull*/

  def copy(rebindings: RebindingMap): Expression = {
    val exp: NumberSequenceFormatter = new NumberSequenceFormatter(
      copy(valueOp, rebindings),
      copy(formatOp, rebindings),
      copy(groupSizeOp, rebindings),
      copy(groupSeparatorOp, rebindings),
      copy(letterValueOp, rebindings),
      copy(ordinalOp, rebindings),
      copy(startAtOp, rebindings),
      copy(langOp, rebindings),
      formatter,
      backwardsCompatible
    )
    ExpressionTool.copyLocationInfo(this, exp)
    exp
  }

  private def copy(op: Operand, rebindings: RebindingMap): Expression =
    if (op == null) null else op.getChildExpression.copy(rebindings)

  /*@NotNull*/

  def getItemType(): ItemType = BuiltInAtomicType.STRING

  def computeCardinality(): Int = StaticProperty.EXACTLY_ONE

  /**
   * An implementation of Expression must provide at least one of the methods evaluateItem(), iterate(), or process().
   * This method indicates which of these methods is provided directly. The other methods will always be available
   * indirectly, using an implementation that relies on one of the other methods.
   *
   * @return the implementation method, for example { @link #ITERATE_METHOD} or { @link #EVALUATE_METHOD} or
   *         { @link #PROCESS_METHOD}
   */
  override def getImplementationMethod(): Int = Expression.EVALUATE_METHOD

  override def evaluateItem(context: XPathContext): StringValue = {
    val value: Long = -1
    // a list whose items may be of type either Long or
    val vec: List[Any] = new ArrayList[Any](4)
    // BigInteger or the string to be output (e.g. "NaN")
    val rules: ConversionRules = context.getConfiguration.getConversionRules
    val startAv: String =
      startAtOp.getChildExpression.evaluateAsString(context).toString
    val startValues: List[Integer] = parseStartAtValue(startAv)
    val iter: SequenceIterator = valueOp.getChildExpression.iterate(context)
    var atomicVal: AtomicValue = null
    var pos: Int = 0
    breakable {
      while ( {
        atomicVal = iter.next().asInstanceOf[AtomicValue]
        atomicVal
      } != null) {
        if (backwardsCompatible && !vec.isEmpty) {
          break()
        }
        val startValue: Int =
          if (startValues.size > pos) startValues.get(pos)
          else startValues.get(startValues.size - 1)
        pos += 1
        try {
          var num: NumericValue = null
          num =
            if (atomicVal.isInstanceOf[NumericValue])
              atomicVal.asInstanceOf[NumericValue]
            else Number_1.convert(atomicVal, context.getConfiguration)
          if (num.isNaN) {
            // thrown to be caught
            throw new XPathException("NaN")
          }
          num = num.round(0)
          if (num.compareTo(Int64Value.MAX_LONG) > 0) {
            var bi: BigInteger = Converter
              .convert(num, BuiltInAtomicType.INTEGER, rules)
              .asAtomic()
              .asInstanceOf[BigIntegerValue]
              .asBigInteger()
            if (startValue != 1) {
              bi = bi.add(BigInteger.valueOf(startValue - 1))
            }
            vec.add(bi)
          } else {
            if (num.compareTo(Int64Value.ZERO) < 0) {
              throw new XPathException(
                "The numbers to be formatted must not be negative")
            }
            // thrown to be caught
            // thrown to be caught
            var i: Long = Converter
              .convert(num, BuiltInAtomicType.INTEGER, rules)
              .asAtomic()
              .asInstanceOf[NumericValue]
              .longValue()
            i += startValue - 1
            vec.add(i)
          }
        } catch {
          case err: XPathException =>
            if (backwardsCompatible) {
              vec.add("NaN")
            } else {
              vec.add(atomicVal.getStringValue)
              val e: XPathException = new XPathException(
                "Cannot convert supplied value to an integer. " + err.getMessage)
              e.setErrorCode("XTDE0980")
              e.setLocation(getLocation)
              e.setXPathContext(context)
              throw e
            }

        }
      }
    }
    if (backwardsCompatible && vec.isEmpty) {
      vec.add("NaN")
    }
    var gpsize: Int = 0
    var gpseparator: String = ""
    var letterVal: String = null
    var ordinalVal: String = null
    if (groupSizeOp != null) {
      val g: String =
        groupSizeOp.getChildExpression.evaluateAsString(context).toString
      try gpsize = java.lang.Integer.parseInt(g)
      catch {
        case err: NumberFormatException => {
          val e: XPathException = new XPathException(
            "grouping-size must be numeric")
          e.setXPathContext(context)
          e.setErrorCode("XTDE0030")
          e.setLocation(getLocation)
          throw e
        }

      }
    }
    if (groupSeparatorOp != null) {
      gpseparator =
        groupSeparatorOp.getChildExpression.evaluateAsString(context).toString
    }
    if (ordinalOp != null) {
      ordinalVal =
        ordinalOp.getChildExpression.evaluateAsString(context).toString
    }
    // add it to the table.
    var numb: Numberer = numberer
    if (numb == null) {
      if (langOp == null) {
        numb = context.getConfiguration.makeNumberer(null, null)
      } else {
        val language: String =
          langOp.getChildExpression.evaluateAsString(context).toString
        val vf: ValidationFailure =
          StringConverter.StringToLanguage.INSTANCE.validate(language)
        if (vf != null) {
          throw new XPathException(
            "The lang attribute of xsl:number must be a valid language code",
            "XTDE0030")
        }
        numb = context.getConfiguration.makeNumberer(language, null)
      }
    }
    if (letterValueOp == null) {
      letterVal = ""
    } else {
      letterVal =
        letterValueOp.getChildExpression.evaluateAsString(context).toString
      if (!("alphabetic" == letterVal || "traditional" == letterVal)) {
        val e: XPathException = new XPathException(
          "letter-value must be \"traditional\" or \"alphabetic\"")
        e.setXPathContext(context)
        e.setErrorCode("XTDE0030")
        e.setLocation(getLocation)
        throw e
      }
    }
    var nf: NumberFormatter = null
    if (formatter == null) {
      // format not known until run-time
      nf = new NumberFormatter()
      nf.prepare(
        formatOp.getChildExpression.evaluateAsString(context).toString)
    } else {
      nf = formatter
    }
    val s: CharSequence =
      nf.format(vec, gpsize, gpseparator, letterVal, ordinalVal, numb)
    new StringValue(s)
  }

  // Use the numberer decided at compile time if possible; otherwise try to get it from
  // a table of numberers indexed by language; if not there, load the relevant class and
  // Use the numberer decided at compile time if possible; otherwise try to get it from
  // a table of numberers indexed by language; if not there, load the relevant class and

  def parseStartAtValue(value: String): List[Integer] = {
    val list: List[Integer] = new ArrayList[Integer]()
    val tokens: Array[String] = value.split("\\s+")
    for (tok <- tokens) {
      try {
        val n: Int = java.lang.Integer.parseInt(tok)
        list.add(n)
      } catch {
        case err: NumberFormatException => {
          val e: XPathException = new XPathException(
            "Invalid start-at value: non-integer component {" + tok +
              "}")
          e.setErrorCode("XTDE0030")
          e.setLocation(getLocation)
          throw e
        }

      }
    }
    if (list.isEmpty) {
      val e: XPathException = new XPathException(
        "Invalid start-at value: no numeric components found")
      e.setErrorCode("XTDE0030")
      e.setLocation(getLocation)
      throw e
    }
    list
  }

  def export(out: ExpressionPresenter): Unit = {
    out.startElement("numSeqFmt", this)
    var flags: String = ""
    if (backwardsCompatible) {
      flags += "1"
    }
    if (!flags.isEmpty) {
      out.emitAttribute("flags", flags)
    }
    if (valueOp != null) {
      out.setChildRole("value")
      valueOp.getChildExpression.export(out)
    }
    if (formatOp != null) {
      out.setChildRole("format")
      formatOp.getChildExpression.export(out)
    }
    if (startAtOp != null) {
      out.setChildRole("startAt")
      startAtOp.getChildExpression.export(out)
    }
    if (langOp != null) {
      out.setChildRole("lang")
      langOp.getChildExpression.export(out)
    }
    if (ordinalOp != null) {
      out.setChildRole("ordinal")
      ordinalOp.getChildExpression.export(out)
    }
    if (groupSeparatorOp != null) {
      out.setChildRole("gpSep")
      groupSeparatorOp.getChildExpression.export(out)
    }
    if (groupSizeOp != null) {
      out.setChildRole("gpSize")
      groupSizeOp.getChildExpression.export(out)
    }
    out.endElement()
  }

}

// Copyright (c) 2018-2020 Saxonica Limited
// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is "Incompatible With Secondary Licenses", as defined by the Mozilla Public License, v. 2.0.
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
/**
 * This expression performs the formatting part of the logic of the xsl:number instruction
 * It takes as input a sequence of integers, which may either be supplied directly as the
 * value attribute of xsl:number, or may be computed by counting nodes. The expression
 * returns a string.
 */
