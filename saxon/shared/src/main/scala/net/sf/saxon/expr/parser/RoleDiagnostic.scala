package net.sf.saxon.expr.parser

import net.sf.saxon.expr.Expression

import net.sf.saxon.expr.Literal

import net.sf.saxon.expr.StaticProperty

import net.sf.saxon.model.ItemType

import net.sf.saxon.model.TypeHierarchy

import net.sf.saxon.model.UType

import net.sf.saxon.om.AxisInfo

import net.sf.saxon.om.Item

import net.sf.saxon.trans.Err

import net.sf.saxon.tree.util.FastStringBuffer

import net.sf.saxon.value.SequenceType

import java.util.Optional

import RoleDiagnostic._

import scala.beans.{BeanProperty, BooleanBeanProperty}

object RoleDiagnostic {

  val FUNCTION: Int = 0

  val BINARY_EXPR: Int = 1

  val TYPE_OP: Int = 2

  val VARIABLE: Int = 3

  val INSTRUCTION: Int = 4

  val FUNCTION_RESULT: Int = 5

  val ORDER_BY: Int = 6

  val TEMPLATE_RESULT: Int = 7

  val PARAM: Int = 8

  val UNARY_EXPR: Int = 9

  val UPDATING_EXPR: Int = 10

  val EVALUATE_RESULT: Int = 12

  val CONTEXT_ITEM: Int = 13

  val AXIS_STEP: Int = 14

  val OPTION: Int = 15

  val CHARACTER_MAP_EXPANSION: Int = 16

  val MATCH_PATTERN: Int = 19

  val MISC: Int = 20

  def reconstruct(in: String): RoleDiagnostic = {
    val v: Int = in.indexOf('|')
    val kind: Int = java.lang.Integer.parseInt(in.substring(0, v))
    val w: Int = in.indexOf('|', v + 1)
    val operand: Int = java.lang.Integer.parseInt(in.substring(v + 1, w))
    val x: Int = in.indexOf('|', w + 1)
    val errorCode: String = in.substring(w + 1, x)
    val operation: String = in.substring(x + 1)
    val cd: RoleDiagnostic = new RoleDiagnostic(kind, operation, operand)
    if (!errorCode.isEmpty) {
      cd.setErrorCode(errorCode)
    }
    cd
  }

  def ordinal(n: Int): String = n match {
    case 1 => "first"
    case 2 => "second"
    case 3 => "third"
    case _ =>
      if (n >= 21) {
        n % 10 match {
          case 1 => n + "st"
          case 2 => n + "nd"
          case 3 => n + "rd"

        }
      }
      n + "th"

  }

}

class RoleDiagnostic(private var kind: Int,
                     private var operation: String,
                     private var operand: Int) {

  var errorCode: String = "XPTY0004"

  def setErrorCode(code: String): Unit = {
    if (code != null) {
      this.errorCode = code
    }
  }

  def getErrorCode: String = errorCode

  def isTypeError(): Boolean =
    !errorCode.startsWith("FORG") && errorCode.!=("XPDY0050")

  def getMessage(): String = {
    var name: String = operation
    kind match {
      case FUNCTION =>
        if (name.==("saxon:call") || name.==("saxon:apply")) {
          if (operand == 0) {
            "target of the dynamic function call"
          } else {
            ordinal(operand) + " argument of the dynamic function call"
          }
        } else {
          ordinal(operand + 1) + " argument of " +
            (if (name.isEmpty) "the anonymous function" else name + "()")
        }
      case BINARY_EXPR => ordinal(operand + 1) + " operand of '" + name + '\''
      case UNARY_EXPR => "operand of '-'"
      case TYPE_OP => "value in '" + name + "' expression"
      case VARIABLE =>
        if (name.==("saxon:context-item")) {
          "context item"
        } else {
          "value of variable $" + name
        }
      case INSTRUCTION =>
        var slash: Int = name.indexOf('/')
        var attributeName: String = ""
        if (slash >= 0) {
          attributeName = name.substring(slash + 1)
          name = name.substring(0, slash)
        }
        "@" + attributeName + " attribute of " +
          (if (name.==("LRE")) "a literal result element" else name)
      case FUNCTION_RESULT =>
        if (name.isEmpty) {
          "result of the anonymous function"
        } else {
          "result of a call to " + name
        }
      case TEMPLATE_RESULT => "result of template " + name
      case ORDER_BY => ordinal(operand + 1) + " sort key"
      case PARAM => "value of parameter $" + name
      case UPDATING_EXPR =>
        "value of the " + ordinal(operand + 1) + " operand of " +
          name +
          " expression"
      case EVALUATE_RESULT =>
        "result of the expression {" + name + "} evaluated by xsl:evaluate"
      case CONTEXT_ITEM => "context item"
      case AXIS_STEP =>
        "context item for the " + AxisInfo.axisName(operand) +
          " axis"
      case OPTION => "value of the " + name + " option"
      case CHARACTER_MAP_EXPANSION =>
        "substitute value for character '" + name + "' in the character map"
      case MATCH_PATTERN => "match pattern"
      case MISC => operation
      case _ => ""

    }
  }

  def composeRequiredMessage(requiredItemType: ItemType): String =
    "The required item type of the " + getMessage + " is " +
      requiredItemType

  def composeErrorMessage(requiredItemType: ItemType,
                          suppliedItemType: ItemType): String =
    composeRequiredMessage(requiredItemType) + "; supplied value has item type " +
      suppliedItemType

  def composeErrorMessage(requiredItemType: ItemType,
                          supplied: Expression,
                          th: TypeHierarchy): String = {
    if (supplied.isInstanceOf[Literal]) {
      var s: String = composeRequiredMessage(requiredItemType)
      val more: Optional[String] = SequenceType
        .makeSequenceType(requiredItemType, StaticProperty.ALLOWS_ZERO_OR_MORE)
        .explainMismatch(supplied.asInstanceOf[Literal].value, th)
      if (more.isPresent) {
        s = s + ". " + more.get
      }
      return s
    }
    composeRequiredMessage(requiredItemType) + ", but the supplied expression {" +
      supplied.toShortString() +
      "} has item type " +
      supplied.getItemType
  }

  def composeErrorMessage(requiredItemType: ItemType,
                          item: Item,
                          th: TypeHierarchy): String = {
    val message: FastStringBuffer = new FastStringBuffer(256)
    message.append(composeRequiredMessage(requiredItemType))
    message.append("; the supplied value ")
    message.cat(Err.depict(item))
    if (requiredItemType.getGenre != item.getGenre) {
      message.append(" is ")
      message.append(item.getGenre.getDescription)
    } else {
      message.append(" does not match. ")
      if (th != null) {
        val more: Optional[String] = requiredItemType.explainMismatch(item, th)
        more.ifPresent(message.append)
      }
    }
    message.toString
  }

  def composeErrorMessage(requiredItemType: ItemType,
                          suppliedItemType: UType): String =
    composeRequiredMessage(requiredItemType) + "; supplied value has item type " +
      suppliedItemType

  def save(): String = {
    val fsb: FastStringBuffer = new FastStringBuffer(FastStringBuffer.C256)
    fsb.append(kind + "|")
    fsb.append(operand + "|")
    fsb.append(if (errorCode.==("XPTY0004")) "" else errorCode)
    fsb.append("|")
    fsb.append(operation)
    fsb.toString
  }

}
