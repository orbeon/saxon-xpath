package org.orbeon.saxon.expr.parser

import org.orbeon.saxon.expr.parser.RoleDiagnostic._
import org.orbeon.saxon.expr.{Expression, Literal, StaticProperty}
import org.orbeon.saxon.model.{ItemType, TypeHierarchy, UType}
import org.orbeon.saxon.om.{AxisInfo, Item}
import org.orbeon.saxon.trans.Err
import org.orbeon.saxon.tree.util.FastStringBuffer
import org.orbeon.saxon.value.SequenceType


object RoleDiagnostic {

  val FUNCTION                : Int = 0
  val BINARY_EXPR             : Int = 1
  val TYPE_OP                 : Int = 2
  val VARIABLE                : Int = 3
  val INSTRUCTION             : Int = 4
  val FUNCTION_RESULT         : Int = 5
  val ORDER_BY                : Int = 6
  val TEMPLATE_RESULT         : Int = 7
  val PARAM                   : Int = 8
  val UNARY_EXPR              : Int = 9
  val UPDATING_EXPR           : Int = 10
  val EVALUATE_RESULT         : Int = 12
  val CONTEXT_ITEM            : Int = 13
  val AXIS_STEP               : Int = 14
  val OPTION                  : Int = 15
  val CHARACTER_MAP_EXPANSION : Int = 16
  val MATCH_PATTERN           : Int = 19
  val MISC                    : Int = 20

  def reconstruct(in: String): RoleDiagnostic = {
    val v         = in.indexOf('|')
    val kind      = java.lang.Integer.parseInt(in.substring(0, v))
    val w         = in.indexOf('|', v + 1)
    val operand   = java.lang.Integer.parseInt(in.substring(v + 1, w))
    val x         = in.indexOf('|', w + 1)
    val errorCode = in.substring(w + 1, x)
    val operation = in.substring(x + 1)
    val cd        = new RoleDiagnostic(kind, operation, operand)
    if (errorCode.nonEmpty)
      cd.setErrorCode(errorCode)
    cd
  }

  def ordinal(n: Int): String = n match {
    case 1 => "first"
    case 2 => "second"
    case 3 => "third"
    case _ =>
      if (n >= 21) {
        n % 10 match {
          case 1 => return n.toString + "st"
          case 2 => return n.toString + "nd"
          case 3 => return n.toString + "rd"
        }
      }
      n.toString + "th"
  }
}

class RoleDiagnostic(private var kind: Int,
                     private var operation: String,
                     private var operand: Int) {

  var errorCode: String = "XPTY0004"

  def setErrorCode(code: String): Unit =
    if (code != null)
      this.errorCode = code

  def getErrorCode: String = errorCode

  def isTypeError: Boolean =
    ! errorCode.startsWith("FORG") && errorCode.!=("XPDY0050")

  def getMessage: String = {
    var name = operation
    kind match {
      case FUNCTION =>
        if (name == "saxon:call" || name == "saxon:apply") {
          if (operand == 0) {
            "target of the dynamic function call"
          } else {
            ordinal(operand) + " argument of the dynamic function call"
          }
        } else
          ordinal(operand + 1) + " argument of " + (if (name.isEmpty) "the anonymous function" else name + "()")
      case BINARY_EXPR => ordinal(operand + 1) + " operand of '" + name + '\''
      case UNARY_EXPR => "operand of '-'"
      case TYPE_OP => "value in '" + name + "' expression"
      case VARIABLE =>
        if (name == "saxon:context-item")
          "context item"
        else
          "value of variable $" + name
      case INSTRUCTION =>
        val slash = name.indexOf('/')
        var attributeName = ""
        if (slash >= 0) {
          attributeName = name.substring(slash + 1)
          name = name.substring(0, slash)
        }
        "@" + attributeName + " attribute of " + (if (name.==("LRE")) "a literal result element" else name)
      case FUNCTION_RESULT =>
        if (name.isEmpty)
          "result of the anonymous function"
        else
          "result of a call to " + name
      case TEMPLATE_RESULT => "result of template " + name
      case ORDER_BY => ordinal(operand + 1) + " sort key"
      case PARAM => "value of parameter $" + name
      case UPDATING_EXPR =>
        "value of the " + ordinal(operand + 1) + " operand of " + name + " expression"
      case EVALUATE_RESULT =>
        "result of the expression {" + name + "} evaluated by xsl:evaluate"
      case CONTEXT_ITEM => "context item"
      case AXIS_STEP =>
        "context item for the " + AxisInfo.axisName(operand) + " axis"
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
    supplied match {
      case literal: Literal =>
        var s    = composeRequiredMessage(requiredItemType)
        val more = SequenceType
          .makeSequenceType(requiredItemType, StaticProperty.ALLOWS_ZERO_OR_MORE)
          .explainMismatch(literal.value, th)
        if (more.isDefined)
          s = s + ". " + more.get
        return s
      case _ =>
    }
    composeRequiredMessage(requiredItemType) + ", but the supplied expression {" +
      supplied.toShortString + "} has item type " + supplied.getItemType
  }

  def composeErrorMessage(requiredItemType: ItemType,
                          item: Item,
                          th: TypeHierarchy): String = {
    val message = new FastStringBuffer(256)
    message.append(composeRequiredMessage(requiredItemType))
    message.append("; the supplied value ")
    message.cat(Err.depict(item))
    if (requiredItemType.getGenre != item.getGenre) {
      message.append(" is ")
      message.append(item.getGenre.getDescription)
    } else {
      message.append(" does not match. ")
      if (th != null) {
        val more = requiredItemType.explainMismatch(item, th)
        more foreach message.append
      }
    }
    message.toString
  }

  def composeErrorMessage(requiredItemType: ItemType,
                          suppliedItemType: UType): String =
    composeRequiredMessage(requiredItemType) + "; supplied value has item type " +
      suppliedItemType

  def save(): String = {
    val fsb = new FastStringBuffer(FastStringBuffer.C256)
    fsb.append(kind.toString + "|")
    fsb.append(operand.toString + "|")
    fsb.append(if (errorCode.==("XPTY0004")) "" else errorCode)
    fsb.append("|")
    fsb.append(operation)
    fsb.toString
  }
}
