package net.sf.saxon.expr.instruct

import net.sf.saxon.utils.Controller

import net.sf.saxon.event.Outputter

import net.sf.saxon.event.ProxyOutputter

import net.sf.saxon.event.ReceiverOption

import net.sf.saxon.expr._

import net.sf.saxon.expr.parser._

import net.sf.saxon.functions.SystemFunction

import net.sf.saxon.model._

import net.sf.saxon.om.GroundedValue

import net.sf.saxon.om.Item

import net.sf.saxon.om.NodeInfo

import net.sf.saxon.om.StandardNames

import net.sf.saxon.pattern.NodeKindTest

import net.sf.saxon.s9api.Location

import net.sf.saxon.trace.ExpressionPresenter

import net.sf.saxon.trans.Err

import net.sf.saxon.trans.XPathException

import net.sf.saxon.tree.util.CharSequenceConsumer

import net.sf.saxon.tree.util.Orphan

import net.sf.saxon.value.Cardinality

import net.sf.saxon.value.StringValue

import net.sf.saxon.value.Whitespace

import java.util.function.BiConsumer

import scala.beans.{BeanProperty, BooleanBeanProperty}
import scala.util.control.Breaks._

class ValueOf(select: Expression,
              disable: Boolean,
              @BooleanBeanProperty var noNodeIfEmpty: Boolean)
  extends SimpleNodeConstructor {

  @BeanProperty
  var options: Int = if (disable) ReceiverOption.DISABLE_ESCAPING else ReceiverOption.NONE

  var isNumberingInstruction: Boolean = false

  this.setSelect(select)

  adoptChildExpression(select)

  if (select.isInstanceOf[StringLiteral]) {
    var special: Boolean = false
    val `val`: CharSequence = select.asInstanceOf[StringLiteral].getStringValue
    breakable {
      for (k <- 0 until `val`.length) {
        val c: Char = `val`.charAt(k)
        if (c.toInt < 33 || c.toInt > 126 || c == '<' || c == '>' ||
          c == '&') {
          special = true
          break()
        }
      }
    }
    if (!special) {
      options |= ReceiverOption.NO_SPECIAL_CHARS
    }
  }

  def setIsNumberingInstruction(): Unit = {
    isNumberingInstruction = true
  }

  override def toShortString: String =
    if (getSelect.isInstanceOf[StringLiteral]) {
      "text{" +
        Err.depict(getSelect.asInstanceOf[StringLiteral].getValue) +
        "}"
    } else {
      super.toShortString
    }

  override def gatherProperties(consumer: BiConsumer[String, Any]): Unit = {
    if (getSelect.isInstanceOf[StringLiteral]) {
      consumer.accept(
        "text",
        (getSelect.asInstanceOf[StringLiteral].getValue.getStringValue))
    }
  }

  override def getInstructionNameCode(): Int =
    if (isNumberingInstruction) {
      StandardNames.XSL_NUMBER
    } else if (getSelect.isInstanceOf[StringLiteral]) {
      StandardNames.XSL_TEXT
    } else {
      StandardNames.XSL_VALUE_OF
    }

  def isDisableOutputEscaping: Boolean =
    ReceiverOption.contains(options, ReceiverOption.DISABLE_ESCAPING)

  override def getItemType: ItemType = NodeKindTest.TEXT

  override def computeCardinality(): Int =
    if (noNodeIfEmpty) {
      StaticProperty.ALLOWS_ZERO_OR_ONE
    } else {
      StaticProperty.EXACTLY_ONE
    }

  def localTypeCheck(visitor: ExpressionVisitor,
                     contextItemType: ContextItemStaticInfo): Unit = ()

  override def getIntrinsicDependencies: Int = {
    var d: Int = super.getIntrinsicDependencies
    if (isDisableOutputEscaping) {
      d |= StaticProperty.DEPENDS_ON_ASSIGNABLE_GLOBALS
    }
    d
  }

  def copy(rebindings: RebindingMap): Expression = {
    val exp: ValueOf = new ValueOf(
      getSelect.copy(rebindings),
      ReceiverOption.contains(options, ReceiverOption.DISABLE_ESCAPING),
      noNodeIfEmpty)
    ExpressionTool.copyLocationInfo(this, exp)
    if (isNumberingInstruction) {
      exp.setIsNumberingInstruction()
    }
    exp
  }

  override def checkPermittedContents(parentType: SchemaType, whole: Boolean): Unit = {
    if (getSelect.isInstanceOf[Literal]) {
      val selectValue: GroundedValue = getSelect.asInstanceOf[Literal].value
      var stype: SimpleType = null
      if (parentType.isInstanceOf[SimpleType] && whole) {
        stype = parentType.asInstanceOf[SimpleType]
      } else if (parentType.isInstanceOf[ComplexType] &&
        parentType.asInstanceOf[ComplexType].isSimpleContent) {
        stype = parentType.asInstanceOf[ComplexType].getSimpleContentType
      }
      if (whole && stype != null && !stype.isNamespaceSensitive) {
        val err: ValidationFailure = stype.validateContent(
          selectValue.getStringValue,
          null,
          getConfiguration.getConversionRules)
        if (err != null) {
          err.setLocator(getLocation)
          err.setErrorCode(if (isXSLT) "XTTE1540" else "XQDY0027")
          throw err.makeException()
        }
        return
      }
      if (parentType.isInstanceOf[ComplexType] &&
        !parentType.asInstanceOf[ComplexType].isSimpleContent &&
        !parentType.asInstanceOf[ComplexType].isMixedContent &&
        !Whitespace.isWhite(selectValue.getStringValue)) {
        val err = new XPathException(
          "The containing element must be of type " + parentType.getDescription +
            ", which does not allow text content " +
            Err.wrap(selectValue.getStringValue))
        err.setLocation(getLocation)
        err.setIsTypeError(true)
        throw err
      }
    }
  }

  def convertToCastAsString(): Expression =
    if (noNodeIfEmpty || !Cardinality.allowsZero(getSelect.getCardinality)) {
      new CastExpression(getSelect, BuiltInAtomicType.UNTYPED_ATOMIC, true)
    } else {
      val sf: Expression =
        SystemFunction.makeCall("string", getRetainedStaticContext, getSelect)
      new CastExpression(sf, BuiltInAtomicType.UNTYPED_ATOMIC, false)
    }

  override def processLeavingTail(output: Outputter, context: XPathContext): TailCall =
    if (noNodeIfEmpty) {
      val value: StringValue =
        getSelect.evaluateItem(context).asInstanceOf[StringValue]
      if (value != null) {
        processValue(value.getStringValueCS, output, context)
      }
      null
    } else if (getSelect.getItemType == BuiltInAtomicType.STRING && !isDisableOutputEscaping) {
      val toText: Outputter = new ProxyOutputter(output) {
        override def append(item: Item): Unit = {
          getNextOutputter.characters(item.getStringValueCS, Loc.NONE, options)
        }

        override def append(item: Item,
                            locationId: Location,
                            properties: Int): Unit = {
          getNextOutputter.characters(item.getStringValueCS,
            locationId,
            properties | options)
        }

        override def getStringReceiver(
                                        asTextNode: Boolean): CharSequenceConsumer =
          getNextOutputter.getStringReceiver(true)
      }
      getSelect.process(toText, context)
      null
    } else {
      super.processLeavingTail(output, context)
    }

  def processValue(value: CharSequence,
                   output: Outputter,
                   context: XPathContext): Unit = {
    output.characters(value, getLocation, options)
  }

  override def evaluateItem(context: XPathContext): NodeInfo =
    try {
      var `val`: CharSequence = null
      val item: Item = getSelect.evaluateItem(context)
      if (item == null) {
        if (noNodeIfEmpty) {
          return null
        } else {
          `val` = ""
        }
      } else {
        `val` = item.getStringValueCS
      }
      val controller: Controller = context.getController
      assert(controller != null)
      val o: Orphan = new Orphan(controller.getConfiguration)
      o.setNodeKind(Type.TEXT)
      o.setStringValue(`val`)
      if (isDisableOutputEscaping) {
        o.setDisableOutputEscaping(true)
      }
      o
    } catch {
      case err: XPathException => {
        err.maybeSetLocation(getLocation)
        throw err
      }

    }

  def export(out: ExpressionPresenter): Unit = {
    out.startElement("valueOf", this)
    var flags: String = ""
    if (isDisableOutputEscaping) {
      flags += "d"
    }
    if (ReceiverOption.contains(options, ReceiverOption.NO_SPECIAL_CHARS)) {
      flags += "S"
    }
    if (noNodeIfEmpty) {
      flags += "e"
    }
    if (isLocal) {
      flags += "l"
    }
    if (!flags.isEmpty) {
      out.emitAttribute("flags", flags)
    }
    getSelect.export(out)
    out.endElement()
  }

}
