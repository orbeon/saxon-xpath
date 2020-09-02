////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
package net.sf.saxon.event

import net.sf.saxon.lib.NamespaceConstant

import net.sf.saxon.model.ComplexType

import net.sf.saxon.model.SchemaType

import net.sf.saxon.model.Untyped

import net.sf.saxon.om._

import net.sf.saxon.s9api.Location

import net.sf.saxon.trace.ExpressionPresenter

import net.sf.saxon.trans.XPathException

import net.sf.saxon.trans.rules.Rule

import net.sf.saxon.trans.rules.RuleTarget

import net.sf.saxon.value.Whitespace

import java.util.Arrays

import Stripper._


object Stripper {

  val STRIP: StripRuleTarget = new StripRuleTarget() {}

  val PRESERVE: StripRuleTarget = new StripRuleTarget() {}

  // whitespace always preserved (e.g. xsl:text)
  val ALWAYS_PRESERVE: Byte = 0x01

  // whitespace always stripped (e.g. xsl:choose)
  val ALWAYS_STRIP: Byte = 0x02

  // no special action
  val STRIP_DEFAULT: Byte = 0x00

  // parent element specifies xml:space="preserve"
  val PRESERVE_PARENT: Byte = 0x04

  // type annotation indicates simple typed content
  val SIMPLE_CONTENT: Byte = 0x08

  // XSD 1.1 assertions are in scope
  val ASSERTIONS_EXIST: Byte = 0x10

  private var XML_SPACE: NodeName = new FingerprintedQName(
    "xml",
    NamespaceConstant.XML,
    "space",
    StandardNames.XML_SPACE)

  class StripRuleTarget extends RuleTarget {

    def export(presenter: ExpressionPresenter): Unit = ()

    // no-op
    // no-op

    def registerRule(rule: Rule): Unit = ()

    // no action
    // no action

  }

}

class Stripper( var rule: SpaceStrippingRule, next: Receiver)
  extends ProxyReceiver(next) {

  assert(rule != null)

  private var stripStack: Array[Byte] = new Array[Byte](100)

  private var top: Int = 0

  def getAnother(next: Receiver): Stripper = new Stripper(rule, next)

  private def isSpacePreserving(name: NodeName, `type`: SchemaType): Int =
    rule.isSpacePreserving(name, `type`)

  override def open(): Unit = {
    // System.err.println("Stripper#startDocument()");
    top = 0
    // {xml:preserve = false, preserve this element = true}
    stripStack(top) = ALWAYS_PRESERVE
    super.open()
  }

  override def startElement(elemName: NodeName,
                            `type`: SchemaType,
                            attributes: AttributeMap,
                            namespaces: NamespaceMap,
                            location: Location,
                            properties: Int): Unit = {
    // System.err.println("startElement " + nameCode);
    nextReceiver.startElement(elemName,
      `type`,
      attributes,
      namespaces,
      location,
      properties)
    val preserveParent: Byte = stripStack(top)
    var preserve: Byte =
      (preserveParent & (PRESERVE_PARENT | ASSERTIONS_EXIST)).toByte
    val elementStrip: Int = isSpacePreserving(elemName, `type`)
    if (elementStrip == ALWAYS_PRESERVE) {
      preserve = (preserve | ALWAYS_PRESERVE).toByte
    } else if (elementStrip == ALWAYS_STRIP) {
      preserve = (preserve | ALWAYS_STRIP).toByte
    }
    if (`type` != Untyped.getInstance) {
      if (preserve == 0) {
        // if the element has simple content, whitespace stripping is disabled
        if (`type`.isSimpleType || `type`
          .asInstanceOf[ComplexType]
          .isSimpleContent) {
          preserve = (preserve | SIMPLE_CONTENT).toByte
        }
      }
      if (`type`.isInstanceOf[ComplexType] && `type`
        .asInstanceOf[ComplexType]
        .hasAssertions) {
        preserve = (preserve | ASSERTIONS_EXIST).toByte
      }
    }
    top += 1
    if (top >= stripStack.length) {
      stripStack = Arrays.copyOf(stripStack, top * 2)
    }
    stripStack(top) = preserve
    val xmlSpace: String = attributes.getValue(NamespaceConstant.XML, "space")
    if (xmlSpace != null) {
      if (Whitespace.normalizeWhitespace(xmlSpace).==("preserve")) {
        stripStack(top) = (stripStack(top) | PRESERVE_PARENT).toByte
      } else {
        stripStack(top) = (stripStack(top) & ~PRESERVE_PARENT).toByte
      }
    }
  }

  // put "preserve" value on top of stack
  // put "preserve" value on top of stack

  override def endElement(): Unit = {
    nextReceiver.endElement()
    top -= 1
  }

  override def characters(chars: CharSequence,
                          locationId: Location,
                          properties: Int): Unit = {
    if (((((stripStack(top) &
      (ALWAYS_PRESERVE | PRESERVE_PARENT | SIMPLE_CONTENT | ASSERTIONS_EXIST)) !=
      0) &&
      (stripStack(top) & ALWAYS_STRIP) == 0) ||
      !Whitespace.isWhite(chars)) &&
      chars.length > 0) {
      nextReceiver.characters(chars, locationId, properties)
    }
  }

  override def usesTypeAnnotations: Boolean = true

}
