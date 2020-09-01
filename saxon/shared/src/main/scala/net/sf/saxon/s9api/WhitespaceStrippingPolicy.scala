package net.sf.saxon.s9api

import net.sf.saxon.event.FilterFactory

import net.sf.saxon.event.ProxyReceiver

import net.sf.saxon.event.Receiver

import net.sf.saxon.event.Stripper

import net.sf.saxon.model.SchemaType

import net.sf.saxon.om._

import net.sf.saxon.style.StylesheetPackage

import net.sf.saxon.trace.ExpressionPresenter

import net.sf.saxon.trans.XPathException

import net.sf.saxon.value.Whitespace

import java.util.function.Predicate

object WhitespaceStrippingPolicy {

  val NONE: WhitespaceStrippingPolicy = new WhitespaceStrippingPolicy(
    Whitespace.NONE)

  val IGNORABLE: WhitespaceStrippingPolicy = new WhitespaceStrippingPolicy(
    Whitespace.IGNORABLE)

  val ALL: WhitespaceStrippingPolicy = new WhitespaceStrippingPolicy(
    Whitespace.ALL)

  val UNSPECIFIED: WhitespaceStrippingPolicy = new WhitespaceStrippingPolicy(
    Whitespace.UNSPECIFIED)

  def makeCustomPolicy(
                        elementTest: Predicate[QName]): WhitespaceStrippingPolicy = {
    val rule: SpaceStrippingRule = new SpaceStrippingRule() {
      override def isSpacePreserving(nodeName: NodeName,
                                     schemaType: SchemaType): Int =
        if (elementTest.test(new QName(nodeName.getStructuredQName)))
          Stripper.ALWAYS_STRIP
        else Stripper.ALWAYS_PRESERVE

      override def makeStripper(next: Receiver): ProxyReceiver =
        new Stripper(this, next)

      override def export(presenter: ExpressionPresenter): Unit = {
        throw new UnsupportedOperationException()
      }
    }
    val wsp: WhitespaceStrippingPolicy = new WhitespaceStrippingPolicy(
      Whitespace.XSLT)
    wsp.stripperRules = rule
    wsp
  }

}

class WhitespaceStrippingPolicy(private var policy: Int) {

  private var stripperRules: SpaceStrippingRule = _

  policy match {
    case Whitespace.ALL =>
      stripperRules = AllElementsSpaceStrippingRule.getInstance
    case Whitespace.NONE =>
      stripperRules = NoElementsSpaceStrippingRule.getInstance
    case Whitespace.IGNORABLE =>
      stripperRules = IgnorableSpaceStrippingRule.getInstance
    case _ =>

  }

  def this(pack: StylesheetPackage) = {
    this(0)
    policy = Whitespace.XSLT
    stripperRules = pack.getStripperRules
  }

   def ordinal(): Int = policy

   def getSpaceStrippingRule: SpaceStrippingRule = stripperRules

   def makeStripper(): FilterFactory = new FilterFactory() {
    def makeFilter(next: Receiver): ProxyReceiver =
      new Stripper(stripperRules, next)
  }

}
