package net.sf.saxon.s9api

import net.sf.saxon.expr.StaticContext

import net.sf.saxon.om.StructuredQName

import net.sf.saxon.s9api.streams.Step

import net.sf.saxon.sxpath.IndependentContext

import net.sf.saxon.sxpath.XPathExpression

import net.sf.saxon.sxpath.XPathVariable

import java.util.Iterator

import java.util.LinkedHashMap

import java.util.Map

import java.util.stream.Stream

import OccurrenceIndicator._

class XPathExecutable(private var exp: XPathExpression,
                      private var processor: Processor,
                      private var env: IndependentContext) {

  def load(): XPathSelector = {
    val declaredVariables: Map[StructuredQName, XPathVariable] =
      new LinkedHashMap[StructuredQName, XPathVariable]()
    var iter: Iterator[XPathVariable] = env.iterateExternalVariables()
    while (iter.hasNext) {
      val `var`: XPathVariable = iter.next()
      declaredVariables.put(`var`.getVariableQName, `var`)
    }
    new XPathSelector(exp, declaredVariables)
  }

  def asStep(): Step[XdmItem] = new Step[XdmItem]() {
    override def apply(item: XdmItem): Stream[_ <: XdmItem] = {
      val selector: XPathSelector = load()
      selector.setContextItem(item)
      var result: XdmSequenceIterator[XdmItem] = selector.iterator()
      result.asInstanceOf[XdmItem].stream()
    }
  }

  def getResultItemType: ItemType = {
    val it: net.sf.saxon.model.ItemType = exp.getInternalExpression.getItemType
    new ConstructedItemType(it, processor)
  }

  def getResultCardinality: OccurrenceIndicator = {
    val card: Int = exp.getInternalExpression.getCardinality
    OccurrenceIndicator.getOccurrenceIndicator(card)
  }

  def iterateExternalVariables(): Iterator[QName] = {
    val varIterator: Iterator[XPathVariable] = env.iterateExternalVariables()
    new Iterator[QName]() {
      def hasNext(): Boolean = varIterator.hasNext

      def next(): QName =
        new QName(
          varIterator.next().getVariableQName)

      override def remove(): Unit = {
        throw new UnsupportedOperationException("remove")
      }
    }
  }

  def getRequiredItemTypeForVariable(variableName: QName): ItemType = {
    val `var`: XPathVariable =
      env.getExternalVariable(variableName.getStructuredQName)
    if (`var` == null) {
      null
    } else {
      new ConstructedItemType(`var`.getRequiredType.getPrimaryType, processor)
    }
  }

  def getRequiredCardinalityForVariable(variableName: QName): OccurrenceIndicator = {
    val `var`: XPathVariable =
      env.getExternalVariable(variableName.getStructuredQName)
    if (`var` == null) {
      null
    } else {
      OccurrenceIndicator.getOccurrenceIndicator(
        `var`.getRequiredType.getCardinality)
    }
  }

  def getUnderlyingExpression: XPathExpression = exp

  def getUnderlyingStaticContext: StaticContext = env

}
