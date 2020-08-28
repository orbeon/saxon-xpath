package net.sf.saxon.s9api.streams

import java.util

import net.sf.saxon.s9api.XdmAtomicValue
import net.sf.saxon.s9api.XdmItem
import net.sf.saxon.s9api.XdmNode
import net.sf.saxon.s9api.XdmValue
import java.util._
import java.util.function.BiConsumer
import java.util.function.BinaryOperator
import java.util.function.Function
import java.util.function.Supplier
import java.util.stream.Collector
import java.util.stream.Collector.Characteristics

object XdmCollectors {

  class MultipleItemException extends RuntimeException

   abstract class XdmCollector[R, I <: XdmItem]
    extends Collector[XdmItem, List[I], R] {

     def onEmpty(): Unit = ()

     def onMultiple(): Unit = ()

     def convert(item: XdmItem): I = item.asInstanceOf[I]

     def makeResult(list: List[I]): R

    def supplier: Supplier[util.List[I]] = () => new util.ArrayList[I]()


    override def accumulator(): BiConsumer[List[I], XdmItem] =
      (list, next) => {
        val item = convert(next)
        if (!list.isEmpty) {
          onMultiple()
        }
        list.add(item)
      }

    override def combiner(): BinaryOperator[List[I]] = (list1, list2) => {
      list1.addAll(list2)
      if (list1.size > 1) {
        onMultiple()
      }
      list1
    }

    override def finisher(): Function[List[I], R] = (list) => {
      if (list.isEmpty) {
        onEmpty()
      }
      makeResult(list)
    }

    override def characteristics(): Set[Characteristics] =
      Collections.emptySet()

  }

  def asXdmValue(): XdmCollector[XdmValue, XdmItem] =
    new XdmCollector[XdmValue, XdmItem]() {
       override def makeResult(list: List[XdmItem]): XdmValue =
        new XdmValue(list)
    }

  def asNode(): XdmCollector[XdmNode, XdmNode] =
    new XdmCollector[XdmNode, XdmNode]() {
       override def onEmpty(): Unit = {
        throw new NoSuchElementException()
      }

       override def onMultiple(): Unit = {
        throw new MultipleItemException()
      }

       override def convert(item: XdmItem): XdmNode =
        item.asInstanceOf[XdmNode]

       override def makeResult(list: List[XdmNode]): XdmNode =
        list.get(0)
    }

  def asOptionalNode(): XdmCollector[Optional[XdmNode], XdmNode] =
    new XdmCollector[Optional[XdmNode], XdmNode]() {
       override def onEmpty(): Unit = ()

       override def onMultiple(): Unit = {
        throw new MultipleItemException()
      }

       override def convert(item: XdmItem): XdmNode =
        item.asInstanceOf[XdmNode]

       override def makeResult(
                                         list: List[XdmNode]): Optional[XdmNode] =
        if (list.isEmpty) Optional.empty() else Optional.of(list.get(0))
    }

  def asListOfNodes(): XdmCollector[List[XdmNode], XdmNode] =
    new XdmCollector[List[XdmNode], XdmNode]() {
       override def onEmpty(): Unit = ()

       override def onMultiple(): Unit = ()

       override def convert(item: XdmItem): XdmNode =
        item.asInstanceOf[XdmNode]

       override def makeResult(list: List[XdmNode]): List[XdmNode] =
        list
    }

  def asListOfAtomic(): XdmCollector[List[XdmAtomicValue], XdmAtomicValue] =
    new XdmCollector[List[XdmAtomicValue], XdmAtomicValue]() {
       override def onEmpty(): Unit = ()

       override def onMultiple(): Unit = ()

       override def convert(item: XdmItem): XdmAtomicValue =
        item.asInstanceOf[XdmAtomicValue]

       override def makeResult(
                                         list: List[XdmAtomicValue]): List[XdmAtomicValue] = list
    }

  def asOptionalAtomic()
  : XdmCollector[Optional[XdmAtomicValue], XdmAtomicValue] =
    new XdmCollector[Optional[XdmAtomicValue], XdmAtomicValue]() {
       override def onEmpty(): Unit = ()

       override def onMultiple(): Unit = {
        throw new MultipleItemException()
      }

       override def convert(item: XdmItem): XdmAtomicValue =
        item.asInstanceOf[XdmAtomicValue]

       override def makeResult(
                                         list: List[XdmAtomicValue]): Optional[XdmAtomicValue] =
        if (list.isEmpty) Optional.empty() else Optional.of(list.get(0))
    }

  def asAtomic(): XdmCollector[XdmAtomicValue, XdmAtomicValue] =
    new XdmCollector[XdmAtomicValue, XdmAtomicValue]() {
       override def onEmpty(): Unit = {
        throw new NoSuchElementException()
      }

       override def onMultiple(): Unit = {
        throw new MultipleItemException()
      }

       override def convert(item: XdmItem): XdmAtomicValue =
        item.asInstanceOf[XdmAtomicValue]

       override def makeResult(
                                         list: List[XdmAtomicValue]): XdmAtomicValue = list.get(0)
    }

  def asOptionalString(): XdmCollector[Optional[String], XdmItem] =
    new XdmCollector[Optional[String], XdmItem]() {
       override def onMultiple(): Unit = {
        throw new MultipleItemException()
      }

       override def makeResult(
                                         list: List[XdmItem]): Optional[String] =
        if (list.isEmpty) Optional.empty()
        else Optional.of(list.get(0).getStringValue)
    }

  def asString(): XdmCollector[String, XdmItem] =
    new XdmCollector[String, XdmItem]() {
       override def onEmpty(): Unit = {
        throw new NoSuchElementException()
      }

       override def onMultiple(): Unit = {
        throw new MultipleItemException()
      }

       override def makeResult(list: List[XdmItem]): String =
        list.get(0).getStringValue
    }

}
