package net.sf.saxon.ma.arrays

import net.sf.saxon.expr.Atomizer

import net.sf.saxon.expr.XPathContext

import net.sf.saxon.expr.sort.AtomicComparer

import net.sf.saxon.expr.sort.AtomicSortComparer

import net.sf.saxon.functions.SystemFunction

import net.sf.saxon.lib.StringCollator

import net.sf.saxon.om._

import scala.jdk.CollectionConverters._

import net.sf.saxon.trans.XPathException

import net.sf.saxon.tree.iter.UnfailingIterator

import net.sf.saxon.value.AtomicValue

import net.sf.saxon.value.StringValue

import java.util.ArrayList

import java.util.List

import ArraySort._

import net.sf.saxon.om.GroundedValue

object ArraySort {

  private class MemberToBeSorted {

    var value: GroundedValue = _

    var sortKey: GroundedValue = _

    var originalPosition: Int = _

  }

  def compareSortKeys(a: GroundedValue,
                      b: GroundedValue,
                      comparer: AtomicComparer): Int = {
    val iteratora: UnfailingIterator = a.iterate()
    val iteratorb: UnfailingIterator = b.iterate()
    while (true) {
      val firsta: AtomicValue = iteratora.next().asInstanceOf[AtomicValue]
      val firstb: AtomicValue = iteratorb.next().asInstanceOf[AtomicValue]
      if (firsta == null) {
        if (firstb == null) {
          0
        } else {
          -1
        }
      } else if (firstb == null) {
        +1
      } else {
        val first: Int = comparer.compareAtomicValues(firsta, firstb)
        if (first == 0) {
          //continue
        } else {
          first
        }
      }
    }
    0
  }

  private def atomize(input: Sequence): GroundedValue = {
    val iterator: SequenceIterator = input.iterate()
    val mapper: SequenceIterator =
      Atomizer.getAtomizingIterator(iterator, false)
    mapper.materialize()
  }

}

class ArraySort extends SystemFunction {

  override def call(context: XPathContext,
                    arguments: Array[Sequence]): ArrayItem = {
    val array: ArrayItem = arguments(0).head().asInstanceOf[ArrayItem]
    val inputList: List[MemberToBeSorted] =
      new ArrayList[MemberToBeSorted](array.arrayLength())
    var i: Int = 0
    var collation: StringCollator = null
    if (arguments.length == 1) {
      collation = context.getConfiguration.getCollation(
        getRetainedStaticContext.getDefaultCollationName)
    } else {
      val collName: StringValue = arguments(1).head().asInstanceOf[StringValue]
      collation =
        if (collName == null)
          context.getConfiguration.getCollation(
            getRetainedStaticContext.getDefaultCollationName)
        else
          context.getConfiguration
            .getCollation(collName.getStringValue, getStaticBaseUriString)
    }
    var key: Function = null
    if (arguments.length == 3) {
      key = arguments(2).head().asInstanceOf[Function]
    }
    for (seq <- array.members()) {
      val member: MemberToBeSorted = new MemberToBeSorted()
      member.value = seq
      i += 1
      member.originalPosition = i
      member.sortKey =
        if (key != null) SystemFunction.dynamicCall(key, context, Array(seq)).materialize()
        else ArraySort.atomize(seq)
      inputList.add(member)
    }
    val atomicComparer: AtomicComparer = AtomicSortComparer.makeSortComparer(
      collation,
      StandardNames.XS_ANY_ATOMIC_TYPE,
      context)
    try inputList.sort((a, b) => {
      val result: Int = compareSortKeys(a.sortKey, b.sortKey, atomicComparer)
      if (result == 0) {
        a.originalPosition - b.originalPosition
      } else {
        result
      }
    })
    catch {
      case e: ClassCastException => {
        val err: XPathException = new XPathException(
          "Non-comparable types found while sorting: " + e.getMessage)
        err.setErrorCode("XPTY0004")
        throw err
      }

    }
    val outputList: List[GroundedValue] =
      new ArrayList[GroundedValue](array.arrayLength())
    for (member <- inputList.asScala) {
      outputList.add(member.value)
    }
    new SimpleArrayItem(outputList)
  }

}
