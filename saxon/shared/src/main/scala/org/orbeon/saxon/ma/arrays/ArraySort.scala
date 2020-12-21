package org.orbeon.saxon.ma.arrays

import java.util

import org.orbeon.saxon.expr.sort.{AtomicComparer, AtomicSortComparer}
import org.orbeon.saxon.expr.{Atomizer, XPathContext}
import org.orbeon.saxon.functions.SystemFunction
import org.orbeon.saxon.lib.StringCollator
import org.orbeon.saxon.om._

//import scala.collection.compat._
import java.util.{ArrayList, List}
import java.{util => ju}

import org.orbeon.saxon.ma.arrays.ArraySort._
import org.orbeon.saxon.om.GroundedValue
import org.orbeon.saxon.trans.XPathException
import org.orbeon.saxon.value.{AtomicValue, StringValue}

import scala.jdk.CollectionConverters._

object ArraySort {

  // ORBEON
  def sortList[E <: AnyRef](list: ju.List[E])(c: ju.Comparator[_ >: E]): Unit = {
    val a = list.toArray
    util.Arrays.sort(a, c.asInstanceOf[ju.Comparator[AnyRef]])
    val i = list.listIterator
    for (e <- a) {
      i.next()
      i.set(e.asInstanceOf[E])
    }
  }

  private class MemberToBeSorted {
    var value: GroundedValue = _
    var sortKey: GroundedValue = _
    var originalPosition: Int = _
  }

  def compareSortKeys(a: GroundedValue,
                      b: GroundedValue,
                      comparer: AtomicComparer): Int = {
    val iteratora = a.iterate()
    val iteratorb = b.iterate()
    while (true) {
      val firsta = iteratora.next().asInstanceOf[AtomicValue]
      val firstb = iteratorb.next().asInstanceOf[AtomicValue]
      if (firsta == null) {
        if (firstb == null) {
          return 0
        } else {
          return -1
        }
      } else if (firstb == null) {
        return +1
      } else {
        val first = comparer.compareAtomicValues(firsta, firstb)
        if (first == 0) {
        } else {
          return first
        }
      }
    }
    0
  }

  private def atomize(input: Sequence) = {
    val iterator = input.iterate()
    val mapper =
      Atomizer.getAtomizingIterator(iterator, oneToOne = false)
    mapper.materialize
  }

}

class ArraySort extends SystemFunction {

  override def call(context: XPathContext, arguments: Array[Sequence]): ArrayItem = {
    val array: ArrayItem = arguments(0).head.asInstanceOf[ArrayItem]
    val inputList: List[MemberToBeSorted] =
      new ArrayList[MemberToBeSorted](array.arrayLength())
    var i: Int = 0
    var collation: StringCollator = null
    if (arguments.length == 1) {
      collation = context.getConfiguration.getCollation(
        getRetainedStaticContext.getDefaultCollationName)
    } else {
      val collName: StringValue = arguments(1).head.asInstanceOf[StringValue]
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
      key = arguments(2).head.asInstanceOf[Function]
    }
    for (seq <- array.members) {
      val member: MemberToBeSorted = new MemberToBeSorted()
      member.value = seq
      i += 1
      member.originalPosition = i
      member.sortKey =
        if (key != null) SystemFunction.dynamicCall(key, context, Array(seq)).materialize
        else ArraySort.atomize(seq)
      inputList.add(member)
    }
    val atomicComparer: AtomicComparer = AtomicSortComparer.makeSortComparer(
      collation,
      StandardNames.XS_ANY_ATOMIC_TYPE,
      context)
    try
      sortList(inputList)((a, b) => {
        val result = compareSortKeys(a.sortKey, b.sortKey, atomicComparer)
        if (result == 0) {
          a.originalPosition - b.originalPosition
        } else {
          result
        }
      })
    catch {
      case e: ClassCastException =>
        val err = new XPathException("Non-comparable types found while sorting: " + e.getMessage)
        err.setErrorCode("XPTY0004")
        throw err
    }
    val outputList = new ArrayList[GroundedValue](array.arrayLength())
    for (member <- inputList.asScala)
      outputList.add(member.value)
    new SimpleArrayItem(outputList)
  }

}
