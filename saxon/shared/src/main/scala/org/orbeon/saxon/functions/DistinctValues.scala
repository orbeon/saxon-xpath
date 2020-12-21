package org.orbeon.saxon.functions

import org.orbeon.saxon.expr.XPathContext

import org.orbeon.saxon.expr.sort.AtomicMatchKey

import org.orbeon.saxon.lib.StringCollator

import org.orbeon.saxon.om.Sequence

import org.orbeon.saxon.om.SequenceIterator

import org.orbeon.saxon.om.ZeroOrMore

import org.orbeon.saxon.trans.XPathException

import org.orbeon.saxon.value.AtomicValue

import java.util.HashSet

import DistinctValues._

object DistinctValues {

  class DistinctIterator(private var base: SequenceIterator,
                         private var collator: StringCollator,
                         private var context: XPathContext)
    extends SequenceIterator {

    private var lookup: HashSet[AtomicMatchKey] = new HashSet(40)

    def next(): AtomicValue = {
      val implicitTimezone: Int = context.getImplicitTimezone
      while (true) {
        val nextBase: AtomicValue = base.next().asInstanceOf[AtomicValue]
        if (nextBase == null) {
          return null
        }
        var key: AtomicMatchKey = null
        key = if (nextBase.isNaN) AtomicMatchKey.NaN_MATCH_KEY
          else nextBase.getXPathComparable(ordered = false, collator, implicitTimezone)
        if (lookup.add(key)) return nextBase
      }
      null
    }

    override def close(): Unit = {
      base.close()
    }

  }

}

class DistinctValues extends CollatingFunctionFixed {

  override def getStreamerName: String = "DistinctValues"

  def call(context: XPathContext,
           arguments: Array[Sequence]): ZeroOrMore[AtomicValue] = {
    val collator: StringCollator = getStringCollator
    new ZeroOrMore(
      new DistinctIterator(arguments(0).iterate(), collator, context))
  }

}
