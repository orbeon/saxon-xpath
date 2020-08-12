package net.sf.saxon.functions

import net.sf.saxon.expr.XPathContext

import net.sf.saxon.expr.sort.AtomicMatchKey

import net.sf.saxon.lib.StringCollator

import net.sf.saxon.om.Sequence

import net.sf.saxon.om.SequenceIterator

import net.sf.saxon.om.ZeroOrMore

import net.sf.saxon.trans.XPathException

import net.sf.saxon.value.AtomicValue

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
          else nextBase.getXPathComparable(false, collator, implicitTimezone)
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

  override def getStreamerName(): String = "DistinctValues"

  def call(context: XPathContext,
           arguments: Array[Sequence]): ZeroOrMore[AtomicValue] = {
    val collator: StringCollator = getStringCollator
    new ZeroOrMore(
      new DistinctIterator(arguments(0).iterate(), collator, context))
  }

}
