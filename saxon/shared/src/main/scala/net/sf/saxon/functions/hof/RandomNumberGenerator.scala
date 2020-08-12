////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
package net.sf.saxon.functions.hof

import net.sf.saxon.expr.Callable

import net.sf.saxon.expr.StaticProperty

import net.sf.saxon.expr.XPathContext

import net.sf.saxon.functions.CallableFunction

import net.sf.saxon.functions.SystemFunction

import net.sf.saxon.ma.map.DictionaryMap

import net.sf.saxon.ma.map.MapItem

import net.sf.saxon.ma.map.MapType

import net.sf.saxon.model.BuiltInAtomicType

import net.sf.saxon.model.FunctionItemType

import net.sf.saxon.model.SpecificFunctionType

import net.sf.saxon.om.Item

import net.sf.saxon.om.Sequence

import net.sf.saxon.om.SequenceIterator

import net.sf.saxon.trans.XPathException

import net.sf.saxon.value.AtomicValue

import net.sf.saxon.value.DoubleValue

import net.sf.saxon.value.SequenceExtent

import net.sf.saxon.value.SequenceType

import java.util.LinkedList

import java.util.List

import java.util.Random

import RandomNumberGenerator._




object RandomNumberGenerator {

  val RETURN_TYPE: MapType =
    new MapType(BuiltInAtomicType.STRING, SequenceType.SINGLE_ITEM)

  private val NEXT_FN_TYPE: FunctionItemType =
    new SpecificFunctionType( // zero arguments
      Array(),
      SequenceType.makeSequenceType(RETURN_TYPE, StaticProperty.ALLOWS_ONE))

  private val PERMUTE_FN_TYPE: FunctionItemType = new SpecificFunctionType(
    Array(SequenceType.ANY_SEQUENCE),
    SequenceType.ANY_SEQUENCE)

  private def generator(seed: Long, context: XPathContext): MapItem = {
    val random: Random = new Random(seed)
    val number: Double = random.nextDouble()
    val nextSeed: Long = random.nextLong()
    val map: DictionaryMap = new DictionaryMap()
    map.initialPut("number", new DoubleValue(number))
    map.initialPut(
      "next",
      new CallableFunction(0, new NextGenerator(nextSeed), NEXT_FN_TYPE))
    map.initialPut(
      "permute",
      new CallableFunction(1, new Permutation(nextSeed), PERMUTE_FN_TYPE))
    map
  }

  private class Permutation(var nextSeed: java.lang.Long) extends Callable {

    def call(context: XPathContext, arguments: Array[Sequence]): Sequence = {
      val input: Sequence = arguments(0).asInstanceOf[Sequence]
      val iterator: SequenceIterator = input.iterate()
      var item: Item = null
      val output: List[Item] = new LinkedList[Item]()
      val random: Random = new Random(nextSeed)
      while ((item = iterator.next()) != null) {
        val p: Int = random.nextInt(output.size + 1)
        output.add(p, item)
      }
      new SequenceExtent(output)
    }

    override def toString(): String = "random-number-generator.permute"

  }

  private class NextGenerator(var nextSeed: Long) extends Callable {

    def call(context: XPathContext, arguments: Array[Sequence]): Sequence =
      generator(nextSeed, context)

    override def toString(): String = "random-number-generator.next"

  }

}

/**
  * This class implements the function random-number-generator(), which is a standard function in XPath 3.1
  */
class RandomNumberGenerator extends SystemFunction with Callable {

  def call(context: XPathContext, arguments: Array[Sequence]): Sequence = {
    var seed: Long = 0l
    if (arguments.length == 0) {
// seed value must be repeatable within execution scope
      seed = context.getCurrentDateTime.getCalendar.getTimeInMillis
    } else {
      val `val`: AtomicValue = arguments(0).head().asInstanceOf[AtomicValue]
      seed =
        if (`val` == null)
          context.getCurrentDateTime.getCalendar.getTimeInMillis
        else `val`.hashCode
    }
    generator(seed, context)
  }

}

// Copyright (c) 2020 Saxonica Limited
// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is "Incompatible With Secondary Licenses", as defined by the Mozilla Public License, v. 2.0.
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// Copyright (c) 2018-2020 Saxonica Limited
