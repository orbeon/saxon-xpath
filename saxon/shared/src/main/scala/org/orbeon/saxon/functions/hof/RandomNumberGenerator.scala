////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
package org.orbeon.saxon.functions.hof

import org.orbeon.saxon.expr.Callable

import org.orbeon.saxon.expr.StaticProperty

import org.orbeon.saxon.expr.XPathContext

import org.orbeon.saxon.functions.CallableFunction

import org.orbeon.saxon.functions.SystemFunction

import org.orbeon.saxon.ma.map.DictionaryMap

import org.orbeon.saxon.ma.map.MapItem

import org.orbeon.saxon.ma.map.MapType

import org.orbeon.saxon.model.BuiltInAtomicType

import org.orbeon.saxon.model.FunctionItemType

import org.orbeon.saxon.model.SpecificFunctionType

import org.orbeon.saxon.om.Item

import org.orbeon.saxon.om.Sequence

import org.orbeon.saxon.om.SequenceIterator

import org.orbeon.saxon.trans.XPathException

import org.orbeon.saxon.value.AtomicValue

import org.orbeon.saxon.value.DoubleValue

import org.orbeon.saxon.value.SequenceExtent

import org.orbeon.saxon.value.SequenceType

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
      val input: Sequence = arguments(0)
      val iterator: SequenceIterator = input.iterate()
      var item: Item = null
      val output: List[Item] = new LinkedList[Item]()
      val random: Random = new Random(nextSeed)
      while (({
        item = iterator.next()
        item
      }) != null) {
        val p: Int = random.nextInt(output.size + 1)
        output.add(p, item)
      }
      new SequenceExtent(output)
    }

    override def toString: String = "random-number-generator.permute"

  }

  private class NextGenerator(var nextSeed: Long) extends Callable {

    def call(context: XPathContext, arguments: Array[Sequence]): Sequence =
      generator(nextSeed, context)

    override def toString: String = "random-number-generator.next"

  }

}

/**
  * This class implements the function random-number-generator(), which is a standard function in XPath 3.1
  */
class RandomNumberGenerator extends SystemFunction with Callable {

  def call(context: XPathContext, arguments: Array[Sequence]): Sequence = {
    var seed: Long = 0L
    if (arguments.length == 0) {
// seed value must be repeatable within execution scope
      seed = context.getCurrentDateTime.getCalendar.getTimeInMillis
    } else {
      val `val`: AtomicValue = arguments(0).head.asInstanceOf[AtomicValue]
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