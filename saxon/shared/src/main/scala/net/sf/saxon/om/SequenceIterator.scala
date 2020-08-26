////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// Copyright (c) 2018-2020 Saxonica Limited
// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is "Incompatible With Secondary Licenses", as defined by the Mozilla Public License, v. 2.0.
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
/**
 * A SequenceIterator is used to iterate over any XPath 2 sequence (of values or nodes).
 * To get the next item in a sequence, call next(); if this returns null, you've
 * reached the end of the sequence.
 * <p>The objects returned by the SequenceIterator will generally be either nodes
 * (class NodeInfo), singleton values (class AtomicValue), or function items: these are represented
 * collectively by the interface {@link Item}.</p>
 * <p>The interface to SequenceIterator is changed in Saxon 9.6 to drop support for the
 * current() and position() methods. Internal iterators no longer need to maintain the values
 * of the current item or the current position. This information is needed (in general) only
 * for an iterator that acts as the current focus; that is, an iterator stored as the current
 * iterator in an XPathContext. SequenceIterators than maintain the value of position()
 * and last() are represented by the interface {@link FocusIterator}.</p>
 *
 * @author Michael H. Kay
 * @since 8.4. Significant changes in 9.6. Generics added in 9.9, removed again in 10.0
 */
package net.sf.saxon.om

import SequenceIterator.Property.Property
import net.sf.saxon.om.{FocusIterator, GroundedValue, Item, ItemConsumer}
import net.sf.saxon.trans.XPathException
import net.sf.saxon.value.SequenceExtent
import java.io.Closeable
import java.util.EnumSet

import SequenceIterator._
import net.sf.saxon.om.SequenceIterator.Property.Property
import scala.jdk.CollectionConverters._

object SequenceIterator {

  object Property extends Enumeration {

    val GROUNDED: Property = new Property()

    val LAST_POSITION_FINDER: Property = new Property()

    val LOOKAHEAD: Property = new Property()

    val ATOMIZING: Property = new Property()

    class Property extends Val

    implicit def convertValue(v: Value): Property = v.asInstanceOf[Property]

  }

}

trait SequenceIterator extends Closeable {

  /*@Nullable*/

  def next(): Item

  def close(): Unit = ()

  def getProperties(): Set[Property] = Set()

  def forEachOrFail(consumer: ItemConsumer[_ >: Item]): Unit = {
    var item: Item = null
    while (({
      item = next()
      item
    }) != null) consumer.accept(item)
  }

  def materialize(): GroundedValue = new SequenceExtent(this).reduce()

}


