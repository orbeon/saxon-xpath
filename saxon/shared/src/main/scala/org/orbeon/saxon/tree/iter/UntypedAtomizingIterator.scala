package org.orbeon.saxon.tree.iter

import java.util

import org.orbeon.saxon.expr.LastPositionFinder
import org.orbeon.saxon.om.Item
import org.orbeon.saxon.om.SequenceIterator
import org.orbeon.saxon.trans.XPathException
import org.orbeon.saxon.value.AtomicValue
import java.util.EnumSet

//import scala.collection.compat._
import scala.jdk.CollectionConverters._
import org.orbeon.saxon.om.SequenceIterator.Property._
import java.util.Set

import scala.collection.immutable

class UntypedAtomizingIterator(private val base: SequenceIterator)
  extends SequenceIterator
    with LastPositionFinder
    with LookaheadIterator {

  def next(): AtomicValue = {
    val nextSource: Item = base.next()
    if (nextSource == null) {
      null
    } else {
      nextSource.atomize().asInstanceOf[AtomicValue]
    }
  }

  override def close(): Unit = {
    base.close()
  }

  override def getProperties: immutable.Set[Property] =
    base.getProperties.intersect(immutable.Set(LAST_POSITION_FINDER, LOOKAHEAD))

  def getLength: Int = base.asInstanceOf[LastPositionFinder].getLength

  def hasNext: Boolean = base.asInstanceOf[LookaheadIterator].hasNext

}