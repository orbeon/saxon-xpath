package net.sf.saxon.tree.iter

import java.util

import net.sf.saxon.expr.LastPositionFinder
import net.sf.saxon.om.Item
import net.sf.saxon.om.SequenceIterator
import net.sf.saxon.trans.XPathException
import net.sf.saxon.value.AtomicValue
import java.util.EnumSet

import scala.jdk.CollectionConverters._
import net.sf.saxon.om.SequenceIterator.Property._
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

  override def getProperties(): immutable.Set[Property] = {
    val p: Set[Property] = base.getProperties.asJava
    val props = new util.HashSet[Property]()
    props.add(LAST_POSITION_FINDER)
    props.add(LOOKAHEAD)
    p.retainAll(props)
    p.asScala.toSet
  }

  def getLength(): Int = base.asInstanceOf[LastPositionFinder].getLength

  def hasNext(): Boolean = base.asInstanceOf[LookaheadIterator].hasNext

}