package org.orbeon.saxon.tree.iter

import java.util.{ArrayList, List}
import java.util.function.Consumer

import org.orbeon.saxon.om.{Item, SequenceIterator}


trait UnfailingIterator extends SequenceIterator {

  def next(): Item

  def forEach(consumer: Consumer[_ >: Item]): Unit = {
    var item: Item = null
    while ({
      item = next()
      item
    } != null) consumer.accept(item)
  }

  def toList: List[Item] = {
    val list = new ArrayList[Item]()
    forEach((res: Item) => list.add(res))
    list
  }
}