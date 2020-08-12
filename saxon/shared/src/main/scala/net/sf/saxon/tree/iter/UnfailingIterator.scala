package net.sf.saxon.tree.iter

import net.sf.saxon.om.Item

import net.sf.saxon.om.SequenceIterator

import java.util.ArrayList

import java.util.List

import java.util.function.Consumer

trait UnfailingIterator extends SequenceIterator {

  def next(): Item

  def forEach(consumer: Consumer[_ >: Item]): Unit = {
    var item: Item = null
    while ((item = next()) != null) consumer.accept(item)
  }

  def toList(): List[Item] = {
    val list: List[Item] = new ArrayList[Item]()
    forEach((res: Item) => list.add(res))
    list
  }

}