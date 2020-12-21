package org.orbeon.saxon.z

trait IntToIntMap {

  def setDefaultValue(defaultValue: Int): Unit

  def getDefaultValue: Int

  def clear(): Unit

  def find(key: Int): Boolean

  def get(key: Int): Int

  def size(): Int

  def remove(key: Int): Boolean

  def put(key: Int, value: Int): Unit

  def keyIterator: IntIterator

}

