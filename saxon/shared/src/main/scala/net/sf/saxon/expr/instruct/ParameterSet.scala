package net.sf.saxon.expr.instruct

import net.sf.saxon.om.Sequence

import net.sf.saxon.om.StructuredQName

import net.sf.saxon.trans.XPathException

import net.sf.saxon.value.Closure

import java.util.Arrays

import java.util.Map

import scala.jdk.CollectionConverters._

object ParameterSet {

  var EMPTY_PARAMETER_SET: ParameterSet = new ParameterSet(0)

  val NOT_SUPPLIED: Int = 0

  val SUPPLIED: Int = 1

  val SUPPLIED_AND_CHECKED: Int = 2

}

class ParameterSet(capacity: Int) {

  private var keys: Array[StructuredQName] =
    new Array[StructuredQName](capacity)

  private var values: Array[Sequence] =
    new Array[Sequence](capacity)

  private var typeChecked: Array[Boolean] = new Array[Boolean](capacity)

  private var used: Int = 0

  def this() = this(10)

  def this(map: Map[StructuredQName, Sequence]) = {
    this(map.size)
    var i: Int = 0
    for ((key, value) <- map.asScala) {
      keys(i) = key
      values(i) = value
      i += 1
      typeChecked(i) = false
    }
    used = i
  }

  def this(existing: ParameterSet, extra: Int) = {
    this(existing.used + extra)
    for (i <- 0 until existing.used) {
      put(existing.keys(i), existing.values(i), existing.typeChecked(i))
    }
  }

  def size(): Int = used

  def put(id: StructuredQName, value: Sequence, checked: Boolean): Unit = {
    for (i <- 0 until used if keys(i) == id) {
      values(i) = value
      typeChecked(i) = checked
      return
    }
    if (used + 1 > keys.length) {
      val newLength: Int = if (used <= 5) 10 else used * 2
      values = Arrays.copyOf(values, newLength)
      keys = Arrays.copyOf(keys, newLength)
      typeChecked = Arrays.copyOf(typeChecked, newLength)
    }
    keys(used) = id
    typeChecked(used) = checked
    values({
      used += 1; used - 1
    }) = value
  }

  def getParameterNames: Array[StructuredQName] = keys

  def getIndex(id: StructuredQName): Int =
    (0 until used).find(keys(_) == id).getOrElse(-1)

  def getValue(index: Int): Sequence = values(index)

  def isTypeChecked(index: Int): Boolean = typeChecked(index)

  def clear(): Unit = {
    used = 0
  }

  def materializeValues(): Unit = {
    for (i <- 0 until used if values(i).isInstanceOf[Closure]) {
      values(i) = values(i).asInstanceOf[Closure].reduce()
    }
  }

}
