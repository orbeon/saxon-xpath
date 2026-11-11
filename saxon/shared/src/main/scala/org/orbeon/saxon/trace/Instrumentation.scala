package org.orbeon.saxon.trace

import java.util.HashMap

//import scala.collection.compat._
import scala.jdk.CollectionConverters._

object Instrumentation {

  val ACTIVE: Boolean = false

  var counters: HashMap[String, java.lang.Long] = new HashMap()

  def count(counter: String): Unit = {
    if (counters.containsKey(counter)) {
      counters.put(counter, counters.get(counter) + 1L)
    } else {
      counters.put(counter, 1L)
    }
  }

  def count(counter: String, increment: Long): Unit = {
    if (counters.containsKey(counter)) {
      counters.put(counter, counters.get(counter) + increment)
    } else {
      counters.put(counter, increment)
    }
  }

  def report(): Unit = {
    if (ACTIVE && !counters.isEmpty) {
      System.err.println("COUNTERS")
      for ((key, value) <- counters.asScala) {
        System.err.println(key + " = " + value)
      }
    }
  }

  def reset(): Unit = {
    counters.clear()
  }

}
