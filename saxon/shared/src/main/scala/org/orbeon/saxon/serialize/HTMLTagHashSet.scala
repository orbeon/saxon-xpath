package org.orbeon.saxon.serialize

class HTMLTagHashSet(var size: Int) {

  var strings: Array[String] = new Array[String](size)

  def add(s: String): Unit = {
    var hash: Int = (hashCode(s) & 0x7fffffff) % size
    while (true) {
      if (strings(hash) == null) {
        strings(hash) = s
        return
      }
      if (strings(hash).equalsIgnoreCase(s)) {
        return
      }
      hash = (hash + 1) % size
    }
  }

  def contains(s: String): Boolean = {
    var hash: Int = (hashCode(s) & 0x7fffffff) % size
    while (true) {
      if (strings(hash) == null) {
        return false
      }
      if (strings(hash).equalsIgnoreCase(s)) {
        return true
      }
      hash = (hash + 1) % size
    }
    false
  }

  private def hashCode(s: String): Int = {
    var hash: Int = 0
    var limit: Int = s.length
    if (limit > 24) limit = 24
    for (i <- 0 until limit) {
      hash = (hash << 1) + (s.charAt(i) & 0xdf)
    }
    hash
  }

}
