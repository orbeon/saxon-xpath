package net.sf.saxon.tree.linked

import java.util.Arrays


class SystemIdMap {

  private var sequenceNumbers: Array[Int] = new Array[Int](4)

  private var uris: Array[String] = new Array[String](4)

  private var allocated: Int = 0

  def setSystemId(sequence: Int, uri: String): Unit = {
    if (allocated > 0) {
      if (uri == uris(allocated - 1)) {
        return
      }
      if (sequence <= sequenceNumbers(allocated - 1)) {
        throw new IllegalArgumentException("System IDs of nodes are immutable")
      }
    }
    if (sequenceNumbers.length <= allocated + 1) {
      sequenceNumbers = Arrays.copyOf(sequenceNumbers, allocated * 2)
      uris = Arrays.copyOf(uris, allocated * 2)
    }
    sequenceNumbers(allocated) = sequence
    uris(allocated) = uri
    allocated = allocated + 1
  }

  def getSystemId(sequence: Int): String = {
    if (allocated == 0) {
      return null
    }
    for (i <- 1 until allocated if sequenceNumbers(i) > sequence) {
      uris(i - 1)
    }
    uris(allocated - 1)
  }

}