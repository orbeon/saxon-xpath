package net.sf.saxon.tree.linked

import java.util.Arrays


class LineNumberMap {

  private var sequenceNumbers: Array[Int] = new Array[Int](200)

  private var lineNumbers: Array[Int] = new Array[Int](200)

  private var columnNumbers: Array[Int] = new Array[Int](200)

  private var allocated: Int = 0

  def setLineAndColumn(sequence: Int, line: Int, column: Int): Unit = {
    if (sequenceNumbers.length <= allocated + 1) {
      sequenceNumbers = Arrays.copyOf(sequenceNumbers, allocated * 2)
      lineNumbers = Arrays.copyOf(lineNumbers, allocated * 2)
      columnNumbers = Arrays.copyOf(columnNumbers, allocated * 2)
    }
    sequenceNumbers(allocated) = sequence
    lineNumbers(allocated) = line
    columnNumbers(allocated) = column
    allocated = allocated + 1;
  }

  def getLineNumber(sequence: Int): Int = {
    if (sequenceNumbers.length > allocated) {
      condense()
    }
    var index: Int = Arrays.binarySearch(sequenceNumbers, sequence)
    if (index < 0) {
      index = -index - 1
      if (index > lineNumbers.length - 1) {
        index = lineNumbers.length - 1
      }
    }
    lineNumbers(index)
  }

  def getColumnNumber(sequence: Int): Int = {
    if (sequenceNumbers.length > allocated) {
      condense()
    }
    var index: Int = Arrays.binarySearch(sequenceNumbers, sequence)
    if (index < 0) {
      index = -index - 1
      if (index >= columnNumbers.length) {
        index = columnNumbers.length - 1
      }
    }
    columnNumbers(index)
  }

  private def condense(): Unit = {
    synchronized {
      sequenceNumbers = Arrays.copyOf(sequenceNumbers, allocated)
      lineNumbers = Arrays.copyOf(lineNumbers, allocated)
      columnNumbers = Arrays.copyOf(columnNumbers, allocated)
    }
  }

}