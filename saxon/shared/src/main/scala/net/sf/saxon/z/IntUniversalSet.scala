package net.sf.saxon.z

object IntUniversalSet {

  private var THE_INSTANCE: IntUniversalSet = new IntUniversalSet()

  def getInstance(): IntUniversalSet = THE_INSTANCE

}

class IntUniversalSet private() extends IntSet {

  def copy(): IntSet = this

  def mutableCopy(): IntSet = new IntComplementSet(new IntHashSet())

  override def isMutable(): Boolean = false

  def clear(): Unit = {
    throw new UnsupportedOperationException("IntUniversalSet is immutable")
  }

  def size(): Int = java.lang.Integer.MAX_VALUE

  def isEmpty(): Boolean = false

  def contains(value: Int): Boolean = true

  def remove(value: Int): Boolean =
    throw new UnsupportedOperationException("IntUniversalSet is immutable")

  def add(value: Int): Boolean =
    throw new UnsupportedOperationException("IntUniversalSet is immutable")

  def iterator(): IntIterator =
    throw new UnsupportedOperationException("Cannot enumerate an infinite set")

  override def union(other: IntSet): IntSet = this

  override def intersect(other: IntSet): IntSet = other.copy()

  override def except(other: IntSet): IntSet =
    if (other.isInstanceOf[IntUniversalSet]) {
      IntEmptySet.getInstance
    } else {
      new IntComplementSet(other.copy())
    }

  override def containsAll(other: IntSet): Boolean = true

}
