package net.sf.saxon.z

class IntSingletonSet(private var value: Int) extends IntSet {

  def getMember(): Int = value

  def clear(): Unit = {
    throw new UnsupportedOperationException("IntSingletonSet is immutable")
  }

  def copy(): IntSet = this

  def mutableCopy(): IntSet = {
    val intHashSet: IntHashSet = new IntHashSet()
    intHashSet.add(value)
    intHashSet
  }

  override def isMutable(): Boolean = false

  def size(): Int = 1

  def isEmpty(): Boolean = false

  def contains(value: Int): Boolean = this.value == value

  def remove(value: Int): Boolean =
    throw new UnsupportedOperationException("IntSingletonSet is immutable")

  def add(value: Int): Boolean =
    throw new UnsupportedOperationException("IntSingletonSet is immutable")

  def iterator(): IntIterator = new IntSingletonIterator(value)

  override def union(other: IntSet): IntSet = {
    val n: IntSet = other.mutableCopy()
    n.add(value)
    n
  }

  override def intersect(other: IntSet): IntSet =
    if (other.contains(value)) {
      this
    } else {
      IntEmptySet.getInstance
    }

  override def except(other: IntSet): IntSet =
    if (other.contains(value)) {
      IntEmptySet.getInstance
    } else {
      this
    }

  override def containsAll(other: IntSet): Boolean = {
    if (other.size > 1) {
      return false
    }
    val ii: IntIterator = other.iterator()
    while (ii.hasNext) if (value != ii.next) {
      false
    }
    true
  }

}
