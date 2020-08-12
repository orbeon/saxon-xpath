////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
package net.sf.saxon.ma.parray

import net.sf.saxon.tree.jiter.ConcatenatingIterator
import java.util.Iterator

import ImmList2._

import scala.collection


object ImmList2 {

  private val THRESHOLD: Int = 10

}

/**
 * Implementation of an immutable list of arbitrary length, implemented as a binary tree
 */
class ImmList2[E](private var left: ImmList[E],
                  private var right: ImmList[E])
  extends ImmList[E] {

  var imSize: Int = left.size + right.size

  override def size(): Int = imSize

  override def get(index: Int): E =
    if (index < 0) {
      throw outOfBounds(index, imSize)
    } else if (index < left.size) {
      left.get(index)
    } else if (index < imSize) {
      right.get(index - left.size)
    } else {
      throw outOfBounds(index, imSize)
    }

  override def isEmpty(): Boolean = false

  override def replace(index: Int, member: E): ImmList[E] =
    if (index < 0) {
      throw outOfBounds(index, imSize)
    } else if (index < left.size) {
      new ImmList2(left.replace(index, member), right)
    } else if (index < imSize) {
      new ImmList2(left, right.replace(index - left.size, member))
    } else {
      throw outOfBounds(index, imSize)
    }

  override def insert(index: Int, member: E): ImmList[E] =
    if (index < 0) {
      throw outOfBounds(index, imSize)
    } else if (index <= left.size) {
      new ImmList2(left.insert(index, member), right).rebalance()
    } else if (index <= imSize) {
      new ImmList2(left, right.insert(index - left.size, member)).rebalance()
    } else {
      throw outOfBounds(index, imSize)
    }

  override def append(member: E): ImmList[E] =
    new ImmList2(this, new ImmList1(member)).rebalance()

  override def appendList(members: ImmList[E]): ImmList[E] =
    new ImmList2(this, members).rebalance()

  override def remove(index: Int): ImmList[E] =
    if (index < 0) {
      throw outOfBounds(index, imSize)
    } else if (index < left.size) {
      new ImmList2(left.remove(index), right).rebalance()
    } else if (index < imSize) {
      new ImmList2(left, right.remove(index - left.size)).rebalance()
    } else {
      throw outOfBounds(index, imSize)
    }

  override def subList(start: Int, end: Int): ImmList[E] = {
    if (start < 0 || start >= imSize) {
      throw outOfBounds(start, imSize)
    } else if (end < start || end > imSize) {
      throw outOfBounds(end, imSize)
    }
    if (start < left.size && end <= left.size) {
      left.subList(start, end)
    } else if (start >= left.size && end >= left.size) {
      right.subList(start - left.size, end - left.size)
    } else {
      new ImmList2(left.subList(start, left.size),
        right.subList(0, end - left.size)).rebalance()
    }
  }

  override def iterator(): collection.Iterator[E] =
    new ConcatenatingIterator(left.iterator, () => right.iterator)

  override def rebalance(): ImmList[E] = {
    if (left.isEmpty) {
      return right
    }
    if (right.isEmpty) {
      return left
    }
    //.rebalance();
    val l2: ImmList[E] = left
    //.rebalance();
    val r2: ImmList[E] = right
    //System.err.println("Balance l=" + left.size() + " r=" + right.size());
    if (imSize > THRESHOLD) {
      if (l2.isInstanceOf[ImmList2[E]] && l2.size > THRESHOLD * r2.size) {
        new ImmList2(l2.asInstanceOf[ImmList2[E]].left,
          new ImmList2(l2.asInstanceOf[ImmList2[E]].right, r2))
      } else if (r2.isInstanceOf[ImmList2[E]] && r2.size > THRESHOLD * l2.size) {
        new ImmList2(new ImmList2(l2, r2.asInstanceOf[ImmList2[E]].left),
          r2.asInstanceOf[ImmList2[E]].right)
      } else {
        this
      }
    } else if (left == l2 && right == r2) {
      this
    } else {
      new ImmList2(l2, r2)
    }
  }

}