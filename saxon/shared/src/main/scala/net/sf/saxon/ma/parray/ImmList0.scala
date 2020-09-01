package net.sf.saxon.ma.parray

import java.util.Collections
import java.util.Iterator

import scala.collection

object ImmList0 {

  val INSTANCE: ImmList0[_] = new ImmList0()

}

class ImmList0[E] private() extends ImmList[E] {

  override def get(index: Int): E = throw outOfBounds(index, 0)

  override def size(): Int = 0

  override def isEmpty: Boolean = true

  override def replace(index: Int, member: E): ImmList[E] =
    throw outOfBounds(index, 0)

  override def insert(index: Int, member: E): ImmList[E] =
    if (index == 0) {
      new ImmList1(member)
    } else {
      throw outOfBounds(index, 0)
    }

  override def append(member: E): ImmList[E] = new ImmList1(member)

  override def appendList(members: ImmList[E]): ImmList[E] = members

  override def remove(index: Int): ImmList[E] = throw outOfBounds(index, 0)

  override def subList(start: Int, end: Int): ImmList[E] =
    if (start == 0 && end == 0) {
      this
    } else {
      throw outOfBounds(0, 0)
    }

  override def iterator(): collection.Iterator[E] = Collections.emptyIterator().asInstanceOf[collection.Iterator[E]]

}
