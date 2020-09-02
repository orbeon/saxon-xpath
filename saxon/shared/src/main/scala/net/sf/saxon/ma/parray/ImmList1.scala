package net.sf.saxon.ma.parray

import net.sf.saxon.tree.jiter.MonoIterator

class ImmList1[E](private var member: E) extends ImmList[E] {

  override def get(index: Int): E =
    if (index == 0) {
      member
    } else {
      throw outOfBounds(index, 1)
    }

  override def size(): Int = 1

  override def isEmpty: Boolean = false

  override def replace(index: Int, member: E): ImmList[E] =
    if (index == 0) {
      new ImmList1(member)
    } else {
      throw outOfBounds(index, 1)
    }

  override def insert(index: Int, member: E): ImmList[E] =
    if (index == 0) {
      new ImmList2(new ImmList1(member), this)
    } else if (index == 1) {
      new ImmList2(this, new ImmList1(member))
    } else {
      throw outOfBounds(index, 1)
    }

  override def append(member: E): ImmList[E] =
    new ImmList2(this, new ImmList1(member))

  override def appendList(members: ImmList[E]): ImmList[E] =
    new ImmList2(this, members)

  override def remove(index: Int): ImmList[E] =
    if (index == 0) {
      ImmList.empty()
    } else {
      throw outOfBounds(index, 1)
    }

  override def subList(start: Int, end: Int): ImmList[E] = {
    if (start != 0) {
      throw outOfBounds(start, 1)
    }
    if (end == 0) {
      ImmList.empty()
    } else if (end == 1) {
      this
    } else {
      throw outOfBounds(end, 1)
    }
  }

  override def iterator: collection.Iterator[E] = new MonoIterator(member).asInstanceOf[collection.Iterator[E]]

}
