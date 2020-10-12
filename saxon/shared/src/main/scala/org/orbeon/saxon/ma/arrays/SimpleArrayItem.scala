package org.orbeon.saxon.ma.arrays

import org.orbeon.saxon.expr.OperandRole

import org.orbeon.saxon.om.GroundedValue

import org.orbeon.saxon.om.Sequence

import org.orbeon.saxon.om.SequenceIterator

import org.orbeon.saxon.query.AnnotationList

import org.orbeon.saxon.trans.Err

//import scala.collection.compat._
import scala.jdk.CollectionConverters._

import org.orbeon.saxon.tree.util.FastStringBuffer

import org.orbeon.saxon.value.ExternalObject

import org.orbeon.saxon.z.IntSet

import java.util.ArrayList

import java.util.List

import scala.beans.{BeanProperty, BooleanBeanProperty}

object SimpleArrayItem {

  val EMPTY_ARRAY: SimpleArrayItem = new SimpleArrayItem(new ArrayList())

  def makeSimpleArrayItem(input: SequenceIterator): SimpleArrayItem = {
    val members: List[GroundedValue] = new ArrayList[GroundedValue]()
    input.forEachOrFail(
      (item) =>
        if (item.getClass.getName.==(
          "com.saxonica.functions.extfn.ArrayMemberValue")) {
          members.add(
            item
              .asInstanceOf[ExternalObject[_]]
              .getObject
              .asInstanceOf[GroundedValue])
        } else {
          members.add(item)
        })
    val result: SimpleArrayItem = new SimpleArrayItem(members)
    result.knownToBeGrounded = true
    result
  }

}

class SimpleArrayItem extends AbstractArrayItem with ArrayItem {

  var membersList: List[GroundedValue] = _

  private var knownToBeGrounded: Boolean = false

  override def getOperandRoles(): Array[OperandRole] = Array(OperandRole.SINGLE_ATOMIC)

  def getMembersList: List[GroundedValue] = membersList

  def this(members: List[GroundedValue]) {
    this()
    this.membersList = members
  }

  def makeGrounded(): Unit = {
    if (!knownToBeGrounded) {
      this.synchronized {
        for (i <- 0 until membersList.size) {
          membersList.set(i, membersList.get(i).asInstanceOf[Sequence].materialize)
        }
        knownToBeGrounded = true
      }
    }
  }

  override def isArray(): Boolean = true

  override def isMap(): Boolean = false

  override def getAnnotations(): AnnotationList = AnnotationList.EMPTY

  def get(index: Int): GroundedValue = membersList.get(index)

  override def put(index: Int, newValue: GroundedValue): ArrayItem = {
    val a2: ImmutableArrayItem = new ImmutableArrayItem(this)
    a2.put(index, newValue)
  }

  def arrayLength(): Int = membersList.size

  def isEmpty: Boolean = membersList.isEmpty

  def members: Iterable[GroundedValue] = membersList.asScala.toList

  def removeSeveral(positions: IntSet): ArrayItem = {
    val a2: ImmutableArrayItem = new ImmutableArrayItem(this)
    a2.removeSeveral(positions)
  }

  def remove(pos: Int): ArrayItem = {
    val a2: ImmutableArrayItem = new ImmutableArrayItem(this)
    a2.remove(pos)
  }

  override def subArray(start: Int, end: Int): ArrayItem =
    new SimpleArrayItem(members.slice(start, end).asInstanceOf[List[GroundedValue]])

  override def insert(position: Int, member: GroundedValue): ArrayItem = {
    val a2: ImmutableArrayItem = new ImmutableArrayItem(this)
    a2.insert(position, member)
  }

  def concat(other: ArrayItem): ArrayItem = {
    val a2: ImmutableArrayItem = new ImmutableArrayItem(this)
    a2.concat(other)
  }

  override def toShortString: String = {
    val size: Int = getLength
    if (size == 0) {
      "[]"
    } else if (size > 5) {
      "[(:size " + size + ":)]"
    } else {
      val buff: FastStringBuffer = new FastStringBuffer(256)
      buff.append("[")
      for (entry <- members()) {
        buff.append(Err.depictSequence(entry).toString.trim())
        buff.append(", ")
      }
      if (size == 1) {
        buff.append("]")
      } else {
        buff.setCharAt(buff.length - 2, ']')
      }
      buff.toString.trim()
    }
  }

}
