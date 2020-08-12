package net.sf.saxon.s9api.streams

import net.sf.saxon.s9api.XdmItem
import java.util.function.Function
import java.util.function.Predicate
import java.util.stream.Stream

import net.sf.saxon.om.Item

abstract class Step[T <: XdmItem] extends Function[XdmItem, Stream[_ <: T]] {

  def where(predicate: Predicate[_ >: T]): Step[T] = {
    val base: Step[T] = this
    new Step[T]() {
      override def apply(item: XdmItem): Stream[_ <: T] =
        base.apply(item).filter(predicate)
    }
  }

  def cat(other: Step[T]): Step[T] = {
    val base: Step[T] = this
    new Step[T]() {
      override def apply(item: XdmItem): Stream[T] =
        Stream.concat(base.apply(item), other.apply(item))
    }
  }

  def first(): Step[T] = {
    val base: Step[T] = this
    new Step[T]() {
      override def apply(item: XdmItem): Stream[_ <: T] =
        base.apply(item).limit(1)
    }
  }

  def last(): Step[T] = {
    val base: Step[T] = this
    (item: XdmItem) =>
      base
        .apply(item)
        .reduce((f1, f2) => f2)
        .map((t: T) => Stream.of(t))
        .orElseGet(() => Stream.empty)
  }

  def at(index: Long): Step[T] = {
    val base: Step[T] = this
    (item: XdmItem) => base.apply(item).skip(index).limit(1)
  }

  def then[U <: XdmItem](next: Step[U]): Step[U] = {
    val me: Step[T] = this
    new Step[U]() {
      override def apply(item: XdmItem): Stream[_ <: U] =
        me.apply(item).flatMap(next)
    }
  }

}
