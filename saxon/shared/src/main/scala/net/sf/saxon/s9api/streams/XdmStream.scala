package net.sf.saxon.s9api.streams

import net.sf.saxon.s9api.XdmAtomicValue

import net.sf.saxon.s9api.XdmItem

import net.sf.saxon.s9api.XdmNode

import net.sf.saxon.s9api.XdmValue

import java.util._

import java.util.function._

import java.util.stream._

class XdmStream[T <: XdmItem] extends ProxyStream[T] {

  var base: Stream[T] = _

  def this(base: Stream[T]) {
    this()
    this.base = base
  }

  def this(input: Optional[T]) = {
    this()
    this.base = input.map((s: T) => Stream.of(s)).orElseGet(Stream.empty[T]().asInstanceOf[Supplier[Stream[T]]])
  }

  def filter(predicate: Predicate[_ >: T]): XdmStream[T] = new XdmStream(base.filter(predicate))
  def map[R](mapper: Function[_ >: T, _ <: R]): Stream[R] = base.map(mapper)
  def mapToInt(mapper: ToIntFunction[_ >: T]): IntStream = base.mapToInt(mapper)
  def mapToLong(mapper: ToLongFunction[_ >: T]): LongStream = base.mapToLong(mapper)
  def mapToDouble(mapper: ToDoubleFunction[_ >: T]): DoubleStream = base.mapToDouble(mapper)
  def flatMap[R](function: Function[_ >: T, _ <: Stream[_ <: R]]): Stream[R] = base.flatMap(function)
  def flatMapToXdm[U <: XdmItem](mapper: Step[U]): XdmStream[U] = new XdmStream(base.flatMap(mapper))
  def flatMapToInt(mapper: Function[_ >: T, _ <: IntStream]): IntStream = base.flatMapToInt(mapper)
  def flatMapToLong(mapper: Function[_ >: T, _ <: LongStream]): LongStream = base.flatMapToLong(mapper)
  def flatMapToDouble(mapper: Function[_ >: T, _ <: DoubleStream]): DoubleStream = base.flatMapToDouble(mapper)
  def distinct(): XdmStream[T] = new XdmStream(base.distinct())
  def sorted(): XdmStream[T] = new XdmStream(base.sorted())
  def sorted(comparator: Comparator[_ >: T]): XdmStream[T] = new XdmStream(base.sorted(comparator))
  def peek(action: Consumer[_ >: T]): XdmStream[T] = new XdmStream(base.peek(action))
  def limit(maxSize: Long): XdmStream[T] = new XdmStream(base.limit(maxSize))
  def skip(n: Long): XdmStream[T] = new XdmStream(base.skip(n))
  def forEach(action: Consumer[_ >: T]): Unit = base.forEach(action)
  def forEachOrdered(action: Consumer[_ >: T]): Unit = base.forEachOrdered(action)
  def toArray: Array[AnyRef] = base.toArray()

/*  def toArray[A >: AnyRef](generator: IntFunction[Array[A]]): Array[AnyRef] =
    base.toArray(generator)*/


  def reduce(identity: T, accumulator: BinaryOperator[T]): T =
    base.reduce(identity, accumulator)

  def reduce(accumulator: BinaryOperator[T]): Optional[T] =
    base.reduce(accumulator)

  def reduce[U](identity: U,
                         accumulator: BiFunction[U, _ >: T, U],
                         combiner: BinaryOperator[U]): U =
    base.reduce(identity, accumulator, combiner)

  def collect[R](supplier: Supplier[R],
                          accumulator: BiConsumer[R, _ >: T],
                          combiner: BiConsumer[R, R]): R =
    base.collect(supplier, accumulator, combiner)

  def collect[R, A](collector: Collector[_ >: T, A, R]): R =
    base.collect(collector)

  def min(comparator: Comparator[_ >: T]): Optional[T] =
    base.min(comparator)

  def max(comparator: Comparator[_ >: T]): Optional[T] =
    base.max(comparator)

  def count(): Long = base.count()

  def anyMatch(predicate: Predicate[_ >: T]): Boolean =
    base.anyMatch(predicate)

  def allMatch(predicate: Predicate[_ >: T]): Boolean =
    base.allMatch(predicate)

  def noneMatch(predicate: Predicate[_ >: T]): Boolean =
    base.noneMatch(predicate)

  def findFirst(): Optional[T] = base.findFirst()

  def findAny(): Optional[T] = base.findAny()

  def iterator(): Iterator[T] = base.iterator()

  def spliterator(): Spliterator[T] = base.spliterator()

  def isParallel(): Boolean = base.isParallel

  def sequential(): Stream[T] = new XdmStream(base.sequential())

  def parallel(): Stream[T] = new XdmStream(base.parallel())

  def unordered(): Stream[T] = new XdmStream(base.unordered())

  def onClose(closeHandler: Runnable): Stream[T] =
    new XdmStream(base.onClose(closeHandler))

  def close(): Unit = {
    base.close()
  }

  def asXdmValue(): XdmValue = base.collect(XdmCollectors.asXdmValue())

  def asList(): List[T] = base.collect(Collectors.toList())

  def asListOfNodes(): List[XdmNode] =
    base.collect(XdmCollectors.asListOfNodes())

  def asOptionalNode(): Optional[XdmNode] =
    base.collect(XdmCollectors.asOptionalNode())

  def asNode(): XdmNode = base.collect(XdmCollectors.asNode())

  def asListOfAtomic(): List[XdmAtomicValue] =
    base.collect(XdmCollectors.asListOfAtomic())

  def asOptionalAtomic(): Optional[XdmAtomicValue] =
    base.collect(XdmCollectors.asOptionalAtomic())

  def asAtomic(): XdmAtomicValue = base.collect(XdmCollectors.asAtomic())

  def asOptionalString(): Optional[String] =
    base.collect(XdmCollectors.asOptionalString())

  def asString(): String = base.collect(XdmCollectors.asString())

  def first(): XdmStream[T] = {
    val result: Optional[T] = base.findFirst()
    new XdmStream(result)
  }

  def exists(): Boolean = {
    val result: Optional[T] = base.findFirst()
    result.isPresent
  }

  def last(): XdmStream[T] = {
    val result: Optional[T] = base.reduce((first, second) => second)
    new XdmStream(result)
  }

  def at(position: Int): Optional[T] = base.skip(position).findFirst()

  def subStream(start: Int, end: Int): XdmStream[T] = {
    var srt = start
    if (srt < 0) {
      srt = 0
    }
    if (end <= srt) {
      new XdmStream(Stream.empty[T]())
    }
    new XdmStream(base.skip(srt).limit(end - srt))
  }

  def untilFirstInclusive(predicate: Predicate[_ >: XdmItem]): XdmStream[T] = {
    val stoppable: Stream[T] = base.peek((item) =>
      if (predicate.test(item)) {
        base.close()
      })
    new XdmStream(stoppable)
  }

  def untilFirstExclusive(predicate: Predicate[_ >: XdmItem]): XdmStream[T] = {
    val stoppable: Stream[T] = base.peek((item) =>
      if (predicate.test(item)) {
        base.close()
      })
    new XdmStream(stoppable.filter(predicate.negate()))
  }
}
