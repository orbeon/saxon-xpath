package net.sf.saxon.ma.trie

import java.util
import java.util.Collections
import java.util.Iterator
import java.util.LinkedList
import java.util.List

import scala.jdk.CollectionConverters._
import scala.util.control.Breaks._

object ImmutableHashTrieMap {


  def empty[K, V](): ImmutableHashTrieMap[K, V] = EMPTY_NODE[K, V]

  def EMPTY_NODE[K, V]: ImmutableHashTrieMap[K, V] = new EmptyHashNode()


  private def getBucket[K](shift: Int, key: K): Int =
    key.hashCode >> shift & MASK

  private val BITS: Int = 5

  private val FANOUT: Int = 1 << BITS

  private val MASK: Int = FANOUT - 1

  private class EmptyHashNode[K, V] extends ImmutableHashTrieMap[K, V] {

    override def put(shift: Int, key: K, value: V): ImmutableHashTrieMap[K, V] = new EntryHashNode[K, V](key, value)

    override def remove(shift: Int, key: K): ImmutableHashTrieMap[K, V] = this

    override def isArrayNode(): Boolean = false

    override def get(shift: Int, key: K): V = null.asInstanceOf[V]

    def iterator(): Iterator[Tuple2[K, V]] =
      Collections.emptySet[Tuple2[K, V]]().iterator()

  }

  private class EntryHashNode[K, V](private val key: K, private val value: V) extends ImmutableHashTrieMap[K, V] {

    override def put(shift: Int,
                     key: K,
                     value: V): ImmutableHashTrieMap[K, V] = {
      if (this.key == key) {
        new EntryHashNode(key, value)
      } else if (this.key.hashCode == key.hashCode) {
        new ListHashNode(new Tuple2(this.key, this.value),
          new Tuple2(key, value))
      }
      newArrayHashNode(shift,
        this.key.hashCode,
        this,
        key.hashCode,
        new EntryHashNode[K, V](key, value))
    }

    override def remove(shift: Int, key: K): ImmutableHashTrieMap[K, V] = {
      if (this.key == key) {
        empty()
      }
      this
    }

    override def isArrayNode(): Boolean = false

    override def get(shift: Int, key: K): V = {
      if (this.key == key) {
        return value
      }
      null.asInstanceOf[V]
    }

    def iterator(): Iterator[Tuple2[K, V]] =
      Collections.singleton(new Tuple2(key, value)).iterator()

  }

  private class ListHashNode[K, V]
    extends ImmutableHashTrieMap[K, V] {

    var entry1: Tuple2[K, V] = _
    var entry2: Tuple2[K, V] = _
    private var entries: ImmutableList[Tuple2[K, V]] = _

    def this(entry1: Tuple2[K, V], entry2: Tuple2[K, V]) {
      this()
      assert(entry1._1.hashCode == entry2._1.hashCode)
      entries = ImmutableList.empty[Tuple2[K, V]]().prepend(entry1).prepend(entry2)
    }


    def this(entries: ImmutableList[Tuple2[K, V]]) = {
      this()
      assert(!entries.isEmpty)
      assert(!entries.tail().isEmpty)
      this.entries = entries
    }

    override def put(shift: Int,
                     key: K,
                     value: V): ImmutableHashTrieMap[K, V] = {
      if (entries.head._1.hashCode != key.hashCode) {
        newArrayHashNode(shift,
          entries.head._1.hashCode,
          this,
          key.hashCode,
          new EntryHashNode(key, value))
      }
      var newList: ImmutableList[Tuple2[K, V]] = ImmutableList.empty()
      var found: Boolean = false
      for (entry <- entries.asScala) {
        if (entry._1 == key) {
          newList = newList.prepend(new Tuple2(key, value))
          found = true
        } else {
          newList = newList.prepend(entry)
        }
      }
      if (!found) {
        newList = newList.prepend(new Tuple2(key, value))
      }
      new ListHashNode(newList)
    }

    override def remove(shift: Int, key: K): ImmutableHashTrieMap[K, V] = {
      var newList: ImmutableList[Tuple2[K, V]] = ImmutableList.empty()
      var size: Int = 0
      for (entry <- entries.asScala if entry._1 != key) {
        newList = newList.prepend(entry)
        size += 1
      }
      if (size == 1) {
        val entry: Tuple2[K, V] = newList.head
        new EntryHashNode(entry._1, entry._2)
      }
      new ListHashNode(newList)
    }

    override def isArrayNode(): Boolean = false

    override def get(shift: Int, key: K): V =
      entries.asScala.find(_._1 == key).map(_._2).getOrElse(null.asInstanceOf[V])

    def iterator(): Iterator[Tuple2[K, V]] = new Iterator[Tuple2[K, V]]() {
      private var curList: ImmutableList[Tuple2[K, V]] =
        ListHashNode.this.entries

      def hasNext(): Boolean = !curList.isEmpty

      def next(): Tuple2[K, V] = {
        val retVal: Tuple2[K, V] = curList.head
        curList = curList.tail()
        retVal
      }

      override def remove(): Unit = {
        throw new UnsupportedOperationException()
      }
    }

  }

  private def newArrayHashNode[K, V](
                                      shift: Int,
                                      hash1: Int,
                                      subNode1: ImmutableHashTrieMap[K, V],
                                      hash2: Int,
                                      subNode2: ImmutableHashTrieMap[K, V]): ImmutableHashTrieMap[K, V] = {
    var curShift: Int = shift
    var h1: Int = hash1 >> shift & MASK
    var h2: Int = hash2 >> shift & MASK
    val buckets: List[Integer] = new LinkedList[Integer]()
    while (h1 == h2) {
      buckets.add(0, h1)
      curShift += BITS
      h1 = hash1 >> curShift & MASK
      h2 = hash2 >> curShift & MASK
    }
    var newNode: ImmutableHashTrieMap[K, V] =
      new BranchedArrayHashNode[K, V](h1, subNode1, h2, subNode2)
    for (bucket <- buckets.asScala) {
      newNode = new SingletonArrayHashNode[K, V](bucket, newNode)
    }
    newNode
  }

  private abstract class ArrayHashNode[K, V]
    extends ImmutableHashTrieMap[K, V] {

    override def isArrayNode(): Boolean = true

  }

  private class BranchedArrayHashNode[K, V] extends ArrayHashNode[K, V] {

    private var subnodes: Array[ImmutableHashTrieMap[K, V]] = new Array[ImmutableHashTrieMap[K, V]](FANOUT)
    private var size: Int = 0

    def this(h1: Int,
             subNode1: ImmutableHashTrieMap[K, V],
             h2: Int,
             subNode2: ImmutableHashTrieMap[K, V]) {
      this()
      assert(h1 != h2)
      size = 2
      for (i <- 0 until FANOUT) {
        val res: ImmutableHashTrieMap[K, V] = if (i == h1) subNode1 else if (i == h2) subNode2 else EMPTY_NODE.asInstanceOf[ImmutableHashTrieMap[K, V]]
        subnodes(i) = res
      }

    }


    def this(size: Int, subnodes: Array[ImmutableHashTrieMap[K, V]]) = {
      this()
      assert(subnodes.length == FANOUT)
      this.size = size
      this.subnodes = subnodes
    }

    override def put(shift: Int,
                     key: K,
                     value: V): ImmutableHashTrieMap[K, V] = {
      val bucket: Int = getBucket(shift, key)
      val newNodes: Array[ImmutableHashTrieMap[K, V]] =
        Array.ofDim[ImmutableHashTrieMap[K, V]](FANOUT)
      System.arraycopy(subnodes, 0, newNodes, 0, FANOUT)
      val newSize: Int = if (newNodes(bucket) == EMPTY_NODE) size + 1 else size
      newNodes(bucket) = newNodes(bucket).put(shift + BITS, key, value)
      new BranchedArrayHashNode[K, V](newSize, newNodes)
    }

    override def remove(shift: Int, key: K): ImmutableHashTrieMap[K, V] = {
      val bucket: Int = getBucket(shift, key)
      if (subnodes(bucket) == EMPTY_NODE) {
        return this
      }
      val newNodes: Array[ImmutableHashTrieMap[K, V]] =
        Array.ofDim[ImmutableHashTrieMap[K, V]](FANOUT)
      System.arraycopy(subnodes, 0, newNodes, 0, FANOUT)
      newNodes(bucket) = newNodes(bucket).remove(shift + BITS, key)
      val newSize: Int = if (newNodes(bucket) == EMPTY_NODE) size - 1 else size
      if (newSize == 1) {
        var orphanedBucket: Int = -1
        breakable { for (i <- 0 until FANOUT if newNodes(i) != EMPTY_NODE) {
          orphanedBucket = i
          break()
        }
      }
        val orphanedEntry: ImmutableHashTrieMap[K, V] = subnodes(
          orphanedBucket)
        if (orphanedEntry.isArrayNode) {
          new SingletonArrayHashNode(orphanedBucket, orphanedEntry)
        }
        return orphanedEntry
      }
      new BranchedArrayHashNode(newSize, newNodes)
    }

    override def get(shift: Int, key: K): V = {
      val bucket: Int = getBucket(shift, key)
      subnodes(bucket).get(shift + BITS, key)
    }

    def iterator(): Iterator[Tuple2[K, V]] = new Iterator[Tuple2[K, V]]() {
      private var bucket: Int = 0

      private var childIterator: Iterator[Tuple2[K, V]] =
        subnodes(0).iterator()

      def hasNext(): Boolean = {
        if (childIterator.hasNext) {
          return true
        }
        bucket += 1
        while (bucket < FANOUT) {
          childIterator = subnodes(bucket).iterator()
          if (childIterator.hasNext) {
            return true
          }
          bucket += 1
        }
        false
      }

      def next(): Tuple2[K, V] = childIterator.next()

      override def remove(): Unit = {
        throw new UnsupportedOperationException()
      }
    }

  }

  private class SingletonArrayHashNode[K, V](
                                              private val bucket: Int,
                                              private val subnode: ImmutableHashTrieMap[K, V])
    extends ArrayHashNode[K, V] {

    assert(subnode.isInstanceOf[ArrayHashNode[_, _]])

    override def put(shift: Int,
                     key: K,
                     value: V): ImmutableHashTrieMap[K, V] = {
      val bucket: Int = getBucket(shift, key)
      if (bucket == this.bucket) {
        new SingletonArrayHashNode(bucket,
          subnode.put(shift + BITS, key, value))
      }
      new BranchedArrayHashNode(this.bucket,
        subnode,
        bucket,
        new EntryHashNode(key, value))
    }

    override def remove(shift: Int, key: K): ImmutableHashTrieMap[K, V] = {
      val bucket: Int = getBucket(shift, key)
      if (bucket == this.bucket) {
        val newNode: ImmutableHashTrieMap[K, V] =
          subnode.remove(shift + BITS, key)
        if (!newNode.isArrayNode) {
          return newNode
        }
        new SingletonArrayHashNode(bucket, newNode)
      }
      this
    }

    override def get(shift: Int, key: K): V = {
      val bucket: Int = getBucket(shift, key)
      if (bucket == this.bucket) {
        subnode.get(shift + BITS, key)
      }
      null.asInstanceOf[V]
    }

    def iterator(): Iterator[Tuple2[K, V]] = subnode.iterator()

  }

}

abstract class ImmutableHashTrieMap[K, V]
  extends ImmutableMap[K, V]
    with java.lang.Iterable[Tuple2[K, V]] {

  def put(key: K, value: V): ImmutableHashTrieMap[K, V] = put(0, key, value)

  def remove(key: K): ImmutableHashTrieMap[K, V] = remove(0, key)

  def get(key: K): V = get(0, key)

  def put(shift: Int, key: K, value: V): ImmutableHashTrieMap[K, V]

  def remove(shift: Int, key: K): ImmutableHashTrieMap[K, V]

  def get(shift: Int, key: K): V

  def isArrayNode: Boolean

}
