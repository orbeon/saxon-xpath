package net.sf.saxon.om

import net.sf.saxon.trans.XPathException

import java.util.Iterator




/**
  * Class IteratorWrapper - provides an an SequenceIterator over a Java Iterator.
  */
class IteratorWrapper(var iterator: Iterator[_ <: Item])
    extends SequenceIterator {

  /**
    * Get the next item in the Iterator
    *
    * @return the next item in the iterator, or null if there are no more items. Once a call
    * on next() has returned null, no further calls should be made.
    */
  override def next(): Item = if (iterator.hasNext) iterator.next() else null

}
