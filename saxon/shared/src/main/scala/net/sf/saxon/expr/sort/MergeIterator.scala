package net.sf.saxon.expr.sort

import net.sf.saxon.model.Type
import net.sf.saxon.om.SequenceIterator
import net.sf.saxon.trans.XPathException
import net.sf.saxon.tree.iter.LookaheadIterator
import net.sf.saxon.value.AtomicValue
import net.sf.saxon.value.ObjectValue
import java.util.EnumSet

import SequenceIterator.Property._

class MergeIterator(p1: SequenceIterator,
                    p2: SequenceIterator,
                    private var comparer: ItemOrderComparer)
  extends SequenceIterator
    with LookaheadIterator {

  private var e1: SequenceIterator = p1

  private var e2: SequenceIterator = p2

  private var nextItem1: ObjectValue[ItemWithMergeKeys] =
    e1.next().asInstanceOf[ObjectValue[ItemWithMergeKeys]]

  private var nextItem2: ObjectValue[ItemWithMergeKeys] =
    e2.next().asInstanceOf[ObjectValue[ItemWithMergeKeys]]

  def hasNext(): Boolean = nextItem1 != null || nextItem2 != null

  def next(): ObjectValue[ItemWithMergeKeys] = {
    if (nextItem1 != null && nextItem2 != null) {
      var c: Int = 0
      try c = comparer.compare(nextItem1, nextItem2)
      catch {
        case e: ClassCastException => {
          val i1: ItemWithMergeKeys = nextItem1.getObject
          val i2: ItemWithMergeKeys = nextItem2.getObject
          val a1: AtomicValue = i1.sortKeyValues.get(0)
          val a2: AtomicValue = i2.sortKeyValues.get(0)
          val err: XPathException = new XPathException(
            "Merge key values are of non-comparable types (" + Type
              .displayTypeName(a1) +
              " and " +
              Type.displayTypeName(a2) +
              ")",
            "XTTE2230")
          err.setIsTypeError(true)
          throw err
        }

      }
      if (c <= 0) {
        val current: ObjectValue[ItemWithMergeKeys] = nextItem1
        nextItem1 = e1.next().asInstanceOf[ObjectValue[ItemWithMergeKeys]]
        current
      } else {
        val current: ObjectValue[ItemWithMergeKeys] = nextItem2
        nextItem2 = e2.next().asInstanceOf[ObjectValue[ItemWithMergeKeys]]
        current
      }
    }
    if (nextItem1 != null) {
      val current: ObjectValue[ItemWithMergeKeys] = nextItem1
      nextItem1 = e1.next().asInstanceOf[ObjectValue[ItemWithMergeKeys]]
      current
    }
    if (nextItem2 != null) {
      val current: ObjectValue[ItemWithMergeKeys] = nextItem2
      nextItem2 = e2.next().asInstanceOf[ObjectValue[ItemWithMergeKeys]]
      current
    }
    null
  }

  override def close(): Unit = {
    e1.close()
    e2.close()
  }

  override def getProperties(): Set[Property] = Set(LOOKAHEAD)

}
