////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
package net.sf.saxon.functions.hof

import net.sf.saxon.expr.XPathContext
import net.sf.saxon.functions.{Sort_2, SystemFunction}
import net.sf.saxon.om.Function
import net.sf.saxon.om.Item
import net.sf.saxon.om.Sequence
import net.sf.saxon.om.SequenceIterator
import net.sf.saxon.trans.XPathException
import java.util.ArrayList
import java.util.List
import net.sf.saxon.functions.Sort_1.ItemToBeSorted

class Sort_3 extends Sort_2 {

  override def call(context: XPathContext, arguments: Array[Sequence]): Sequence = {
    val input: Sequence = arguments(0)
    val inputList: List[ItemToBeSorted] = new ArrayList[ItemToBeSorted]()
    var i: Int = 0
    val key: Function = arguments(2).head().asInstanceOf[Function]
    val iterator: SequenceIterator = input.iterate()
    var item: Item = null
    while ((item = iterator.next()) != null) {
      val member: ItemToBeSorted = new ItemToBeSorted()
      member.value = item
      member.originalPosition = {
        i
      }
      i = i + 1
      member.sortKey = SystemFunction.dynamicCall(key, context, Array(item)).materialize()
      inputList.add(member)
    }
    doSort(inputList, getCollation(context, arguments(1)), context)
  }
}
