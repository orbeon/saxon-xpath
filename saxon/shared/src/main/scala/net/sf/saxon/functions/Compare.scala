package net.sf.saxon.functions

import net.sf.saxon.expr.XPathContext
import net.sf.saxon.expr.sort.AtomicComparer
import net.sf.saxon.expr.sort.GenericAtomicComparer
import net.sf.saxon.om.{Item, Sequence, ZeroOrOne}
import net.sf.saxon.trans.XPathException
import net.sf.saxon.value.Int64Value
import net.sf.saxon.value.StringValue
import Compare._

object Compare {

  private def compare(s1: StringValue,
                      s2: StringValue,
                      comparer: AtomicComparer): Int64Value = {
    if (s1 == null || s2 == null) return null
    val result: Int = comparer.compareAtomicValues(s1, s2)
    if (result < 0) {
      Int64Value.MINUS_ONE
    } else if (result > 0) {
      Int64Value.PLUS_ONE
    } else {
     Int64Value.ZERO
    }
  }

}

class Compare extends CollatingFunctionFixed {

  def call(context: XPathContext, arguments: Array[Sequence]): ZeroOrOne[Item] = {
    val arg0: StringValue = arguments(0).head().asInstanceOf[StringValue]
    val arg1: StringValue = arguments(1).head().asInstanceOf[StringValue]
    val comparer: GenericAtomicComparer =
      new GenericAtomicComparer(getStringCollator, context)
    val result: Int64Value = compare(arg0, arg1, comparer)
    new ZeroOrOne(result)
  }
}
