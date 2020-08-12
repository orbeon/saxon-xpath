package net.sf.saxon.functions

import net.sf.saxon.expr.XPathContext
import net.sf.saxon.lib.StringCollator
import net.sf.saxon.om.Sequence
import net.sf.saxon.value.StringValue
import java.util.List

import net.sf.saxon.functions.Sort_1.ItemToBeSorted

class Sort_2 extends Sort_1 {

  override def call(context: XPathContext, arguments: Array[Sequence]): Sequence = {
    val inputList: List[ItemToBeSorted] = getItemsToBeSorted(arguments(0))
    doSort(inputList, getCollation(context, arguments(1)), context)
  }

   def getCollation(context: XPathContext,
                             collationArg: Sequence): StringCollator = {
    val secondArg: StringValue = collationArg.head().asInstanceOf[StringValue]
    if (secondArg == null) {
      context.getConfiguration.getCollation(
        getRetainedStaticContext.getDefaultCollationName)
    } else {
      context.getConfiguration
        .getCollation(secondArg.getStringValue, getStaticBaseUriString)
    }
  }

}
