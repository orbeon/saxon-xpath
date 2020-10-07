package org.orbeon.saxon.functions

import org.orbeon.saxon.expr.XPathContext
import org.orbeon.saxon.lib.StringCollator
import org.orbeon.saxon.om.Sequence
import org.orbeon.saxon.value.StringValue
import java.util.List

import org.orbeon.saxon.functions.Sort_1.ItemToBeSorted

class Sort_2 extends Sort_1 {

  override def call(context: XPathContext, arguments: Array[Sequence]): Sequence = {
    val inputList: List[ItemToBeSorted] = getItemsToBeSorted(arguments(0))
    doSort(inputList, getCollation(context, arguments(1)), context)
  }

   def getCollation(context: XPathContext,
                             collationArg: Sequence): StringCollator = {
    val secondArg: StringValue = collationArg.head.asInstanceOf[StringValue]
    if (secondArg == null) {
      context.getConfiguration.getCollation(
        getRetainedStaticContext.getDefaultCollationName)
    } else {
      context.getConfiguration
        .getCollation(secondArg.getStringValue, getStaticBaseUriString)
    }
  }

}
