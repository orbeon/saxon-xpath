////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
package net.sf.saxon.expr.sort

import net.sf.saxon.expr.XPathContext

import net.sf.saxon.om.Item

import scala.jdk.CollectionConverters._

import net.sf.saxon.value.AtomicValue

import java.util.ArrayList

import java.util.List


class ItemWithMergeKeys(var baseItem: Item,
                        sKeys: SortKeyDefinitionList,
                        var sourceName: String,
                        context: XPathContext) {

  var sortKeyValues: List[AtomicValue] = new ArrayList[AtomicValue](sKeys.size)

  for (sKey <- sKeys.asScala) {
    sortKeyValues.add(
      sKey.getSortKey.evaluateItem(context).asInstanceOf[AtomicValue])
  }
}