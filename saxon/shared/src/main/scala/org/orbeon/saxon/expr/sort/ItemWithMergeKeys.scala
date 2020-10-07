////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
package org.orbeon.saxon.expr.sort

import org.orbeon.saxon.expr.XPathContext

import org.orbeon.saxon.om.Item

//import scala.collection.compat._
import scala.jdk.CollectionConverters._

import org.orbeon.saxon.value.AtomicValue

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