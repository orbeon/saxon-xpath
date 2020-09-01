////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
package net.sf.saxon.ma.map

import net.sf.saxon.expr.XPathContext

import net.sf.saxon.functions.SystemFunction

import net.sf.saxon.om.Sequence

import net.sf.saxon.om.SequenceIterator

import net.sf.saxon.trans.XPathException
import scala.jdk.CollectionConverters._


/**
 * This is a variant of the map:new() or map:merge() function which differs in that duplicate
 * keys are considered an error. It is not available directly to users, but underpins the map
 * constructor expression in XPath and the xsl:map instruction in XSLT.
 *
 * Moved to the Saxon namespace in 9.8 - see bug 2740 and test case function-available-1017
 */
class MapCreate extends SystemFunction {

  def call(context: XPathContext, arguments: Array[Sequence]): Sequence = {
    val iter: SequenceIterator = arguments(0).iterate()
    var baseMap: MapItem = iter.next().asInstanceOf[MapItem]
    if (baseMap == null) {
      new HashTrieMap()
    } else {
      if (!(baseMap.isInstanceOf[HashTrieMap])) {
        baseMap = HashTrieMap.copy(baseMap)
      }
      var next: MapItem = null
      while (({
        next = iter.next().asInstanceOf[MapItem]
        next
      }) != null) for (pair <- next.keyValuePairs().asScala) {
        if (baseMap.get(pair.key) != null) {
          throw new XPathException(
            "Duplicate key value (" + pair.key + ") in map",
            "XQDY0137")
        } else {
          baseMap = baseMap.addEntry(pair.key, pair.value)
        }
      }
      baseMap
    }
  }

  override def getStreamerName: String = "NewMap"

}