////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
package net.sf.saxon.tree.iter

import net.sf.saxon.om.NodeInfo




class ConcatenatingAxisIterator(var first: AxisIterator,
                                var second: AxisIterator)
    extends AxisIterator {

  var active: AxisIterator = first

  /*@Nullable*/

  def next(): NodeInfo = {
    var n: NodeInfo = active.next()
    if (n == null && active == first) {
      active = second
      n = second.next()
    }
    n
  }

  override def close(): Unit = {
    first.close()
    second.close()
  }

}