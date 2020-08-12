////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
package net.sf.saxon.tree.iter

import net.sf.saxon.om.NodeInfo

import net.sf.saxon.trans.XPathException




class PrependAxisIterator(start: NodeInfo, base: AxisIterator)
    extends PrependSequenceIterator(start, base)
    with AxisIterator {

  /*@Nullable*/

  override def next(): NodeInfo = super.next().asInstanceOf[NodeInfo]

}
