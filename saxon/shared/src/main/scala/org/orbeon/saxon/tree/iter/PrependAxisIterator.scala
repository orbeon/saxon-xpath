////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
package org.orbeon.saxon.tree.iter

import org.orbeon.saxon.om.NodeInfo

import org.orbeon.saxon.trans.XPathException




class PrependAxisIterator(start: NodeInfo, base: AxisIterator)
    extends PrependSequenceIterator(start, base)
    with AxisIterator {

  /*@Nullable*/

  override def next(): NodeInfo = super.next().asInstanceOf[NodeInfo]

}
