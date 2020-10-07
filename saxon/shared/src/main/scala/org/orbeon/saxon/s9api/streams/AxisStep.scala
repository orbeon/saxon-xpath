package org.orbeon.saxon.s9api.streams

import org.orbeon.saxon.s9api.Axis

import org.orbeon.saxon.s9api.XdmItem

import org.orbeon.saxon.s9api.XdmNode

import java.util.stream.Stream

import Axis._

class AxisStep(private var axis: Axis) extends Step[XdmNode] {

  override def apply(node: XdmItem): Stream[_ <: XdmNode] =
    if (node.isInstanceOf[XdmNode])
      node.asInstanceOf[XdmNode].axisIterator(axis).asInstanceOf[XdmNode].stream()
    else Stream.empty()

}
