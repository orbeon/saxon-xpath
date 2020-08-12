package net.sf.saxon.s9api.streams

import net.sf.saxon.s9api.Axis

import net.sf.saxon.s9api.XdmItem

import net.sf.saxon.s9api.XdmNode

import java.util.stream.Stream

import Axis._

class AxisStep(private var axis: Axis) extends Step[XdmNode] {

  override def apply(node: XdmItem): Stream[_ <: XdmNode] =
    if (node.isInstanceOf[XdmNode])
      node.asInstanceOf[XdmNode].axisIterator(axis).asInstanceOf[XdmNode].stream()
    else Stream.empty()

}
