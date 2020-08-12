package net.sf.saxon.s9api

import net.sf.saxon.om.AxisInfo


object Axis extends Enumeration {

  val ANCESTOR: Axis = new Axis(AxisInfo.ANCESTOR)

  val ANCESTOR_OR_SELF: Axis = new Axis(AxisInfo.ANCESTOR_OR_SELF)

  val ATTRIBUTE: Axis = new Axis(AxisInfo.ATTRIBUTE)

  val CHILD: Axis = new Axis(AxisInfo.CHILD)

  val DESCENDANT: Axis = new Axis(AxisInfo.DESCENDANT)

  val DESCENDANT_OR_SELF: Axis = new Axis(AxisInfo.DESCENDANT_OR_SELF)

  val FOLLOWING: Axis = new Axis(AxisInfo.FOLLOWING)

  val FOLLOWING_SIBLING: Axis = new Axis(AxisInfo.FOLLOWING_SIBLING)

  val PARENT: Axis = new Axis(AxisInfo.PARENT)

  val PRECEDING: Axis = new Axis(AxisInfo.PRECEDING)

  val PRECEDING_SIBLING: Axis = new Axis(AxisInfo.PRECEDING_SIBLING)

  val SELF: Axis = new Axis(AxisInfo.SELF)

  val NAMESPACE: Axis = new Axis(AxisInfo.NAMESPACE)

  class Axis (var number: Int) extends Val {

    def getAxisNumber(): Int = number

  }

  implicit def convertValue(v: Value): Axis = v.asInstanceOf[Axis]

}
