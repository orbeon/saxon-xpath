package net.sf.saxon.functions

import net.sf.saxon.expr.XPathContext

import net.sf.saxon.om.Item

import net.sf.saxon.trans.XPathException

import net.sf.saxon.value.AtomicValue

import net.sf.saxon.value.Int64Value

import net.sf.saxon.value.IntegerValue

object AccessorFn {

  object Component extends Enumeration {

    val YEAR : Component.Component =new Component()

    val MONTH : Component.Component =new Component()

    val DAY : Component.Component =new Component()

    val HOURS : Component.Component =new Component()

    val MINUTES : Component.Component =new Component()

    val SECONDS : Component.Component =new Component()

    val TIMEZONE : Component.Component =new Component()

    val LOCALNAME : Component.Component =new Component()

    val NAMESPACE : Component.Component =new Component()

    val PREFIX : Component.Component =new Component()

    val MICROSECONDS : Component.Component =new Component()

    val NANOSECONDS : Component.Component =new Component()

    val WHOLE_SECONDS : Component.Component =new Component()

    val YEAR_ALLOWING_ZERO : Component.Component =new Component()

    class Component extends Val

    implicit def convertValue(v: Value) : Component.Component =v.asInstanceOf[Component]

  }

  class YearFromDateTime extends AccessorFn {

    override def getComponentId(): Component.Component = Component.YEAR

  }

  class MonthFromDateTime extends AccessorFn {

    override def getComponentId():  Component.Component = Component.MONTH

  }

  class DayFromDateTime extends AccessorFn {

    override def getComponentId():  Component.Component = Component.DAY

  }

  class HoursFromDateTime extends AccessorFn {

    override def getComponentId():  Component.Component = Component.HOURS

  }

  class MinutesFromDateTime extends AccessorFn {

    override def getComponentId():  Component.Component = Component.MINUTES

  }

  class SecondsFromDateTime extends AccessorFn {

    override def getComponentId():  Component.Component = Component.SECONDS

  }

  class TimezoneFromDateTime extends AccessorFn {

    override def getComponentId() : Component.Component =Component.TIMEZONE

  }

  class YearFromDate extends AccessorFn {

    override def getComponentId() : Component.Component =Component.YEAR

  }

  class MonthFromDate extends AccessorFn {

    override def getComponentId() : Component.Component =Component.MONTH

  }

  class DayFromDate extends AccessorFn {

    override def getComponentId() : Component.Component =Component.DAY

  }

  class TimezoneFromDate extends AccessorFn {

    override def getComponentId() : Component.Component =Component.TIMEZONE

  }

  class HoursFromTime extends AccessorFn {

    override def getComponentId() : Component.Component =Component.HOURS

  }

  class MinutesFromTime extends AccessorFn {

    override def getComponentId() : Component.Component =Component.MINUTES

  }

  class SecondsFromTime extends AccessorFn {

    override def getComponentId() : Component.Component =Component.SECONDS

  }

  class TimezoneFromTime extends AccessorFn {

    override def getComponentId() : Component.Component =Component.TIMEZONE

  }

  class YearsFromDuration extends AccessorFn {

    override def getComponentId() : Component.Component =Component.YEAR

  }

  class MonthsFromDuration extends AccessorFn {

    override def getComponentId() : Component.Component =Component.MONTH

  }

  class DaysFromDuration extends AccessorFn {

    override def getComponentId() : Component.Component =Component.DAY

  }

  class HoursFromDuration extends AccessorFn {

    override def getComponentId() : Component.Component =Component.HOURS

  }

  class MinutesFromDuration extends AccessorFn {

    override def getComponentId() : Component.Component =Component.MINUTES

  }

  class SecondsFromDuration extends AccessorFn {

    override def getComponentId() : Component.Component =Component.SECONDS

  }

  class LocalNameFromQName extends AccessorFn {

    override def getComponentId() : Component.Component =Component.LOCALNAME

  }

  class PrefixFromQName extends AccessorFn {

    override def getComponentId() : Component.Component =Component.PREFIX

  }

  class NamespaceUriFromQName extends AccessorFn {

    override def getComponentId() : Component.Component =Component.NAMESPACE

  }

}

abstract class AccessorFn extends ScalarSystemFunction {

  def getComponentId(): AccessorFn.Component.Component

  override  def getIntegerBounds(): Array[IntegerValue] = getComponentId match {
    case AccessorFn.Component.YEAR =>
      Array(Int64Value.makeIntegerValue(-100000),
        Int64Value.makeIntegerValue(+100000))
    case AccessorFn.Component.MONTH =>
      Array(Int64Value.makeIntegerValue(-11), Int64Value.makeIntegerValue(+11))
    case AccessorFn.Component.DAY =>
      Array(Int64Value.makeIntegerValue(-31), Int64Value.makeIntegerValue(+31))
    case AccessorFn.Component.HOURS =>
      Array(Int64Value.makeIntegerValue(-24), Int64Value.makeIntegerValue(+24))
    case AccessorFn.Component.MINUTES =>
      Array(Int64Value.makeIntegerValue(-59), Int64Value.makeIntegerValue(+59))
    case AccessorFn.Component.SECONDS =>
      Array(Int64Value.makeIntegerValue(-59), Int64Value.makeIntegerValue(+59))
    case _ => null

  }

  def getRequiredComponent(): Int = getComponentId.id

  def evaluate(item: Item, context: XPathContext): AtomicValue =
    item.asInstanceOf[AtomicValue].getComponent(getComponentId)

  override def getCompilerName(): String = "AccessorFnCompiler"

}
