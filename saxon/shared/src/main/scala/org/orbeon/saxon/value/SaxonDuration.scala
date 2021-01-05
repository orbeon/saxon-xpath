package org.orbeon.saxon.value

import org.orbeon.saxon.functions.AccessorFn

import org.orbeon.saxon.lib.NamespaceConstant

import org.orbeon.saxon.trans.XPathException

import javax.xml.datatype.DatatypeConstants

import javax.xml.datatype.Duration

import javax.xml.namespace.QName

import java.math.BigDecimal

import java.math.BigInteger

import java.util.Calendar


class SaxonDuration(private var duration: DurationValue) extends Duration {

  def getDurationValue: DurationValue = duration

  override def getXMLSchemaType: QName =
    duration match {
      case _: DayTimeDurationValue   =>
        new QName(NamespaceConstant.SCHEMA, "dayTimeDuration")
      case _: YearMonthDurationValue =>
        new QName(NamespaceConstant.SCHEMA, "yearMonthDuration")
      case _                         =>
        new QName(NamespaceConstant.SCHEMA, "duration")
    }

  def getSign: Int = duration.signum()

  def getField(field: DatatypeConstants.Field): Number =
    if (field == DatatypeConstants.YEARS) {
      BigInteger.valueOf(
        duration
          .getComponent(AccessorFn.Component.YEAR)
          .asInstanceOf[Int64Value]
          .longValue)
    } else if (field == DatatypeConstants.MONTHS) {
      BigInteger.valueOf(
        duration
          .getComponent(AccessorFn.Component.MONTH)
          .asInstanceOf[Int64Value]
          .longValue)
    } else if (field == DatatypeConstants.DAYS) {
      BigInteger.valueOf(
        duration
          .getComponent(AccessorFn.Component.DAY)
          .asInstanceOf[Int64Value]
          .longValue)
    } else if (field == DatatypeConstants.HOURS) {
      BigInteger.valueOf(
        duration
          .getComponent(AccessorFn.Component.HOURS)
          .asInstanceOf[Int64Value]
          .longValue)
    } else if (field == DatatypeConstants.MINUTES) {
      BigInteger.valueOf(
        duration
          .getComponent(AccessorFn.Component.MINUTES)
          .asInstanceOf[Int64Value]
          .longValue)
    } else if (field == DatatypeConstants.SECONDS) {
      duration
        .getComponent(AccessorFn.Component.SECONDS)
        .asInstanceOf[BigDecimalValue]
        .getDecimalValue
    } else {
      throw new IllegalArgumentException("Invalid field")
    }

  def isSet(field: DatatypeConstants.Field): Boolean = true

  def add(rhs: Duration): Duration =
    new SaxonDuration(duration.add(rhs.asInstanceOf[SaxonDuration].duration))

  override def subtract(rhs: Duration): Duration =
    new SaxonDuration(
      duration.subtract(rhs.asInstanceOf[SaxonDuration].duration))

  def addTo(calendar: Calendar): Unit = {
    val sign: Int = getSign
    if (sign == 0) {
      return
    }
    calendar.add(Calendar.YEAR, getYears * sign)
    calendar.add(Calendar.MONTH, getMonths * sign)
    calendar.add(Calendar.DAY_OF_MONTH, getDays * sign)
    calendar.add(Calendar.HOUR_OF_DAY, getHours * sign)
    calendar.add(Calendar.MINUTE, getMinutes * sign)
    calendar.add(Calendar.SECOND,
      duration
        .getComponent(AccessorFn.Component.WHOLE_SECONDS)
        .asInstanceOf[Int64Value]
        .longValue
        .toInt *
        sign)
    calendar.add(Calendar.MILLISECOND,
      duration
        .getComponent(AccessorFn.Component.MICROSECONDS)
        .asInstanceOf[Int64Value]
        .longValue
        .toInt *
        sign /
        1000)
  }

  def multiply(factor: BigDecimal): Duration =
    new SaxonDuration(duration.multiply(factor.doubleValue()))

  def negate(): Duration = new SaxonDuration(duration.negate())

  def normalizeWith(startTimeInstant: Calendar): Duration =
    throw new UnsupportedOperationException

  def compare(rhs: Duration): Int = {
    if (! rhs.isInstanceOf[SaxonDuration]) {
      throw new IllegalArgumentException(
        "Supplied duration is not a SaxonDuration")
    }
    val c0: Comparable[AnyRef] = duration.getSchemaComparable
    val c1: Comparable[AnyRef] =
      rhs.asInstanceOf[SaxonDuration].duration.getSchemaComparable.asInstanceOf
    c0.compareTo(c1.asInstanceOf)
  }

  override def hashCode: Int = duration.hashCode

}