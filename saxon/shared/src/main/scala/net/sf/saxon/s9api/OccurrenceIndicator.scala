package net.sf.saxon.s9api

import net.sf.saxon.expr.StaticProperty

import net.sf.saxon.value.Cardinality


object OccurrenceIndicator extends Enumeration {

  val ZERO: OccurrenceIndicator = new OccurrenceIndicator()

  val ZERO_OR_ONE: OccurrenceIndicator = new OccurrenceIndicator()

  val ZERO_OR_MORE: OccurrenceIndicator = new OccurrenceIndicator()

  val ONE: OccurrenceIndicator = new OccurrenceIndicator()

  val ONE_OR_MORE: OccurrenceIndicator = new OccurrenceIndicator()

  class OccurrenceIndicator extends Val {

     def getCardinality(): Int = this match {
      case ZERO => StaticProperty.EMPTY
      case ZERO_OR_ONE => StaticProperty.ALLOWS_ZERO_OR_ONE
      case ZERO_OR_MORE => StaticProperty.ALLOWS_ZERO_OR_MORE
      case ONE => StaticProperty.ALLOWS_ONE
      case ONE_OR_MORE => StaticProperty.ALLOWS_ONE_OR_MORE
      case _ => StaticProperty.EMPTY

    }

    def allowsZero(): Boolean = Cardinality.allowsZero(getCardinality)

    def allowsMany(): Boolean = Cardinality.allowsMany(getCardinality)

    def subsumes(other: OccurrenceIndicator): Boolean =
      Cardinality.subsumes(getCardinality, other.getCardinality)

    override def toString(): String = this match {
      case ZERO => "0"
      case ZERO_OR_ONE => "?"
      case ZERO_OR_MORE => "*"
      case ONE => ""
      case ONE_OR_MORE => "+"
      case _ => "!!!"

    }

  }

   def getOccurrenceIndicator(cardinality: Int): OccurrenceIndicator =
    cardinality match {
      case StaticProperty.EMPTY => ZERO
      case StaticProperty.ALLOWS_ZERO_OR_ONE => ZERO_OR_ONE
      case StaticProperty.ALLOWS_ZERO_OR_MORE => ZERO_OR_MORE
      case StaticProperty.ALLOWS_ONE => ONE
      case StaticProperty.ALLOWS_ONE_OR_MORE => ONE_OR_MORE
      case _ => ZERO_OR_MORE

    }

  implicit def convertValue(v: Value): OccurrenceIndicator =
    v.asInstanceOf[OccurrenceIndicator]

}
