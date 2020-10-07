package org.orbeon.saxon.s9api

import org.orbeon.saxon.lib.Validation

import scala.beans.{BeanProperty, BooleanBeanProperty}

object ValidationMode extends Enumeration {

  val STRICT: ValidationMode = new ValidationMode(Validation.STRICT)

  val LAX: ValidationMode = new ValidationMode(Validation.LAX)

  val PRESERVE: ValidationMode = new ValidationMode(Validation.PRESERVE)

  val STRIP: ValidationMode = new ValidationMode(Validation.STRIP)

  val DEFAULT: ValidationMode = new ValidationMode(Validation.DEFAULT)

  class ValidationMode(@BeanProperty  var number: Int)
    extends Val

   def get(number: Int): ValidationMode = number match {
    case Validation.STRICT => STRICT
    case Validation.LAX => LAX
    case Validation.STRIP => STRIP
    case Validation.PRESERVE => PRESERVE
    case Validation.DEFAULT => null
    case _ => DEFAULT

  }

  implicit def convertValue(v: Value): ValidationMode =
    v.asInstanceOf[ValidationMode]

}
