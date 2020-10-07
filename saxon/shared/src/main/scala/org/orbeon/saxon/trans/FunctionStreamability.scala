package org.orbeon.saxon.trans


object FunctionStreamability extends Enumeration {

  val UNCLASSIFIED: FunctionStreamability = new FunctionStreamability(
    "unclassified")

  val ABSORBING: FunctionStreamability = new FunctionStreamability("absorbing")

  val INSPECTION: FunctionStreamability = new FunctionStreamability(
    "inspection")

  val FILTER: FunctionStreamability = new FunctionStreamability("filter")

  val SHALLOW_DESCENT: FunctionStreamability = new FunctionStreamability(
    "shallow-descent")

  val DEEP_DESCENT: FunctionStreamability = new FunctionStreamability(
    "deep-descent")

  val ASCENT: FunctionStreamability = new FunctionStreamability("ascent")

  class FunctionStreamability(var streamabilityStr: String) extends Val {

    def isConsuming: Boolean =
      this == ABSORBING || this == SHALLOW_DESCENT || this == DEEP_DESCENT

    def isStreaming: Boolean = this != UNCLASSIFIED

  }

  def of(v: String): FunctionStreamability = v match {
    case "unclassified" => UNCLASSIFIED
    case "absorbing" => ABSORBING
    case "inspection" => INSPECTION
    case "filter" => FILTER
    case "shallow-descent" => SHALLOW_DESCENT
    case "deep-descent" => DEEP_DESCENT
    case "ascent" => ASCENT
    case _ => UNCLASSIFIED

  }

  implicit def convertValue(v: Value): FunctionStreamability =
    v.asInstanceOf[FunctionStreamability]

}