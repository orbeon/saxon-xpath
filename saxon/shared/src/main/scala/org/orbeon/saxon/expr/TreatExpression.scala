package org.orbeon.saxon.expr

import org.orbeon.saxon.expr.parser.RoleDiagnostic

import org.orbeon.saxon.value.SequenceType

object TreatExpression {

  def make(sequence: Expression, `type`: SequenceType): Expression =
    make(sequence, `type`, "XPDY0050")

  def make(sequence: Expression,
           `type`: SequenceType,
           errorCode: String): Expression = {
    val role: RoleDiagnostic =
      new RoleDiagnostic(RoleDiagnostic.TYPE_OP, "treat as", 0)
    role.setErrorCode(errorCode)
    val e: Expression = CardinalityChecker.makeCardinalityChecker(
      sequence,
      `type`.getCardinality,
      role)
    new ItemChecker(e, `type`.getPrimaryType, role)
  }

}
