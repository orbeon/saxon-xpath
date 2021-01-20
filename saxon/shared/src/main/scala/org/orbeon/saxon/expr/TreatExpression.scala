package org.orbeon.saxon.expr

import org.orbeon.saxon.expr.parser.RoleDiagnostic
import org.orbeon.saxon.value.SequenceType


object TreatExpression {

  def make(
    sequence  : Expression,
    `type`    : SequenceType,
    errorCode : String = "XPDY0050"
  ): Expression = {
    val role = new RoleDiagnostic(RoleDiagnostic.TYPE_OP, "treat as", 0)
    role.setErrorCode(errorCode)
    val e = CardinalityChecker.makeCardinalityChecker(
      sequence,
      `type`.getCardinality,
      role
    )
    new ItemChecker(e, `type`.getPrimaryType, role)
  }
}
