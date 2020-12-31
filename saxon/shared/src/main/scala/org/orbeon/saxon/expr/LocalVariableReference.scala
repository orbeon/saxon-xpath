package org.orbeon.saxon.expr

import org.orbeon.saxon.expr.parser.RebindingMap
import org.orbeon.saxon.om.{Sequence, StructuredQName}

class LocalVariableReference private (qnameOrBinding: StructuredQName Either LocalBinding)
  extends VariableReference(qnameOrBinding) {

  def this(name: StructuredQName) = this(Left(name))
  def this(binding: LocalBinding) = this(Right(binding))

  var slotNumber: Int = -999

  def copy(rebindings: RebindingMap): Expression = {

    if (binding == null)
      throw new UnsupportedOperationException("Cannot copy a variable reference whose binding is unknown")

    val ref = new LocalVariableReference(Left(getVariableName))
    ref.copyFrom(this)
    ref.slotNumber = slotNumber
    val newBinding = rebindings.get(binding)
    if (newBinding != null)
      ref.binding = newBinding
    ref.binding.addReference(ref, isInLoop)
    ref
  }

  def setBinding(binding: LocalBinding): Unit =
    this.binding = binding

  override def getBinding: LocalBinding =
    super.getBinding.asInstanceOf[LocalBinding]

  def setSlotNumber(slotNumber: Int): Unit =
    this.slotNumber = slotNumber

  def getSlotNumber: Int = slotNumber

  override def evaluateVariable(c: XPathContext): Sequence =
    try
      c.getStackFrame.getStackFrameValues(slotNumber)
    catch {
      case _: ArrayIndexOutOfBoundsException =>
        if (slotNumber == -999) {
          if (binding != null) {
            try {
              slotNumber = getBinding.getLocalSlotNumber
              return c.getStackFrame.getStackFrameValues(slotNumber)
            } catch {
              case _: ArrayIndexOutOfBoundsException =>
            }
          }
          throw new ArrayIndexOutOfBoundsException("Local variable $" + getDisplayName + " has not been allocated a stack frame slot")
        } else {
          val actual = c.getStackFrame.getStackFrameValues.length
          throw new ArrayIndexOutOfBoundsException(
            "Local variable $" + getDisplayName + " uses slot " +
              slotNumber +
              " but " +
              (if (actual == 0) "no"
              else "only " + c.getStackFrame.getStackFrameValues.length) +
              " slots" +
              " are allocated on the stack frame")
        }

    }

  override def getExpressionName: String = "locVarRef"
}
