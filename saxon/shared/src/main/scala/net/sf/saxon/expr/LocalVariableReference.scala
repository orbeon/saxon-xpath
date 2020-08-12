package net.sf.saxon.expr

import net.sf.saxon.expr.parser.RebindingMap

import net.sf.saxon.om.Sequence

import net.sf.saxon.om.StructuredQName

class LocalVariableReference(name: StructuredQName)
  extends VariableReference(name) {

  var slotNumber: Int = -999

  def this(binding: LocalBinding) = {
    this(binding.getVariableQName())

  }


  def copy(rebindings: RebindingMap): Expression = {
    if (binding == null) {
      throw new UnsupportedOperationException(
        "Cannot copy a variable reference whose binding is unknown")
    }
    val ref: LocalVariableReference = new LocalVariableReference(
      getVariableName)
    ref.copyFrom(this)
    ref.slotNumber = slotNumber
    val newBinding: Binding = rebindings.get(binding)
    if (newBinding != null) {
      ref.binding = newBinding
    }
    ref.binding.addReference(ref, isInLoop)
    ref
  }

  def setBinding(binding: LocalBinding): Unit = {
    this.binding = binding
  }

  override def getBinding(): LocalBinding =
    super.getBinding.asInstanceOf[LocalBinding]

  def setSlotNumber(slotNumber: Int): Unit = {
    this.slotNumber = slotNumber
  }

  def getSlotNumber(): Int = slotNumber

  override def evaluateVariable(c: XPathContext): Sequence =
    try c.getStackFrame().getStackFrameValues()(slotNumber)
    catch {
      case err: ArrayIndexOutOfBoundsException =>
        if (slotNumber == -999) {
          if (binding != null) {
            try {
              slotNumber = getBinding.getLocalSlotNumber
              c.getStackFrame().getStackFrameValues()(slotNumber)
            } catch {
              case err2: ArrayIndexOutOfBoundsException => {}

            }
          }
          throw new ArrayIndexOutOfBoundsException(
            "Local variable $" + getDisplayName + " has not been allocated a stack frame slot")
        } else {
          val actual: Int = c.getStackFrame.getStackFrameValues.length
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

  override def getExpressionName(): String = "locVarRef"

}
