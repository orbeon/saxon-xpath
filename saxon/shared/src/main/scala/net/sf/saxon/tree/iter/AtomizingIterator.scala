package net.sf.saxon.tree.iter

import net.sf.saxon.expr.parser.RoleDiagnostic

import net.sf.saxon.om.AtomicSequence

import net.sf.saxon.om.Item

import net.sf.saxon.om.SequenceIterator

import net.sf.saxon.trans.XPathException

import net.sf.saxon.value.AtomicValue


class AtomizingIterator(private var base: SequenceIterator)
  extends SequenceIterator {

  private var currentValue: AtomicSequence = null

  private var currentValuePosition: Int = 1

  private var currentValueSize: Int = 1

  private var roleDiagnostic: RoleDiagnostic = _

  def setRoleDiagnostic(role: RoleDiagnostic): Unit = {
    this.roleDiagnostic = role
  }

  def next(): AtomicValue = {
    while (true) {
      if (currentValue != null) {
        if (currentValuePosition < currentValueSize) {
          currentValue.itemAt({
            currentValuePosition += 1;
            currentValuePosition - 1
          })
        } else {
          currentValue = null
        }
      }
      val nextSource: Item = base.next()
      if (nextSource != null) {
        try {
          val v: AtomicSequence = nextSource.atomize()
          if (v.isInstanceOf[AtomicValue]) {
            v.asInstanceOf[AtomicValue]
          } else {
            currentValue = v
            currentValuePosition = 0
            currentValueSize = currentValue.getLength
          }
        } catch {
          case e: XPathException =>
            if (roleDiagnostic == null) {
              throw e
            } else {
              val message: String = e.getMessage + ". Failed while atomizing the " + roleDiagnostic.getMessage
              throw new XPathException(message,
                e.getErrorCodeLocalPart,
                e.getLocator)
            }

        }
      } else {
        currentValue = null
        null
      }
    }
    null
  }


  override def close(): Unit = {
    base.close()
  }

}