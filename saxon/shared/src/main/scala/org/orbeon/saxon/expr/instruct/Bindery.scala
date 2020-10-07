package org.orbeon.saxon.expr.instruct

import org.orbeon.saxon.expr.PackageData

import org.orbeon.saxon.om.GroundedValue

import org.orbeon.saxon.om.Sequence

import org.orbeon.saxon.trans.XPathException

import org.orbeon.saxon.value.ObjectValue

object Bindery {

  class FailureValue(err: XPathException)
    extends ObjectValue[XPathException](err)

}

class Bindery(pack: PackageData) {

  private var globals: Array[GroundedValue] = _

  private var busy: Array[Long] = _

  allocateGlobals(pack.getGlobalSlotManager)

  private def allocateGlobals(map: SlotManager): Unit = {
    val n: Int = map.getNumberOfVariables + 1
    globals = Array.ofDim[GroundedValue](n)
    busy = Array.ofDim[Long](n)
    for (i <- 0 until n) {
      globals(i) = null
      busy(i) = -1L
    }
  }

  def setGlobalVariable(binding: GlobalVariable, value: GroundedValue): Unit = {
    globals(binding.getBinderySlotNumber) = value
  }

  def setExecuting(binding: GlobalVariable): Boolean = {
    val thisThread: Long = Thread.currentThread().getId
    val slot: Int = binding.getBinderySlotNumber
    val busyThread: Long = busy(slot)
    if (busyThread != -1L) {
      if (busyThread == thisThread) {
        throw new XPathException.Circularity(
          "Circular definition of variable " + binding.getVariableQName.getDisplayName)
      } else {
        for (i <- 0.until(10)) {
          try Thread.sleep(20 * i)
          catch {
            case e: InterruptedException => {}

          }
          if (busy(slot) == -1L) {
            false
          }
        }
        return true
      }
    }
    busy(slot) = thisThread
    true
  }

  def setNotExecuting(binding: GlobalVariable): Unit = {
    val slot: Int = binding.getBinderySlotNumber
    busy(slot) = -1L
  }

  def saveGlobalVariableValue(binding: GlobalVariable,
                              value: GroundedValue): GroundedValue =
    synchronized {
      val slot: Int = binding.getBinderySlotNumber
      if (globals(slot) != null) {
        globals(slot)
      } else {
        busy(slot) = -1L
        globals(slot) = value
        value
      }
    }

  def getGlobalVariableValue(binding: GlobalVariable): GroundedValue =
    globals(binding.getBinderySlotNumber)

  def getGlobalVariable(slot: Int): GroundedValue = globals(slot)

  def getGlobalVariables: Array[GroundedValue] = globals

}
