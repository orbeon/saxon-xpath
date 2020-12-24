package org.orbeon.saxon.expr.instruct

import org.orbeon.saxon.expr.XPathContext
import org.orbeon.saxon.lib.Logger
import org.orbeon.saxon.om.StructuredQName

import java.util.ArrayList
import scala.beans.BeanProperty

object SlotManager {
  var EMPTY: SlotManager = new SlotManager(0)
}

class SlotManager {

  @BeanProperty
  var variableMap: ArrayList[StructuredQName] = new ArrayList()

  var numberOfVariables: Int = 0

  def this(n: Int) = {
    this()
    numberOfVariables = n
    variableMap = new ArrayList(n)
  }

  def setNumberOfVariables(numberOfVariables: Int): Unit = {
    this.numberOfVariables = numberOfVariables
    variableMap.trimToSize()
  }

  def allocateSlotNumber(qName: StructuredQName): Int = {
    variableMap.add(qName)
    numberOfVariables += 1
    numberOfVariables
  }

  def showStackFrame(context: XPathContext, logger: Logger): Unit = ()
  def getNumberOfVariables: Int = numberOfVariables

}
