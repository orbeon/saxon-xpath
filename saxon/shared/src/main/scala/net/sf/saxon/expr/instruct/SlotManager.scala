package net.sf.saxon.expr.instruct

import net.sf.saxon.expr.XPathContext

import net.sf.saxon.lib.Logger

import net.sf.saxon.om.StructuredQName

import java.util.ArrayList

import scala.beans.{BeanProperty, BooleanBeanProperty}

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

  def showStackFrame(context: XPathContext, logger: Logger): Unit = {}
  def getNumberOfVariables: Int = numberOfVariables

}
