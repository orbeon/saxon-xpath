package org.orbeon.saxon.sxpath

import org.orbeon.saxon.expr.{Expression, StaticContext, XPathContextMajor}
import org.orbeon.saxon.expr.instruct.{Executable, SlotManager}
import org.orbeon.saxon.model.ItemType
import org.orbeon.saxon.om.{Item, SequenceIterator}
import org.orbeon.saxon.trans.XPathException
import org.orbeon.saxon.utils.Controller

import java.util.{ArrayList, List}
import scala.beans.BeanProperty


class XPathExpression(
  private var env        : StaticContext,
  private var expression : Expression,
  exec                   : Executable
) {

  private var stackFrameMap: SlotManager = _

  @BeanProperty
  var executable: Executable = exec

  private var numberOfExternalVariables: Int = _

  def setStackFrameMap(map: SlotManager, numberOfExternalVariables: Int): Unit = {
    stackFrameMap = map
    this.numberOfExternalVariables = numberOfExternalVariables
  }

  def createDynamicContext: XPathDynamicContext = {
    val context = new XPathContextMajor(null, executable)
    context.openStackFrame(stackFrameMap)
    new XPathDynamicContext(env.getRequiredContextItemType,
      context,
      stackFrameMap)
  }

  def createDynamicContext(contextItem: Item): XPathDynamicContext = {
    checkContextItemType(contextItem)
    val context = new XPathContextMajor(contextItem, executable)
    context.openStackFrame(stackFrameMap)
    new XPathDynamicContext(
      env.getRequiredContextItemType,
      context,
      stackFrameMap
    )
  }

  def createDynamicContext(controller: Controller,
                           contextItem: Item): XPathDynamicContext = {
    checkContextItemType(contextItem)
    if (controller == null) {
      createDynamicContext(contextItem)
    } else {
      val context = controller.newXPathContext
      context.openStackFrame(stackFrameMap)
      val dc = new XPathDynamicContext(
        env.getRequiredContextItemType,
        context,
        stackFrameMap
      )
      if (contextItem != null)
        dc.setContextItem(contextItem)
      dc
    }
  }

  private def checkContextItemType(contextItem: Item): Unit =
    if (contextItem != null) {
      val `type` = env.getRequiredContextItemType
      val th     = env.getConfiguration.getTypeHierarchy
      if (! `type`.matches(contextItem, th))
        throw new XPathException(
          "Supplied context item does not match required context item type " +
            `type`
        )
    }

  def iterate(context: XPathDynamicContext): SequenceIterator = {
    context.checkExternalVariables(stackFrameMap, numberOfExternalVariables)
    expression.iterate(context.getXPathContextObject)
  }

  def evaluate(context: XPathDynamicContext): List[Item] = {
    val list: List[Item] = new ArrayList[Item](20)
    expression.iterate(context.getXPathContextObject).forEachOrFail((res: Item) => list.add(res))
    list
  }

  def evaluateSingle(context: XPathDynamicContext): Item = {
    val iter = expression.iterate(context.getXPathContextObject)
    val result = iter.next()
    iter.close()
    result
  }

  def effectiveBooleanValue(context: XPathDynamicContext): Boolean =
    expression.effectiveBooleanValue(context.getXPathContextObject)

  def getInternalExpression: Expression = expression
}
