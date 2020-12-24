package org.orbeon.saxon.expr

import java.util
import java.util.Iterator

import javax.xml.transform.URIResolver
import org.orbeon.saxon.expr.XPathContextMinor._
import org.orbeon.saxon.expr.instruct.ParameterSet
import org.orbeon.saxon.expr.sort.GroupIterator
import org.orbeon.saxon.lib.ErrorReporter
import org.orbeon.saxon.om._
import org.orbeon.saxon.regex.RegexIterator
import org.orbeon.saxon.trace.ContextStackIterator
import org.orbeon.saxon.trans.XPathException
import org.orbeon.saxon.trans.rules.Rule
import org.orbeon.saxon.tree.iter.LookaheadIterator
import org.orbeon.saxon.utils.{Configuration, Controller}
import org.orbeon.saxon.value.DateTimeValue


/**
 * This class represents a minor change in the dynamic context in which an XPath expression is evaluated:
 * a "major context" object allows all aspects of the dynamic context to change, whereas
 * a "minor context" only allows changes to the focus and the destination for push output.
 */
object XPathContextMinor {
  class LastValue(var value: Int)
}

class XPathContextMinor () extends XPathContext {

  var controller          : Controller    = _
  var currentIterator     : FocusIterator = _
  var last                : LastValue     = null
  var caller              : XPathContext  = null
  var stackFrame          : StackFrame    = _
  var currentDestination  : String        = ""
  var temporaryOutputState: Int           = 0

  def newContext(): XPathContextMajor = XPathContextMajor.newContext(this)

  def newMinorContext(): XPathContextMinor = {
    val c = new XPathContextMinor
    c.controller           = controller
    c.caller               = this
    c.currentIterator      = currentIterator
    c.last                 = last
    c.stackFrame           = stackFrame
    c.currentDestination   = currentDestination
    c.temporaryOutputState = temporaryOutputState
    c
  }

  def setCaller(caller: XPathContext): Unit =
    this.caller = caller

  def newCleanContext(): XPathContextMajor = {
    val c = new XPathContextMajor(getController)
    c.setCaller(this)
    c
  }

  def getLocalParameters : ParameterSet  = getCaller.getLocalParameters
  def getTunnelParameters: ParameterSet  = getCaller.getTunnelParameters
  def getController      : Controller    = controller
  def getConfiguration   : Configuration = controller.getConfiguration
  def getNamePool        : NamePool      = controller.getConfiguration.getNamePool
  def getCaller          : XPathContext  = caller

  def setCurrentIterator(iter: FocusIterator): Unit = {
    currentIterator = iter
    last = new LastValue(-1)
  }

  def trackFocus(iter: SequenceIterator): FocusIterator = {
    val factory = controller.getFocusTrackerFactory(false)
    val fit = factory(iter)
    setCurrentIterator(fit)
    fit
  }

  def trackFocusMultithreaded(iter: SequenceIterator): FocusIterator = {
    val factory = controller.getFocusTrackerFactory(true)
    val fit = factory(iter)
    setCurrentIterator(fit)
    fit
  }

  def getCurrentIterator: FocusIterator = currentIterator

  def getContextItem: Item =
    if (currentIterator == null)
      null
    else
      currentIterator.current

  def getLast: Int = {
    if (currentIterator == null) {
      val e = new XPathException("The context item is absent, so last() is undefined")
      e.setXPathContext(this)
      e.setErrorCode("XPDY0002")
      throw e
    }
    if (last.value >= 0)
      last.value
    else
      currentIterator.getLength
  }

  def isAtLast: Boolean =
    if (currentIterator.getProperties.contains(SequenceIterator.Property.LOOKAHEAD)) {
      ! currentIterator.asInstanceOf[LookaheadIterator].hasNext
    } else
      currentIterator.position == getLast

  def getURIResolver     : URIResolver                     = caller.getURIResolver
  def getErrorReporter   : ErrorReporter                   = caller.getErrorReporter
  def getCurrentException: XPathException                  = caller.getCurrentException
  def getThreadManager   : XPathContextMajor.ThreadManager = caller.getThreadManager
  def getCurrentComponent: Component                       = caller.getCurrentComponent
  def getStackFrame      : StackFrame                      = stackFrame

  def makeStackFrameMutable(): Unit =
    if (stackFrame == StackFrame.EMPTY)
      stackFrame = new StackFrame(null, SequenceTool.makeSequenceArray(0))

  def evaluateLocalVariable(slotnumber: Int): Sequence =
    stackFrame.slots(slotnumber) // need changes in StackFrame class

  def setLocalVariable(slotNumber: Int, value: Sequence): Unit = {
    try
      stackFrame.slots(slotNumber) = value.makeRepeatable()
    catch {
      case _: ArrayIndexOutOfBoundsException =>
        if (slotNumber == -999)
          throw new AssertionError("Internal error: Cannot set local variable: no slot allocated")
        else
          throw new AssertionError(
            "Internal error: Cannot set local variable (slot " + slotNumber +
              " of " +
              getStackFrame.getStackFrameValues.length +
              ")"
          )
    }
  }

  def waitForChildThreads(): Unit =
    synchronized {
      getCaller.waitForChildThreads()
    }

  def setTemporaryOutputState(temporary: Int): Unit =
    temporaryOutputState = temporary

  def getTemporaryOutputState: Int = temporaryOutputState

  def setCurrentOutputUri(uri: String): Unit =
    currentDestination = uri

  def getCurrentOutputUri: String = currentDestination

  def useLocalParameter(parameterId: StructuredQName,
                        slotNumber: Int,
                        isTunnel: Boolean): Int =
    getCaller.useLocalParameter(parameterId, slotNumber, isTunnel)

  def getCurrentMode: Component.M = getCaller.getCurrentMode

  def getCurrentTemplateRule: Rule = null

  def getCurrentGroupIterator: GroupIterator =
    getCaller.getCurrentGroupIterator

  def getCurrentMergeGroupIterator: GroupIterator =
    getCaller.getCurrentMergeGroupIterator

  def getCurrentRegexIterator: RegexIterator =
    getCaller.getCurrentRegexIterator

  def getCurrentDateTime: DateTimeValue = controller.getCurrentDateTime

  def getImplicitTimezone: Int = controller.getImplicitTimezone

  def iterateStackFrames: util.Iterator[AnyRef] = new ContextStackIterator(this).asInstanceOf[Iterator[AnyRef]]

  def getTargetComponent(bindingSlot: Int): Component =
    getCaller.getTargetComponent(bindingSlot)
}
