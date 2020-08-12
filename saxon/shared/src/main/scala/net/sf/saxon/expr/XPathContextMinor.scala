package net.sf.saxon.expr

import net.sf.saxon.utils.Configuration
import net.sf.saxon.utils.Controller
import net.sf.saxon.expr.instruct.ParameterSet
import net.sf.saxon.expr.sort.GroupIterator
import net.sf.saxon.lib.ErrorReporter
import net.sf.saxon.om._
import net.sf.saxon.regex.RegexIterator
import net.sf.saxon.trace.{ContextStackFrame, ContextStackIterator}
import net.sf.saxon.trans.XPathException
import net.sf.saxon.trans.rules.Rule
import net.sf.saxon.tree.iter.LookaheadIterator
import net.sf.saxon.value.DateTimeValue
import javax.xml.transform.URIResolver
import java.util.Iterator
import java.util.function.Function

import XPathContextMinor._

object XPathContextMinor {

  class LastValue(var value: Int)

}

class XPathContextMinor () extends XPathContext {

  var controller: Controller = _

  var currentIterator: FocusIterator = _

  var last: LastValue = null

  var caller: XPathContext = null

  var stackFrame: StackFrame = _

  var currentDestination: String = ""

  var temporaryOutputState: Int = 0

  def newContext(): XPathContextMajor = XPathContextMajor.newContext(this)

  def newMinorContext(): XPathContextMinor = {
    val c: XPathContextMinor = new XPathContextMinor()
    c.controller = controller
    c.caller = this
    c.currentIterator = currentIterator
    c.last = last
    c.stackFrame = stackFrame
    c.currentDestination = currentDestination
    c.temporaryOutputState = temporaryOutputState
    c
  }

  def setCaller(caller: XPathContext): Unit = {
    this.caller = caller
  }

  def newCleanContext(): XPathContextMajor = {
    val c: XPathContextMajor = new XPathContextMajor(getController)
    c.setCaller(this)
    c
  }

  def getLocalParameters(): ParameterSet = getCaller.getLocalParameters

  def getTunnelParameters(): ParameterSet = getCaller.getTunnelParameters

  def getController(): Controller = controller

  def getConfiguration(): Configuration = controller.getConfiguration

  def getNamePool(): NamePool = controller.getConfiguration.getNamePool

  def getCaller(): XPathContext = caller

  def setCurrentIterator(iter: FocusIterator): Unit = {
    currentIterator = iter
    last = new LastValue(-1)
  }

  def trackFocus(iter: SequenceIterator): FocusIterator = {
    val factory: Function[SequenceIterator, FocusTrackingIterator] =
      controller.getFocusTrackerFactory(false).asInstanceOf[Function[SequenceIterator, FocusTrackingIterator]]
    val fit: FocusIterator = factory.apply(iter)
    this.currentIterator = fit
    fit
  }

  def trackFocusMultithreaded(iter: SequenceIterator): FocusIterator = {
    val factory: Function[SequenceIterator, FocusTrackingIterator] =
      controller.getFocusTrackerFactory(true).asInstanceOf[Function[SequenceIterator, FocusTrackingIterator]]
    val fit: FocusIterator = factory.apply(iter)
    this.currentIterator = fit
    fit
  }

  def getCurrentIterator(): FocusIterator = currentIterator

  def getContextItem(): Item = {
    if (currentIterator == null) {
      null
    }
    currentIterator.current()
  }

  def getLast(): Int = {
    if (currentIterator == null) {
      val e: XPathException = new XPathException(
        "The context item is absent, so last() is undefined")
      e.setXPathContext(this)
      e.setErrorCode("XPDY0002")
      throw e
    }
    if (last.value >= 0) {
      last.value
    }
    currentIterator.getLength
  }

  def isAtLast(): Boolean = {
    if (currentIterator.getProperties.contains(
      SequenceIterator.Property.LOOKAHEAD)) {
      !currentIterator.asInstanceOf[LookaheadIterator].hasNext
    }
    currentIterator.position() == getLast
  }

  def getURIResolver(): URIResolver = caller.getURIResolver

  def getErrorReporter(): ErrorReporter = caller.getErrorReporter

  def getCurrentException(): XPathException = caller.getCurrentException

  def getThreadManager(): XPathContextMajor.ThreadManager = caller.getThreadManager.asInstanceOf[XPathContextMajor.ThreadManager]

  def getCurrentComponent(): Component = caller.getCurrentComponent

  def getStackFrame(): StackFrame = stackFrame

  def makeStackFrameMutable(): Unit = {
    if (stackFrame == StackFrame.EMPTY) {
      stackFrame = new StackFrame(null, SequenceTool.makeSequenceArray(0))
    }
  }

  def evaluateLocalVariable(slotnumber: Int): Sequence =
    stackFrame.slots(slotnumber) // need changes in StackFrame class

  def setLocalVariable(slotNumber: Int, value: Sequence): Unit = {
    var sequence = value
    sequence = sequence.makeRepeatable()
    try stackFrame.slots(slotNumber) = sequence
    catch {
      case e: ArrayIndexOutOfBoundsException =>
        if (slotNumber == -999) {
          throw new AssertionError(
            "Internal error: Cannot set local variable: no slot allocated")
        } else {
          throw new AssertionError(
            "Internal error: Cannot set local variable (slot " + slotNumber +
              " of " +
              getStackFrame.getStackFrameValues.length +
              ")")
        }

    }
  }

  def waitForChildThreads(): Unit = {
    synchronized {
      getCaller.waitForChildThreads()
    }
  }

  def setTemporaryOutputState(temporary: Int): Unit = {
    temporaryOutputState = temporary
  }

  def getTemporaryOutputState(): Int = temporaryOutputState

  def setCurrentOutputUri(uri: String): Unit = {
    currentDestination = uri
  }

  def getCurrentOutputUri(): String = currentDestination

  def useLocalParameter(parameterId: StructuredQName,
                        slotNumber: Int,
                        isTunnel: Boolean): Int =
    getCaller.useLocalParameter(parameterId, slotNumber, isTunnel)

  def getCurrentMode(): Component.M = getCaller.getCurrentMode

  def getCurrentTemplateRule(): Rule = null

  def getCurrentGroupIterator(): GroupIterator =
    getCaller.getCurrentGroupIterator

  def getCurrentMergeGroupIterator(): GroupIterator =
    getCaller.getCurrentMergeGroupIterator

  def getCurrentRegexIterator(): RegexIterator =
    getCaller.getCurrentRegexIterator

  def getCurrentDateTime(): DateTimeValue = controller.getCurrentDateTime

  def getImplicitTimezone(): Int = controller.getImplicitTimezone.asInstanceOf[Int]

  def iterateStackFrames() = new ContextStackIterator(this).asInstanceOf[Iterator[AnyRef]]

  def getTargetComponent(bindingSlot: Int): Component =
    getCaller.getTargetComponent(bindingSlot)

}
