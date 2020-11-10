////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// Copyright (c) 2018-2020 Saxonica Limited
// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is "Incompatible With Secondary Licenses", as defined by the Mozilla Public License, v. 2.0.
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
package org.orbeon.saxon.expr

import java.{util => ju}

import javax.xml.transform.URIResolver
import org.orbeon.saxon.expr.instruct.ParameterSet
import org.orbeon.saxon.expr.sort.GroupIterator
import org.orbeon.saxon.lib.ErrorReporter
import org.orbeon.saxon.om._
import org.orbeon.saxon.regex.RegexIterator
import org.orbeon.saxon.trans.rules.Rule
import org.orbeon.saxon.trans.{NoDynamicContextException, XPathException}
import org.orbeon.saxon.utils.{Configuration, Controller}
import org.orbeon.saxon.value.{CalendarValue, DateTimeValue}


/**
  * This class is an implementation of XPathContext used when evaluating constant sub-expressions at
  * compile time.
  */
class EarlyEvaluationContext(private var config: Configuration)
    extends XPathContext {

  def evaluateLocalVariable(slotnumber: Int): Sequence = notAllowed()

  def getCaller: XPathContext = null

  /**
    * Get the URI resolver. This gets the local URIResolver set in the XPathContext if there
    * is one; if not, it gets the URIResolver from the Controller (which itself defaults to the
    * one set in the Configuration).
    *
    * @return the user-supplied URI resolver if there is one, or null otherwise.
    * @since 9.6
    */
  def getURIResolver: URIResolver = config.getURIResolver

  /**
    * Get the error listener. If no ErrorListener
    * has been set locally, the ErrorListener in the Controller is returned; this in turn defaults
    * to the ErrorListener set in the Configuration.
    *
    * @return the ErrorListener in use. This will always be an UnfailingErrorListener,
    *         which is a Saxon subclass of ErrorListener that throws no exceptions.
    * @since 9.6
    */
  def getErrorReporter: ErrorReporter = config.makeErrorReporter

  def getCurrentComponent          : Component             = notAllowed()
  def getConfiguration             : Configuration         = config
  def getContextItem               : Item                  = null
  def getController                : Controller            = null
  def getCurrentGroupIterator      : GroupIterator         = notAllowed()
  def getCurrentMergeGroupIterator : GroupIterator         = notAllowed()
  def getCurrentIterator           : FocusTrackingIterator = null
  def getCurrentMode               : Component.M           = notAllowed()
  def getCurrentRegexIterator      : RegexIterator         = null
  def getCurrentTemplateRule       : Rule                  = null

  def getLast: Int = {
    val err = new XPathException("The context item is absent")
    err.setErrorCode("XPDY0002")
    throw err
  }

  def getLocalParameters  : ParameterSet = notAllowed()
  def getNamePool         : NamePool     = config.getNamePool
  def getStackFrame       : StackFrame   = notAllowed()
  def getTunnelParameters : ParameterSet = notAllowed()

  def isAtLast: Boolean = {
    val err = new XPathException("The context item is absent")
    err.setErrorCode("XPDY0002")
    throw err
  }

  def newCleanContext(): XPathContextMajor = notAllowed()

  def newContext(): XPathContextMajor = {
    val controller: Controller = new Controller(config)
    controller.newXPathContext
  }

  def newMinorContext(): XPathContextMinor = newContext().newMinorContext()

  def setCaller(caller: XPathContext): Unit = ()
  def setCurrentIterator(iter: FocusIterator): Unit =  notAllowed()
  def trackFocus(iter: SequenceIterator): FocusIterator =  notAllowed()
  def setLocalVariable(slotNumber: Int, value: Sequence): Unit =  notAllowed()

  def useLocalParameter(parameterId: StructuredQName,
                        slotNumber: Int,
                        isTunnel: Boolean): Int = ParameterSet.NOT_SUPPLIED

  def getCurrentDateTime: DateTimeValue =
    throw new NoDynamicContextException("current-dateTime")

  def getImplicitTimezone: Int =
    CalendarValue.MISSING_TIMEZONE

  def iterateStackFrames: ju.Iterator[AnyRef] =
    ju.Collections.EMPTY_LIST.iterator.asInstanceOf[ju.Iterator[AnyRef]]

  def getCurrentException: XPathException = null

  def waitForChildThreads(): Unit =
    getCaller.waitForChildThreads()

  def setTemporaryOutputState(temporary: Int): Unit = ()
  def getTemporaryOutputState: Int = 0
  def setCurrentOutputUri(uri: String): Unit = ()
  def getCurrentOutputUri: String = null
  def getThreadManager: XPathContextMajor.ThreadManager = null
  def getTargetComponent(bindingSlot: Int): Component = null

  private def notAllowed(): Nothing =
    throw new UnsupportedOperationException(
      new NoDynamicContextException("Internal error: early evaluation of subexpression with no context")
    )
}