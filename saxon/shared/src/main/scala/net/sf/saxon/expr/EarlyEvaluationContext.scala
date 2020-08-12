////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
package net.sf.saxon.expr

import net.sf.saxon.utils.Configuration

import net.sf.saxon.utils.Controller

import net.sf.saxon.expr.instruct.ParameterSet

import net.sf.saxon.expr.sort.GroupIterator

import net.sf.saxon.lib.ErrorReporter

import net.sf.saxon.om._

import net.sf.saxon.regex.RegexIterator

import net.sf.saxon.trans.NoDynamicContextException

import net.sf.saxon.trans.XPathException

import net.sf.saxon.trans.rules.Rule

import net.sf.saxon.value.CalendarValue

import net.sf.saxon.value.DateTimeValue

import javax.xml.transform.URIResolver

import java.util.Collections

import java.util.Iterator

import scala.collection.JavaConverters._




class EarlyEvaluationContext(private var config: Configuration)
    extends XPathContext {

  /*@Nullable*/

  def evaluateLocalVariable(slotnumber: Int): Sequence = {
    notAllowed()
    null
  }

  def getCaller(): XPathContext = null

  /**
    * Get the URI resolver. This gets the local URIResolver set in the XPathContext if there
    * is one; if not, it gets the URIResolver from the Controller (which itself defaults to the
    * one set in the Configuration).
    *
    * @return the user-supplied URI resolver if there is one, or null otherwise.
    * @since 9.6
    */
  def getURIResolver(): URIResolver = config.getURIResolver

  /**
    * Get the error listener. If no ErrorListener
    * has been set locally, the ErrorListener in the Controller is returned; this in turn defaults
    * to the ErrorListener set in the Configuration.
    *
    * @return the ErrorListener in use. This will always be an UnfailingErrorListener,
    *         which is a Saxon subclass of ErrorListener that throws no exceptions.
    * @since 9.6
    */
  def getErrorReporter(): ErrorReporter = config.makeErrorReporter

  /**
    * Get the current component
    */
  def getCurrentComponent(): Component = {
    notAllowed()
    null
  }

  def getConfiguration(): Configuration = config

  def getContextItem(): Item = null

  def getController(): Controller = null

  def getCurrentGroupIterator(): GroupIterator = {
    notAllowed()
    null
  }

  def getCurrentMergeGroupIterator(): GroupIterator = {
    notAllowed()
    null
  }

  def getCurrentIterator(): FocusTrackingIterator = null

  def getCurrentMode(): Component.M = {
    notAllowed()
    null
  }

  def getCurrentRegexIterator(): RegexIterator = null

  def getCurrentTemplateRule(): Rule = null

  def getLast(): Int = {
    val err: XPathException = new XPathException("The context item is absent")
    err.setErrorCode("XPDY0002")
    throw err
  }

  def getLocalParameters(): ParameterSet = {
    notAllowed()
    null
  }

  def getNamePool(): NamePool = config.getNamePool

  def getStackFrame(): StackFrame = {
    notAllowed()
    null
  }

  def getTunnelParameters(): ParameterSet = {
    notAllowed()
    null
  }

  def isAtLast(): Boolean = {
    val err: XPathException = new XPathException("The context item is absent")
    err.setErrorCode("XPDY0002")
    throw err
  }

  def newCleanContext(): XPathContextMajor = {
    notAllowed()
    null
  }

  def newContext(): XPathContextMajor = {
    val controller: Controller = new Controller(config)
    controller.newXPathContext
  }

  def newMinorContext(): XPathContextMinor = newContext().newMinorContext()

  def setCaller(caller: XPathContext): Unit = {}
// no-op
// no-op

  def setCurrentIterator(iter: FocusIterator): Unit = {
    notAllowed()
  }

  override def trackFocus(iter: SequenceIterator): FocusIterator = {
    notAllowed()
    null
  }

  def setLocalVariable(slotNumber: Int, value: Sequence): Unit = {
    notAllowed()
  }

  def useLocalParameter(parameterId: StructuredQName,
                        slotNumber: Int,
                        isTunnel: Boolean): Int = ParameterSet.NOT_SUPPLIED

  def getCurrentDateTime(): DateTimeValue =
    throw new NoDynamicContextException("current-dateTime")

  def getImplicitTimezone(): Int = CalendarValue.MISSING_TIMEZONE

  def iterateStackFrames(): Iterator[AnyRef] = Collections.EMPTY_LIST.iterator().asInstanceOf[Iterator[AnyRef]]

  def getCurrentException(): XPathException = null

  def waitForChildThreads(): Unit = {
    getCaller.waitForChildThreads()
  }

  def setTemporaryOutputState(temporary: Int): Unit = {}
// no action
// no action

  /**
    * Ask whether the XSLT output state is "temporary" or "final"
    *
    * @return non-zero in temporary output state; zero in final output state
    */
  def getTemporaryOutputState(): Int = 0

  def setCurrentOutputUri(uri: String): Unit = {}
// no action
// no action

  def getCurrentOutputUri(): String = null

  private def notAllowed(): Unit = {
    throw new UnsupportedOperationException(
      new NoDynamicContextException(
        "Internal error: early evaluation of subexpression with no context"))
  }

  /**
    * Get the thread manager used to process asynchronous xsl:result-document threads.
    *
    * @return the current thread manager; or null if multithreading is not supported
    */
  def getThreadManager(): XPathContextMajor.ThreadManager = null

  def getTargetComponent(bindingSlot: Int): Component = null

}

// Copyright (c) 2018-2020 Saxonica Limited
// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is "Incompatible With Secondary Licenses", as defined by the Mozilla Public License, v. 2.0.
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
/**
  * This class is an implementation of XPathContext used when evaluating constant sub-expressions at
  * compile time.
  */
