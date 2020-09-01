////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// Copyright (c) 2018-2020 Saxonica Limited
// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is "Incompatible With Secondary Licenses", as defined by the Mozilla Public License, v. 2.0.
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
/**
  * This class represents a context in which an XPath expression is evaluated.
  */
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

import net.sf.saxon.value.DateTimeValue

import javax.xml.transform.URIResolver

import java.util.Iterator




trait XPathContext {

  def newContext(): XPathContextMajor

  def newCleanContext(): XPathContextMajor

  def newMinorContext(): XPathContextMinor

  def getLocalParameters: ParameterSet

  def getTunnelParameters: ParameterSet

  /*@Nullable*/

  def getController: Controller

  def getConfiguration: Configuration

  def getNamePool: NamePool

  def setCaller(caller: XPathContext): Unit

  def getCaller: XPathContext

  def trackFocus(iter: SequenceIterator): FocusIterator

  def setCurrentIterator(iter: FocusIterator): Unit

  def getCurrentIterator: FocusIterator

  def getContextItem: Item

  def getLast: Int

  def isAtLast: Boolean

  def getURIResolver: URIResolver

  def getErrorReporter: ErrorReporter

  def getCurrentComponent: Component

  def useLocalParameter(parameterId: StructuredQName,
                        slotNumber: Int,
                        isTunnel: Boolean): Int

  def getStackFrame: StackFrame

  def evaluateLocalVariable(slotnumber: Int): Sequence

  def setLocalVariable(slotNumber: Int, value: Sequence): Unit

  def setTemporaryOutputState(temporary: Int): Unit

  /**
    * Ask whether the XSLT output state is "temporary" or "final"
    *
    * @return non-zero if in temporary output state (integer identifies the state); zero if in final output state
    */
  def getTemporaryOutputState: Int

  def setCurrentOutputUri(uri: String): Unit

  def getCurrentOutputUri: String

  def getCurrentMode: Component.M

  def getCurrentTemplateRule: Rule

  def getCurrentGroupIterator: GroupIterator

  def getCurrentMergeGroupIterator: GroupIterator

  def getCurrentRegexIterator: RegexIterator

  def getCurrentDateTime: DateTimeValue

  def getImplicitTimezone: Int

  def iterateStackFrames(): Iterator[AnyRef]

  def getCurrentException: XPathException

  def getThreadManager: XPathContextMajor.ThreadManager

  def waitForChildThreads(): Unit

  def getTargetComponent(bindingSlot: Int): Component

}


