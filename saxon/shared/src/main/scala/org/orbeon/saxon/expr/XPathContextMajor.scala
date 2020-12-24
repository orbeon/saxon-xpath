////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// Copyright (c) 2018-2020 Saxonica Limited
// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is "Incompatible With Secondary Licenses", as defined by the Mozilla Public License, v. 2.0.
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
package org.orbeon.saxon.expr

import org.orbeon.saxon.expr.instruct.{Executable, ParameterSet, SlotManager}
import org.orbeon.saxon.expr.sort.GroupIterator
import org.orbeon.saxon.lib.{ErrorReporter, StandardURIResolver}
import org.orbeon.saxon.om._
import org.orbeon.saxon.regex.RegexIterator
import org.orbeon.saxon.trans.XPathException
import org.orbeon.saxon.trans.rules.Rule
import org.orbeon.saxon.utils.Controller

/*import org.orbeon.saxon.trans.rules.RuleManager*/
// RuleManager not exist
import java.util

import javax.xml.transform.URIResolver
import org.orbeon.saxon.expr.Component.M
import org.orbeon.saxon.expr.TailCallLoop.TailCallInfo
import org.orbeon.saxon.expr.XPathContextMajor.ThreadManager
import org.orbeon.saxon.tree.iter.SingletonIterator

/**
 * This class represents a "major context" in which an XPath expression is evaluated:
 * a "major context" object allows all aspects of the dynamic context to change, whereas
 * a "minor context" only allows changes to the focus and the destination for push output.
 */
object XPathContextMajor {
  /**
   * Create a new "major" context (one that is capable of holding a stack frame with local variables
   *
   * @param prev the previous context (the one causing the new context to be created)
   * @return the new major context
   */
  def newContext(prev: XPathContextMinor): XPathContextMajor = {

    val c = new XPathContextMajor

    var p: XPathContext = prev
    while (! p.isInstanceOf[XPathContextMajor])
      p = p.getCaller

    c.controller                = p.getController
    c.currentIterator           = prev.getCurrentIterator
    c.stackFrame                = prev.getStackFrame
    c.localParameters           = p.getLocalParameters
    c.tunnelParameters          = p.getTunnelParameters
    c.last                      = prev.last
    c.currentDestination        = prev.currentDestination
    c.currentMode               = p.getCurrentMode
    c.currentTemplate           = p.getCurrentTemplateRule
    c.currentRegexIterator      = p.getCurrentRegexIterator
    c.currentGroupIterator      = p.getCurrentGroupIterator
    c.currentMergeGroupIterator = p.getCurrentMergeGroupIterator
    c.caller                    = prev
    c.tailCallInfo              = null
    c.threadManager             = p.asInstanceOf[XPathContextMajor].threadManager
    c.currentComponent          = p.asInstanceOf[XPathContextMajor].currentComponent
    c.errorReporter             = p.asInstanceOf[XPathContextMajor].errorReporter
    c.uriResolver               = p.asInstanceOf[XPathContextMajor].uriResolver
    c.temporaryOutputState      = prev.temporaryOutputState
    c
  }

  /**
   * Make a copy of the supplied context for use in a new thread (typically for
   * an asynchronous xsl:result-document)
   *
   * @param prev the context to be copied
   * @return the copy of the context
   */
  def newThreadContext(prev: XPathContextMinor): XPathContextMajor = {
    val c = newContext(prev)
    c.stackFrame = prev.stackFrame.copy
    c
  }

  /**
   * The ThreadManager is used to manage asynchronous execution of xsl:result-document instructions in Saxon-EE.
   * This is a dummy implementation for Saxon-HE and Saxon-PE; it is subclassed in Saxon-EE
   */
  abstract class ThreadManager {
    @throws[XPathException]
    def waitForChildThreads(): Unit
  }
}

class XPathContextMajor private () extends XPathContextMinor {

  private var localParameters          : ParameterSet      = _
  private var tunnelParameters         : ParameterSet      = _
  private var tailCallInfo             : TailCallInfo      = _
  private var currentMode              : M                 = _
  private var currentTemplate          : Rule              = _
  private var currentGroupIterator     : GroupIterator     = _
  private var currentMergeGroupIterator: GroupIterator     = _
  private var currentRegexIterator     : RegexIterator     = _
  private var origin                   : ContextOriginator = _
  private var threadManager            : ThreadManager     = _
  private var uriResolver              : URIResolver       = _
  private var errorReporter            : ErrorReporter     = _
  private var currentComponent         : Component         = _
  private[expr] var currentException   : XPathException    = _

  /**
   * Constructor should only be called by the Controller,
   * which acts as a XPathContext factory.
   *
   * @param controller the Controller
   */
  def this(controller: Controller) = {
    this()
    this.controller = controller
    stackFrame = StackFrame.EMPTY
    origin = controller
  }

  /**
   * Constructor for use in free-standing Java applications.
   *
   * @param item the item to use as the initial context item. If this is null,
   *             the comtext item is initially undefined (which will cause a dynamic error
   *             if it is referenced).
   * @param exec the Executable
   */
  def this(item: Item, exec: Executable) = {
    this()
    controller = new Controller(exec.getConfiguration, exec) /*if (exec.isInstanceOf[PreparedStylesheet]) new XsltController(exec.getConfiguration, exec.asInstanceOf[PreparedStylesheet]) // PreparedStylesheet not found
    else*/
    if (item != null) {
      val iter = SingletonIterator.makeIterator(item)
      currentIterator = new FocusTrackingIterator(iter)
      try
        currentIterator.next()
      catch {
        case _: XPathException =>
        // cannot happen
      }
      last = new XPathContextMinor.LastValue(1)
    }
    origin = controller
  }

  /**
   * Construct a new context as a copy of another. The new context is effectively added
   * to the top of a stack, and contains a pointer to the previous context.
   */
  override def newContext: XPathContextMajor = {
    val c = new XPathContextMajor
    c.controller                = controller
    c.currentIterator           = currentIterator
    c.stackFrame                = stackFrame
    c.localParameters           = localParameters
    c.tunnelParameters          = tunnelParameters
    c.last                      = last
    c.currentDestination        = currentDestination
    c.currentMode               = currentMode
    c.currentTemplate           = currentTemplate
    c.currentRegexIterator      = currentRegexIterator
    c.currentGroupIterator      = currentGroupIterator
    c.currentMergeGroupIterator = currentMergeGroupIterator
    c.currentException          = currentException
    c.caller                    = this
    c.tailCallInfo              = null
    c.temporaryOutputState      = temporaryOutputState
    c.threadManager             = threadManager
    c.currentComponent          = currentComponent
    c.errorReporter             = errorReporter
    c.uriResolver               = uriResolver
    c
  }

  /**
   * Get the thread manager in use for this context.
   *
   * @return the current thread manager. This will be null if not running XSLT under Saxon-EE
   */
  override def getThreadManager: ThreadManager = threadManager

  /**
   * Create a new thread manager. This is called when starting an XSLT Transformation, and also
   * when entering a try/catch block. In Saxon-HE it does nothing.
   */
  def createThreadManager(): Unit = threadManager = getConfiguration.makeThreadManager

  /**
   * Wait for child threads started under the control of this context to finish.
   * This is called at the end of the (main thread of a) transformation, and also
   * at the end of the "try" part of a try/catch. The threads affected are those
   * used to implement xsl:result-document instructions.
   *
   * @throws XPathException if any of the child threads have failed with a dynamic
   *                        error.
   */
  @throws[XPathException]
  override def waitForChildThreads(): Unit =
    if (threadManager != null) threadManager.waitForChildThreads()

  /**
   * Get the local parameters for the current template call.
   *
   * @return the supplied parameters
   */
  override def getLocalParameters: ParameterSet = {
    if (localParameters == null)
      localParameters = new ParameterSet
    localParameters
  }

  /**
   * Set the local parameters for the current template call.
   *
   * @param localParameters the supplied parameters
   */
  def setLocalParameters(localParameters: ParameterSet): Unit =
    this.localParameters = localParameters

  /**
   * Get the tunnel parameters for the current template call.
   *
   * @return the supplied tunnel parameters
   */
  override def getTunnelParameters: ParameterSet = tunnelParameters

  /**
   * Set the tunnel parameters for the current template call.
   *
   * @param tunnelParameters the supplied tunnel parameters
   */
  def setTunnelParameters(tunnelParameters: ParameterSet): Unit = this.tunnelParameters = tunnelParameters

  /**
   * Set the creating expression (for use in diagnostics). The origin is generally set to "this" by the
   * object that creates the new context. It's up to the debugger to determine whether this information
   * is useful. The object will either be an `Expression`, allowing information
   * about the calling instruction to be obtained, or null.
   */
  def setOrigin(expr: ContextOriginator): Unit = origin = expr

  /**
   * Get information about the creating expression or other construct.
   */
  def getOrigin: ContextOriginator = origin

  /**
   * Set the local stack frame. This method is used when creating a Closure to support
   * delayed evaluation of expressions. The "stack frame" is actually on the Java heap, which
   * means it can survive function returns and the like.
   *
   * @param map       the SlotManager, which holds static details of the allocation of variables to slots
   * @param variables the array of "slots" to hold the actual variable values. This array will be
   *                  copied if it is too small to hold all the variables defined in the SlotManager
   */
  def setStackFrame(map: SlotManager, variables: Array[Sequence]): Unit = {
    stackFrame = new StackFrame(map, variables)
    if (map != null && variables.length != map.getNumberOfVariables) {
      if (variables.length > map.getNumberOfVariables)
        throw new IllegalStateException("Attempting to set more local variables (" + variables.length + ") than the stackframe can accommodate (" + map.getNumberOfVariables + ")")
      stackFrame.slots = new Array[Sequence](map.getNumberOfVariables)
      System.arraycopy(variables, 0, stackFrame.slots, 0, variables.length)
    }
  }

  /**
   * Reset the stack frame variable map, while reusing the StackFrame object itself. This
   * is done on a tail call to a different function
   *
   * @param map            the SlotManager representing the stack frame contents
   * @param numberOfParams the number of parameters required on the new stack frame
   */
  def resetStackFrameMap(map: SlotManager, numberOfParams: Int): Unit = {
    stackFrame.map = map
    if (stackFrame.slots.length != map.getNumberOfVariables) {
      val v2 = new Array[Sequence](map.getNumberOfVariables)
      System.arraycopy(stackFrame.slots, 0, v2, 0, numberOfParams)
      stackFrame.slots = v2
    } else { // not strictly necessary
      //util.Arrays.fill(stackFrame.slots, numberOfParams, stackFrame.slots.length, null)
    }
  }

  /**
   * Get a all the variables in the stack frame
   *
   * @return an array holding all the variables, each referenceable by its slot number
   */
  def getAllVariableValues: Array[Sequence] = stackFrame.getStackFrameValues

  /**
   * Overwrite all the variables in the stack frame
   *
   * @param values an array holding all the variables, each referenceable by its slot number;
   *               the caller must ensure this is the correct length (and valid in other ways)
   */
  def resetAllVariableValues(values: Array[Sequence]): Unit = stackFrame.setStackFrameValues(values)

  /**
   * Overwrite all the parameters in the stack frame. (Used from compiled bytecode)
   *
   * @param values an array holding all the parameters, each referenceable by its slot number;
   *               the caller must ensure this is the correct length (and valid in other ways)
   */
  def resetParameterValues(values: Array[Sequence]): Unit = System.arraycopy(values, 0, stackFrame.slots, 0, values.length)

  /**
   * Reset the local stack frame. This method is used when processing a tail-recursive function.
   * Instead of the function being called recursively, the parameters are set to new values and the
   * function body is evaluated repeatedly
   *
   * @param targetFn  the user function being called using tail recursion
   * @param variables the parameter to be supplied to the user function
   */
  def requestTailCall(targetFn: TailCallLoop.TailCallInfo, variables: Array[Sequence]): Unit = {
    if (variables != null)
      if (variables.length > stackFrame.slots.length)
        stackFrame.slots = util.Arrays.copyOf(variables, variables.length)
      else
        System.arraycopy(variables, 0, stackFrame.slots, 0, variables.length)
    tailCallInfo = targetFn
  }

  /**
   * Determine whether the body of a function is to be repeated, due to tail-recursive function calls
   *
   * @return null if no tail call has been requested, or the name of the function to be called otherwise
   */
  def getTailCallInfo: TailCallInfo = {
    val fn = tailCallInfo
    tailCallInfo = null
    fn
  }

  /**
   * Create a new stack frame for local variables, using the supplied SlotManager to
   * define the allocation of slots to individual variables
   *
   * @param map the SlotManager for the new stack frame
   */
  def openStackFrame(map: SlotManager): Unit = {
    val numberOfSlots = map.getNumberOfVariables
    if (numberOfSlots == 0)
      stackFrame = StackFrame.EMPTY
    else
      stackFrame = new StackFrame(map, new Array[Sequence](numberOfSlots))
  }

  /**
   * Create a new stack frame large enough to hold a given number of local variables,
   * for which no stack frame map is available. This is used in particular when evaluating
   * match patterns of template rules.
   *
   * @param numberOfVariables The number of local variables to be accommodated.
   */
  def openStackFrame(numberOfVariables: Int): Unit =
    stackFrame = new StackFrame(new SlotManager(numberOfVariables), SequenceTool.makeSequenceArray(numberOfVariables))

  /**
   * Set the current mode.
   *
   * @param mode the new current mode
   */
  def setCurrentMode(mode: Component.M): Unit = this.currentMode = mode

  /**
   * Get the current mode.
   *
   * @return the current mode. May return null if the current mode is the default mode.
   */
  override def getCurrentMode: M = {
    currentMode /*val m: M = currentMode
    if (m == null) {
      val rm = getController.getRuleManager
      if (rm != null) rm.getUnnamedMode.getDeclaringComponent // RuleManager Class not found
      else null
    }
    else m*/
  }

  /**
   * Set the current template. This is used to support xsl:apply-imports. The caller
   * is responsible for remembering the previous current template and resetting it
   * after use.
   *
   * @param rule the current template rule, or null to indicate that there is no current template rule
   */
  def setCurrentTemplateRule(rule: Rule): Unit = this.currentTemplate = rule

  /**
   * Get the current template. This is used to support xsl:apply-imports
   *
   * @return the current template
   */
  override def getCurrentTemplateRule: Rule = currentTemplate

  /**
   * Set the current grouping iterator. This supports the current-group() and
   * current-grouping-key() functions in XSLT 2.0
   *
   * @param iterator the new current GroupIterator
   */
  def setCurrentGroupIterator(iterator: GroupIterator): Unit = this.currentGroupIterator = iterator

  /**
   * Get the current group iterator. This supports the current-group() and
   * current-grouping-key() functions in XSLT 2.0
   *
   * @return the current grouped collection
   */
  override def getCurrentGroupIterator: GroupIterator = currentGroupIterator

  /**
   * Set the current merge group iterator. This supports the current-merge-group() and
   * current-merge-key() functions in XSLT 3.0
   *
   * @param iterator the new current GroupIterator
   */
  def setCurrentMergeGroupIterator(iterator: GroupIterator): Unit = this.currentMergeGroupIterator = iterator

  /**
   * Get the current merge group iterator. This supports the current-merge-group() and
   * current-merge-key() functions in XSLT 3.0
   *
   * @return the current grouped collection
   */
  override def getCurrentMergeGroupIterator: GroupIterator = currentMergeGroupIterator

  /**
   * Set the current regex iterator. This supports the functionality of the regex-group()
   * function in XSLT 2.0.
   *
   * @param currentRegexIterator the current regex iterator
   */
  def setCurrentRegexIterator(currentRegexIterator: RegexIterator): Unit = this.currentRegexIterator = currentRegexIterator

  /**
   * Get the current regex iterator. This supports the functionality of the regex-group()
   * function in XSLT 2.0.
   *
   * @return the current regular expressions iterator
   */
  override def getCurrentRegexIterator: RegexIterator = currentRegexIterator

  /**
   * Use local parameter. This is called when a local xsl:param element is processed.
   * If a parameter of the relevant name was supplied, it is bound to the xsl:param element.
   * Otherwise the method returns false, so the xsl:param default will be evaluated
   *
   * @param paramName  the name of the parameter
   * @param slotNumber the slot number of the parameter on the callee's stack frame
   * @param isTunnel   True if a tunnel parameter is required, else false
   * @return ParameterSet.NOT_SUPPLIED, ParameterSet.SUPPLIED, or ParameterSet.SUPPLIED_AND_CHECKED
   */
  @throws[XPathException]
  override def useLocalParameter(paramName: StructuredQName, slotNumber: Int, isTunnel: Boolean): Int = {
    val params =
      if (isTunnel)
        getTunnelParameters
      else
        localParameters
    if (params == null)
      return ParameterSet.NOT_SUPPLIED
    val index = params.getIndex(paramName)
    if (index < 0)
      return ParameterSet.NOT_SUPPLIED
    val `val` = params.getValue(index)
    stackFrame.slots(slotNumber) = `val`
    val checked = params.isTypeChecked(index)
    if (checked)
      ParameterSet.SUPPLIED_AND_CHECKED
    else
      ParameterSet.SUPPLIED
  }

  def setURIResolver(resolver: URIResolver): Unit = {
    uriResolver = resolver
    resolver match {
      case standardUriResolver: StandardURIResolver => standardUriResolver.setConfiguration(getConfiguration)
      case _                                        =>
    }
  }

  /**
   * Get the URI resolver. This gets the local URIResolver set in the XPathContext if there
   * is one; if not, it gets the URIResolver from the Controller (which itself defaults to the
   * one set in the Configuration).
   *
   * @return the user-supplied URI resolver if there is one, or null otherwise.
   * @since 9.6
   */
  override def getURIResolver: URIResolver =
    if (uriResolver == null)
      controller.getURIResolver
    else
      uriResolver

  /**
   * Set the error reporter. The ErrorReporter is set locally to this XPathContext
   * object.
   *
   * @param reporter the ErrorReporter to be used
   * @since 9.6. Changed in 10.0 to use the ErrorReporter interface in place of ErrorListener
   */
  def setErrorReporter(reporter: ErrorReporter): Unit = this.errorReporter = reporter

  /**
   * Get the error reporter. If no ErrorReporter
   * has been set locally, the ErrorReporter in the Controller is returned; this in turn defaults
   * to the ErrorReporter set in the Configuration.
   *
   * @return the ErrorReporter in use.
   * @since 9.6. Changed in 10.0 to use an ErrorReporter rather than ErrorListener
   */
  override def getErrorReporter: ErrorReporter =
    if (errorReporter == null)
      controller.getErrorReporter
    else
      errorReporter

  /**
   * Set the current exception (in saxon:catch)
   *
   * @param exception the current exception
   */
  def setCurrentException(exception: XPathException): Unit = currentException = exception

  /**
   * Get the current exception (in saxon:catch)
   *
   * @return the current exception, or null if there is none defined
   */
  override def getCurrentException: XPathException = currentException

  /**
   * Get the current component
   */
  override def getCurrentComponent: Component = currentComponent

  /**
   * Set the current component, that is, the component being evaluated. This is used during the evaluation
   * to determine the bindings to other components (such as global variables, functions, and templates) referenced
   * during the evaluation
   *
   * @param component the current component
   */
  def setCurrentComponent(component: Component): Unit = {
    //System.err.println("Set current component := " + (component==null ? "null" : component.getCode()));
    currentComponent = component
  }

  /**
   * Bind a component reference to a component. This is used for binding component references
   * (such as function calls, global variable references, or xsl:call-template) across package
   * boundaries. The binding is done dynamically because, in the presence of overridden components,
   * the choice among different components with the same name depends on which package the caller
   * is in.
   *
   * @param bindingSlot Binding slots are allocated statically to the external component references
   *                    in every component: for example, in the case of a template, to all global
   *                    variable references, named function calls, and named template calls within
   *                    that template. The binding slot therefore identifies the name of the
   *                    component that is required; and the selection of an actual component is
   *                    done by selection from the binding vector of the component currently being
   *                    executed
   * @return the component to be invoked
   */
  override def getTargetComponent(bindingSlot: Int): Component =
    try {
      val binding = currentComponent.getComponentBindings.get(bindingSlot)
      binding.getTarget
    } catch {
      case e: NullPointerException =>
        // Suggests that the current component is null, which would be a bug
        e.printStackTrace()
        throw e
      case e: IndexOutOfBoundsException =>
        // Suggests that the current component's binding vector is the wrong size, which would be a bug.
        //System.err.println("Current component = " + currentComponent.getCode());
        e.printStackTrace()
        throw e
    }
}