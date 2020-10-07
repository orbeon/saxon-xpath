////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
package org.orbeon.saxon.lib

import org.orbeon.saxon.expr.XPathContext
import org.orbeon.saxon.functions.AbstractFunction
import org.orbeon.saxon.model.{FunctionItemType, SpecificFunctionType}
import org.orbeon.saxon.om.{Function, Sequence, StructuredQName}
import org.orbeon.saxon.trans.XPathException
import org.orbeon.saxon.value.SequenceType


/**
  * This abstract class is provided to allow user-written extension functions to be implemented
  * with the full capabilities of functions that are an intrinsic part of the Saxon product.
  * In particular, the class has the opportunity to save data from the static context and
  * to optimize itself at compile time.
  * <p>There should be one class implementing this interface for each function name; if there
  * are several functions with the same name but different arity, the same class should implement
  * them all.</p>
  * <p>Note that an IntegratedFunction is trusted; calls are allowed even if the configuration option
  * {@link org.orbeon.saxon.lib.FeatureKeys#ALLOW_EXTERNAL_FUNCTIONS} is false. In cases where an IntegratedFunction
  * is used to load and execute untrusted code, it should check this configuration option before doing so.</p>
  *
  * @since 9.2
  */
abstract class ExtensionFunctionDefinition {

  def getFunctionQName: StructuredQName

  def getMinimumNumberOfArguments: Int = getArgumentTypes.length

  def getMaximumNumberOfArguments: Int = getMinimumNumberOfArguments

  def getArgumentTypes: Array[SequenceType]

  def getResultType(suppliedArgumentTypes: Array[SequenceType]): SequenceType

  def trustResultType(): Boolean = false

  def dependsOnFocus(): Boolean = false

  def hasSideEffects: Boolean = false

  def makeCallExpression(): ExtensionFunctionCall

  def asFunction(): Function = new AbstractFunction() {

    /**
      * Invoke the function
      *
      * @param context the XPath dynamic evaluation context
      * @param args    the actual arguments to be supplied
      * @return the result of invoking the function
      * @throws XPathException if a dynamic error occurs within the function
      */
    override def call(context: XPathContext, args: Array[Sequence]): Sequence =
      makeCallExpression().call(context, args)

    /**
      * Get the item type of the function item
      *
      * @return the function item's type
      */
    override def getFunctionItemType: FunctionItemType =
      new SpecificFunctionType(getArgumentTypes,
                               getResultType(getArgumentTypes))

    /**
      * Get the name of the function, or null if it is anonymous
      *
      * @return the function name, or null for an anonymous inline function
      */
    override def getFunctionName: StructuredQName = getFunctionQName

    /**
      * Get the arity of the function
      *
      * @return the number of arguments in the function signature
      */
    override def getArity: Int = getArgumentTypes.length

    /**
      * Get a description of this function for use in error messages. For named functions, the description
      * is the function name (as a lexical QName). For others, it might be, for example, "inline function",
      * or "partially-applied ends-with function".
      *
      * @return a description of the function for use in error messages
      */
    override def getDescription: String = getFunctionQName.getDisplayName

    /**
      * Ask whether the result of the function should be checked against the declared return type
      * @return true if the result does not need to be checked (which can cause catastrophic
      * failure if this trust is misplaced)
      */
    override def isTrustedResultType(): Boolean = trustResultType()
  }

}
