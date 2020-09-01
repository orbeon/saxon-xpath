////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
package net.sf.saxon.lib

import net.sf.saxon.expr.Callable

import net.sf.saxon.expr.Expression

import net.sf.saxon.expr.StaticContext

import net.sf.saxon.expr.XPathContext

import net.sf.saxon.expr.parser.ExpressionTool

import net.sf.saxon.om.Sequence

import net.sf.saxon.trans.XPathException




/**
  * This abstract class is provided to allow user-written extension functions to be implemented
  * with the full capabilities of functions that are an intrinsic part of the Saxon product.
  * In particular, the class has the opportunity to save data from the static context and
  * to optimize itself at compile time.
  * <p>Instances of this class are created by calling the method makeCallExpression() on the
  * {@link ExtensionFunctionDefinition} object that represents the definition of the function.</p>
  * <p>The compiler will create one instance of this class for each function call appearing in the
  * expression being compiled. The class must therefore have a public zero-argument constructor.</p>
  * <p>The compiler will ensure that the supplied arguments in the extension function call are converted
  * if necessary to the declared argument types, by applying the standard conversion rules. The result
  * returned by the function is checked against the declared return type, but no conversion takes place:
  * the returned value must strictly conform to the declared type.</p>
  * <p>Note that an <code>ExtensionFunctionCall</code> is trusted; calls are allowed even if the configuration option
  * {@link net.sf.saxon.lib.FeatureKeys#ALLOW_EXTERNAL_FUNCTIONS} is false. In cases where an <code>ExtensionFunctionCall</code>
  * is used to load and execute untrusted code, it should check this configuration option before doing so.</p>
  *
  * @since 9.2; modified in 9.5 to use Sequence rather than SequenceIterator for the arguments and result
  */
abstract class ExtensionFunctionCall extends Callable {

  var definition: ExtensionFunctionDefinition = _

  def setDefinition(definition: ExtensionFunctionDefinition): Unit = {
    this.definition = definition
  }

  def getDefinition: ExtensionFunctionDefinition = definition

  def supplyStaticContext(context: StaticContext,
                          locationId: Int,
                          arguments: Array[Expression]): Unit = ()
// default implementation does nothing
// default implementation does nothing

  /*@Nullable*/

  def rewrite(context: StaticContext, arguments: Array[Expression])
    : Expression = // default implementation does nothing
    null

  def copyLocalData(destination: ExtensionFunctionCall): Unit = ()
// default implementation does nothing
// default implementation does nothing

  def call(context: XPathContext, arguments: Array[Sequence]): Sequence

  def effectiveBooleanValue(context: XPathContext,
                            arguments: Array[Sequence]): Boolean =
    ExpressionTool.effectiveBooleanValue(call(context, arguments).iterate())

  def getStreamingImplementation: AnyRef = null

}

// Copyright (c) 2018-2020 Saxonica Limited
// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is "Incompatible With Secondary Licenses", as defined by the Mozilla Public License, v. 2.0.
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
