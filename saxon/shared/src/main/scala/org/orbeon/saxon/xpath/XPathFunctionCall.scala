////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
package org.orbeon.saxon.xpath

import org.orbeon.saxon.utils.Configuration

import org.orbeon.saxon.expr._

import org.orbeon.saxon.expr.parser.ExpressionVisitor

import org.orbeon.saxon.expr.parser.PathMap

import org.orbeon.saxon.expr.parser.RebindingMap

import org.orbeon.saxon.model.ItemType

import org.orbeon.saxon.model.Type

import org.orbeon.saxon.om._

import org.orbeon.saxon.trans.XPathException

import org.orbeon.saxon.value.EmptySequence

import javax.xml.xpath.XPathFunction

import javax.xml.xpath.XPathFunctionException

import java.util.ArrayList

import java.util.List




class XPathFunctionCall(var name: StructuredQName,
                        private var function: XPathFunction)
  extends FunctionCall
    with Callable {

  /**
   * Get the qualified name of the function being called
   *
   * @return the qualified name
   */
  override def getFunctionName: StructuredQName = name

  /**
   * Get the target function to be called
   *
   * @param context the dynamic evaluation context
   * @return always null
   */
  override def getTargetFunction(context: XPathContext): Function = null

  /*@NotNull*/

  override def preEvaluate(visitor: ExpressionVisitor): Expression = this

  override def getIntrinsicDependencies: Int = 0

  /*@NotNull*/

  def copy(rebindings: RebindingMap): Expression =
    new XPathFunctionCall(name, function)

  /*@NotNull*/

  override def addToPathMap(
                    pathMap: PathMap,
                    pathMapNodeSet: PathMap.PathMapNodeSet): PathMap.PathMapNodeSet =
    addExternalFunctionCallToPathMap(pathMap, pathMapNodeSet)

  /*@NotNull*/

  override def iterate(context: XPathContext): SequenceIterator = {
    val argValues: Array[Sequence] = Array.ofDim[Sequence](getArity)
    for (i <- 0 until argValues.length) {
      argValues(i) = SequenceTool.toLazySequence(getArg(i).iterate(context))
    }
    call(context, argValues).iterate()
  }

  /*@Nullable*/

  def call(context: XPathContext, argValues: Array[Sequence]): Sequence = {
    // a double is passed as a Double, a string as a String.
    val convertedArgs: List[Any] = new ArrayList[Any](argValues.length)
    val config: Configuration = context.getConfiguration
    for (argValue <- argValues) {
      val target: List[Any] = new ArrayList[Any]()
      argValue
        .iterate()
        .forEachOrFail((item) => {
          val converter: PJConverter = PJConverter.allocate(
            config,
            Type.getItemType(item, config.getTypeHierarchy),
            StaticProperty.ALLOWS_ONE,
            classOf[AnyRef])
          target.add(converter.convert(item, classOf[AnyRef], context))
        })
      if (target.size == 1) {
        convertedArgs.add(target.get(0))
      } else {
        convertedArgs.add(target)
      }
    }
    val result: AnyRef = function.evaluate(convertedArgs)
    if (result == null) {
      EmptySequence.getInstance
    }
    val converter: JPConverter =
      JPConverter.allocate(result.getClass, null, config)
    converter.convert(result, context)
  }
  // An argument is supplied to the extension function as a List, unless it is a singleton.
  // The items within the list are converted to the "natural Java representation", for example
  // An argument is supplied to the extension function as a List, unless it is a singleton.
  // The items within the list are converted to the "natural Java representation", for example

  /*@NotNull*/

  def getItemType: ItemType = Type.ITEM_TYPE

  /**
   * Determine the cardinality of the result
   *
   * @return ZERO_OR_MORE (we don't know)
   */
  def computeCardinality: Int = StaticProperty.ALLOWS_ZERO_OR_MORE

}

// Copyright (c) 2018-2020 Saxonica Limited
// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is "Incompatible With Secondary Licenses", as defined by the Mozilla Public License, v. 2.0.
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
/**
 * This class is an expression that calls an external function supplied using the
 * JAXP XPathFunction interface
 */
