////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
package net.sf.saxon.expr

import net.sf.saxon.event.Outputter

import net.sf.saxon.model.AnyItemType

import net.sf.saxon.model.ItemType

import net.sf.saxon.om.Item

import net.sf.saxon.om.SequenceIterator

import net.sf.saxon.trans.XPathException




/**
  * A pseudo-expression is an object that can appear as a node on the expression tree, but which cannot
  * actually be evaluated in its own right. An example is a sort key definition. This has to be a node
  * on the tree, because it contains subexpressions, so recursive traversals of the tree need to process
  * it. But evaluating a sort key definition throws an exception.
  *
  * <p>A constraint for pseudo-expressions is that rewrite methods (simplify, typecheck, promote etc)
  * always return an object of the same class (nearly always the same object)</p>
  */
abstract class PseudoExpression extends Expression {

  private def cannotEvaluate(): Unit = {
    throw new XPathException("Cannot evaluate " + getClass.getName)
  }

  override def getImplementationMethod(): Int = 0

   override def computeCardinality(): Int =
    StaticProperty.ALLOWS_ZERO_OR_MORE

  override def getItemType: ItemType = AnyItemType

  override def evaluateItem(context: XPathContext): Item = {
    cannotEvaluate()
    null
  }

  override def effectiveBooleanValue(context: XPathContext): Boolean = {
    cannotEvaluate()
    false
  }

  override def evaluateAsString(context: XPathContext): CharSequence = {
    cannotEvaluate()
    ""
  }

  override def iterate(context: XPathContext): SequenceIterator = {
    cannotEvaluate()
    null
  }

  override def process(output: Outputter, context: XPathContext): Unit = {
    cannotEvaluate()
  }

}

// Copyright (c) 2013-2020 Saxonica Limited
// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is "Incompatible With Secondary Licenses", as defined by the Mozilla Public License, v. 2.0.
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
