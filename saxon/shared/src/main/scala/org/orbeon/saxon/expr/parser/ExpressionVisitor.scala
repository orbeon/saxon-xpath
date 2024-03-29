////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// Copyright (c) 2018-2020 Saxonica Limited
// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is "Incompatible With Secondary Licenses", as defined by the Mozilla Public License, v. 2.0.
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
package org.orbeon.saxon.expr.parser

import org.orbeon.saxon.expr.{StaticContext, XPathContext}
import org.orbeon.saxon.expr.parser.ExpressionVisitor._
import org.orbeon.saxon.s9api.Location
import org.orbeon.saxon.utils.Configuration

import scala.beans.{BeanProperty, BooleanBeanProperty}


/**
  * The ExpressionVisitor supports the various phases of processing of an expression tree which require
  * a recursive walk of the tree structure visiting each node in turn. In maintains a stack holding the
  * ancestor nodes of the node currently being visited.
  */
object ExpressionVisitor {

  private val MAX_DEPTH: Int = 500

  def make(env: StaticContext): ExpressionVisitor = {
    val visitor = new ExpressionVisitor(env.getConfiguration)
    visitor.setStaticContext(env)
    visitor
  }
}

class ExpressionVisitor(private var config: Configuration) {

  @BeanProperty
  var staticContext: StaticContext = _

  @BooleanBeanProperty
  var optimizeForStreaming: Boolean = false

  @BooleanBeanProperty
  var optimizeForPatternMatching: Boolean = false

  private var optimizer: Optimizer = _

  private var depth: Int = 0

  @BooleanBeanProperty
  var suppressWarnings: Boolean = false

  def getConfiguration: Configuration = config

  def issueWarning(message: String, locator: Location): Unit =
    if (! isSuppressWarnings)
      staticContext.issueWarning(message, locator)

  def makeDynamicContext(): XPathContext =
    staticContext.makeEarlyEvaluationContext()

  def obtainOptimizer(): Optimizer = {
    if (optimizer == null)
      optimizer = config.obtainOptimizer(staticContext.getOptimizerOptions)
    optimizer
  }

  def getTargetEdition: String =
    staticContext.getPackageData.getTargetEdition

  def incrementAndTestDepth(): Boolean =
    { depth += 1; depth - 1 } < MAX_DEPTH

  def decrementDepth(): Unit =
     depth -= 1
}