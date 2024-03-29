////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// Copyright (c) 2013-2020 Saxonica Limited
// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is "Incompatible With Secondary Licenses", as defined by the Mozilla Public License, v. 2.0.
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
package org.orbeon.saxon.expr.sort

import java.util.{ArrayList, Arrays, Iterator, List}

import org.orbeon.saxon.expr.{Operand, OperandRole, PseudoExpression}
import org.orbeon.saxon.expr.parser.RebindingMap
import org.orbeon.saxon.trace.ExpressionPresenter




/**
  * The class represents a list of sort key definitions in major-to-minor sort key order. It is not a true
  * expression, because it cannot be evaluated, but it acts as a node in the expression tree, and is therefore
  * classified as a pseudo-expression.
  */
class SortKeyDefinitionList(
    private var sortKeyDefinitions: Array[SortKeyDefinition])
    extends PseudoExpression
    with java.lang.Iterable[SortKeyDefinition] {

  override def operands: java.lang.Iterable[Operand] = {
    val list: List[Operand] = new ArrayList[Operand](size)
    for (skd <- sortKeyDefinitions) {
      list.add(new Operand(this, skd, OperandRole.INSPECT))
    }
    list
  }

  override def isLiftable(forStreaming: Boolean): Boolean = false

  def size: Int = sortKeyDefinitions.length

  def getSortKeyDefinition(i: Int): SortKeyDefinition = sortKeyDefinitions(i)

  def iterator: Iterator[SortKeyDefinition] =
    Arrays.asList(sortKeyDefinitions: _*).iterator

  override def copy(rebindings: RebindingMap): SortKeyDefinitionList = {
    val s2: Array[SortKeyDefinition] =
      Array.ofDim[SortKeyDefinition](sortKeyDefinitions.length)
    for (i <- 0 until sortKeyDefinitions.length) {
      s2(i) = sortKeyDefinitions(i).copy(rebindings)
    }
    new SortKeyDefinitionList(s2)
  }

  /**
    * An implementation of Expression must provide at least one of the methods evaluateItem(), iterate(), or process().
    * This method indicates which of these methods is provided directly. The other methods will always be available
    * indirectly, using an implementation that relies on one of the other methods.
    *
    * @return the implementation method, for example {@link #ITERATE_METHOD} or {@link #EVALUATE_METHOD} or
    * {@link #PROCESS_METHOD}
    */
  override def getImplementationMethod: Int = 0

  override def export(out: ExpressionPresenter): Unit = {
    for (skd <- sortKeyDefinitions) {
      skd.export(out)
    }
  }

}
