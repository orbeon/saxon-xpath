////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// Copyright (c) 2018-2020 Saxonica Limited
// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is "Incompatible With Secondary Licenses", as defined by the Mozilla Public License, v. 2.0.
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
package org.orbeon.saxon.om

import org.orbeon.saxon.expr.sort.AtomicComparer
import org.orbeon.saxon.expr.{Callable, ContextOriginator, OperandRole, XPathContext}
import org.orbeon.saxon.model.FunctionItemType
import org.orbeon.saxon.om.Genre.Genre
import org.orbeon.saxon.query.AnnotationList
import org.orbeon.saxon.trace.ExpressionPresenter


/**
  * XDM 3.0 introduces a third kind of item, beyond nodes and atomic values: the function. Functions
  * implement this interface.
  */
object Function {
  @SafeVarargs
  def argumentArray(args: Sequence*): Seq[Sequence] = args
}

trait Function extends Item with Callable with GroundedValue {

  def isMap: Boolean
  def isArray: Boolean
  def getFunctionItemType: FunctionItemType
  def getFunctionName: StructuredQName
  def getArity: Int
  def getOperandRoles: Array[OperandRole]
  def getAnnotations: AnnotationList
  def makeNewContext(callingContext: XPathContext, originator: ContextOriginator): XPathContext
  def call(context: XPathContext, args: Array[Sequence]): Sequence

  def deepEquals(other: Function,
                 context: XPathContext,
                 comparer: AtomicComparer,
                 flags: Int): Boolean

  def getDescription: String
  def export(out: ExpressionPresenter): Unit
  def isTrustedResultType: Boolean

  /**
    * Provide a short string showing the contents of the item, suitable
    * for use in error messages
    *
    * @return a depiction of the item suitable for use in error messages
    */
  override def toShortString: String = getDescription

  /**
    * Get the genre of this item
    *
    * @return the genre: specifically, {@link Genre#FUNCTION}. Overridden for maps and arrays.
    */
  override def getGenre: Genre = Genre.FUNCTION
}
