package net.sf.saxon.expr

import net.sf.saxon.utils.Configuration

import net.sf.saxon.expr.parser._

import net.sf.saxon.model.AnyItemType

import net.sf.saxon.model.ErrorType

import net.sf.saxon.model.ItemType

import net.sf.saxon.model.UType

import net.sf.saxon.om.Item

import net.sf.saxon.om.SequenceIterator

import net.sf.saxon.pattern.AnchorPattern

import net.sf.saxon.pattern.Pattern

import net.sf.saxon.trace.ExpressionPresenter

import net.sf.saxon.trans.XPathException

import net.sf.saxon.tree.iter.SingletonIterator

import net.sf.saxon.value.SequenceType


class ContextItemExpression extends Expression {

  private var staticInfo: ContextItemStaticInfo = ContextItemStaticInfo.DEFAULT

  private var errorCodeForAbsentContext: String = "XPDY0002"

  private var absentContextIsTypeError: Boolean = false

  override def getExpressionName(): String = "dot"

  /*@NotNull*/

  def copy(rebindings: RebindingMap): Expression = {
    val cie2: ContextItemExpression = new ContextItemExpression()
    cie2.staticInfo = staticInfo
    cie2.setErrorCodeForUndefinedContext(errorCodeForAbsentContext, isTypeError = false)
    ExpressionTool.copyLocationInfo(this, cie2)
    cie2
  }

  def setErrorCodeForUndefinedContext(errorCode: String,
                                      isTypeError: Boolean): Unit = {
    errorCodeForAbsentContext = errorCode
    absentContextIsTypeError = isTypeError
  }

  def getErrorCodeForUndefinedContext(): String = errorCodeForAbsentContext

  def setStaticInfo(info: ContextItemStaticInfo): Unit = {
    staticInfo = info
  }

  /*@NotNull*/

  override def typeCheck(visitor: ExpressionVisitor,
                         contextInfo: ContextItemStaticInfo): Expression = {
    if (contextInfo.getItemType == ErrorType.getInstance) {
      visitor.issueWarning(
        "Evaluation will always fail: there is no context item",
        getLocation)
      val ee: ErrorExpression = new ErrorExpression(
        "There is no context item",
        getErrorCodeForUndefinedContext,
        absentContextIsTypeError)
      ee.setOriginalExpression(this)
      ExpressionTool.copyLocationInfo(this, ee)
      return ee
    } else {
      staticInfo = contextInfo
    }
    this
  }

  override def optimize(visitor: ExpressionVisitor,
                        contextItemType: ContextItemStaticInfo): Expression = {
    if (contextItemType == null) {
      val err = new XPathException(
        "The context item is undefined at this point")
      err.setErrorCode(getErrorCodeForUndefinedContext)
      err.setIsTypeError(absentContextIsTypeError)
      err.setLocation(getLocation)
      throw err
    }
    this
  }

  // In XSLT, we don't catch this error at the typeCheck() phase because it's done one XPath expression
  // In XSLT, we don't catch this error at the typeCheck() phase because it's done one XPath expression

  /*@NotNull*/

  def getItemType(): ItemType = staticInfo.getItemType

  override def getStaticUType(contextItemType: UType): UType = contextItemType

  def isContextPossiblyUndefined(): Boolean = staticInfo.isPossiblyAbsent

  def computeCardinality(): Int = StaticProperty.EXACTLY_ONE

  override def computeSpecialProperties(): Int = {
    val p: Int = super.computeSpecialProperties()
    p | StaticProperty.NO_NODES_NEWLY_CREATED | StaticProperty.CONTEXT_DOCUMENT_NODESET
  }

  override def getImplementationMethod(): Int = Expression.EVALUATE_METHOD

  override def equals(other: Any): Boolean =
    other.isInstanceOf[ContextItemExpression]

  override def computeHashCode(): Int = "ContextItemExpression".hashCode

  override def getIntrinsicDependencies(): Int = StaticProperty.DEPENDS_ON_CONTEXT_ITEM

  override def addToPathMap(
                             pathMap: PathMap,
                             pathMapNodeSet: PathMap.PathMapNodeSet): PathMap.PathMapNodeSet = {
    var pmnSet = pathMapNodeSet
    if (pmnSet == null) {
      pmnSet = new PathMap.PathMapNodeSet(pathMap.makeNewRoot(this))
    }
    pmnSet
  }

  override def isSubtreeExpression(): Boolean = true

  override def getNetCost(): Int = 0

  /**
   * Convert this expression to an equivalent XSLT pattern
   *
   * @param config the Saxon configuration
   * @return the equivalent pattern
   * @throws XPathException
   * if conversion is not possible
   */
  override def toPattern(config: Configuration): Pattern =
    AnchorPattern.getInstance

  /**
   * Get the (partial) name of a class that supports streaming of this kind of expression
   *
   * @return the partial name of a class that can be instantiated to provide streaming support in Saxon-EE,
   *         or null if there is no such class
   */
  override def getStreamerName(): String = "ContextItemExpr"

  /*@NotNull*/

  override def iterate(context: XPathContext): SequenceIterator = {
    val item: Item = context.getContextItem
    if (item == null) {
      reportAbsentContext(context)
    }
    SingletonIterator.makeIterator(item)
  }

  override def evaluateItem(context: XPathContext): Item = {
    val item: Item = context.getContextItem
    if (item == null) {
      reportAbsentContext(context)
    }
    item
  }

  private def reportAbsentContext(context: XPathContext): Unit = {
    if (absentContextIsTypeError) {
      typeError("The context item is absent",
        getErrorCodeForUndefinedContext,
        context)
    } else {
      dynamicError("The context item is absent",
        getErrorCodeForUndefinedContext,
        context)
    }
  }

  override def toString: String = "."

  def export(destination: ExpressionPresenter): Unit = {
    destination.startElement("dot", this)
    val `type`: ItemType = getItemType
    if (!(`type` == AnyItemType)) {
      val st: SequenceType =
        SequenceType.makeSequenceType(`type`, StaticProperty.EXACTLY_ONE)
      destination.emitAttribute("type", st.toAlphaCode)
    }
    if (staticInfo.isPossiblyAbsent) {
      destination.emitAttribute("flags", "a")
    }
    destination.endElement()
  }

  override def toShortString(): String = "."

}

// Copyright (c) 2018-2020 Saxonica Limited
// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is "Incompatible With Secondary Licenses", as defined by the Mozilla Public License, v. 2.0.
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
/**
 * This class represents the expression "(dot)", which always returns the context item.
 * This may be a AtomicValue or a Node.
 */
