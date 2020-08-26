////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
package net.sf.saxon.sxpath

import net.sf.saxon.expr.XPathContext

import net.sf.saxon.expr.XPathContextMajor

import net.sf.saxon.expr.instruct.SlotManager

import net.sf.saxon.expr.parser.TypeChecker

import net.sf.saxon.lib.CollectionFinder

import net.sf.saxon.lib.ErrorReporter

import net.sf.saxon.lib.UnparsedTextURIResolver

import net.sf.saxon.model.ItemType

import net.sf.saxon.model.TypeHierarchy

import net.sf.saxon.om._

import net.sf.saxon.trans.SaxonErrorCode

import net.sf.saxon.trans.XPathException

import net.sf.saxon.tree.iter.ManualIterator

import net.sf.saxon.value.SequenceType

import javax.xml.transform.URIResolver


/**
 * This object represents the dynamic XPath execution context for use in the free-standing Saxon XPath API.
 * The dynamic context holds the context item and the values of external variables used by the XPath expression.
 * <p>This object is always created via the method
 * {@link net.sf.saxon.sxpath.XPathExpression#createDynamicContext(net.sf.saxon.om.Item)}</p>
 */
class XPathDynamicContext(
                           private var contextItemType: ItemType,
                           private var contextObject: XPathContextMajor,
                           private var stackFrameMap: SlotManager) {

  def setContextItem(item: Item): Unit = {
    if (item.isInstanceOf[NodeInfo]) {
      if (!item
        .asInstanceOf[NodeInfo]
        .getConfiguration
        .isCompatible(contextObject.getConfiguration)) {
        throw new XPathException(
          "Supplied node must be built using the same or a compatible Configuration",
          SaxonErrorCode.SXXP0004)
      }
    }
    val th: TypeHierarchy = contextObject.getConfiguration.getTypeHierarchy
    if (!contextItemType.matches(item, th)) {
      throw new XPathException(
        "Supplied context item does not match required context item type " +
          contextItemType)
    }
    val iter: ManualIterator = new ManualIterator(item)
    contextObject.setCurrentIterator(iter)
  }

  def getContextItem(): Item = contextObject.getContextItem

  def setVariable(variable: XPathVariable, value: Sequence): Unit = {
    val requiredType: SequenceType = variable.getRequiredType
    if (requiredType != SequenceType.ANY_SEQUENCE) {
      val err: XPathException =
        TypeChecker.testConformance(value, requiredType, contextObject)
      if (err != null) {
        throw err
      }
    }
    val iter: SequenceIterator = value.iterate()
    var item: Item = null
    while (({
      item = iter.next()
      item
    }) != null) if (item.isInstanceOf[NodeInfo] &&
      !item
        .asInstanceOf[NodeInfo]
        .getConfiguration
        .isCompatible(
          contextObject.getConfiguration)) {
      throw new XPathException(
        "Supplied node must be built using the same or a compatible Configuration",
        SaxonErrorCode.SXXP0004)
    }
    val slot: Int = variable.getLocalSlotNumber
    val expectedName: StructuredQName =
      if (slot >= stackFrameMap.getNumberOfVariables) null
      else stackFrameMap.getVariableMap.get(slot)
    if (variable.getVariableQName != expectedName) {
      throw new XPathException(
        "Supplied XPathVariable is bound to the wrong slot: perhaps it was created using a different static context")
    }
    contextObject.setLocalVariable(slot, value)
  }

  def setURIResolver(resolver: URIResolver): Unit = {
    contextObject.setURIResolver(resolver)
  }

  def getURIResolver(): URIResolver = contextObject.getURIResolver

  def getCollectionFinder(): CollectionFinder =
    contextObject.getController.getCollectionFinder

  def setCollectionFinder(cf: CollectionFinder): Unit = {
    contextObject.getController.setCollectionFinder(cf)
  }

  def setErrorReporter(listener: ErrorReporter): Unit = {
    contextObject.setErrorReporter(listener)
  }

  def getErrorReporter(): ErrorReporter = contextObject.getErrorReporter

  def getXPathContextObject(): XPathContext = contextObject

  def setUnparsedTextURIResolver(resolver: UnparsedTextURIResolver): Unit = {
    contextObject.getController.setUnparsedTextURIResolver(resolver)
  }

  def getUnparsedTextURIResolver(): UnparsedTextURIResolver =
    contextObject.getController.getUnparsedTextURIResolver

  def checkExternalVariables(stackFrameMap: SlotManager,
                             numberOfExternals: Int): Unit = {
    val stack: Array[Sequence] =
      contextObject.getStackFrame.getStackFrameValues
    for (i <- 0 until numberOfExternals if stack(i) == null) {
      val qname: StructuredQName = stackFrameMap.getVariableMap.get(i)
      throw new XPathException(
        "No value has been supplied for variable $" + qname.getDisplayName)
    }
  }

}

// Copyright (c) 2018-2020 Saxonica Limited
// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is "Incompatible With Secondary Licenses", as defined by the Mozilla Public License, v. 2.0.
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
