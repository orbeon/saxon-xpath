////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
package net.sf.saxon.expr

import net.sf.saxon.expr.parser.PathMap

import net.sf.saxon.expr.parser.RebindingMap

import net.sf.saxon.model.BuiltInAtomicType

import net.sf.saxon.model.ItemType

import net.sf.saxon.model.Type

import net.sf.saxon.om.AxisInfo

import net.sf.saxon.om.FingerprintedQName

import net.sf.saxon.om.Item

import net.sf.saxon.om.NodeInfo

import net.sf.saxon.pattern.NameTest

import net.sf.saxon.trace.ExpressionPresenter

import net.sf.saxon.trans.XPathException

import net.sf.saxon.tree.tiny.TinyElementImpl

import net.sf.saxon.value.UntypedAtomicValue

import AttributeGetter._

import scala.beans.{BeanProperty, BooleanBeanProperty}




object AttributeGetter {

//public static final int CHECK_CONTEXT_ITEM_PRESENT = 1;
  val CHECK_CONTEXT_ITEM_IS_NODE: Int = 2

}

class AttributeGetter(@BeanProperty var attributeName: FingerprintedQName)
    extends Expression {

  @BeanProperty
  var requiredChecks: Int = CHECK_CONTEXT_ITEM_IS_NODE

  override def getItemType: ItemType = BuiltInAtomicType.UNTYPED_ATOMIC

  override def computeCardinality(): Int = StaticProperty.ALLOWS_ZERO_OR_ONE

   override def computeSpecialProperties(): Int =
    StaticProperty.NO_NODES_NEWLY_CREATED

  override def getIntrinsicDependencies: Int =
    StaticProperty.DEPENDS_ON_CONTEXT_ITEM

  override def copy(rebindings: RebindingMap): AttributeGetter = {
    val ag2: AttributeGetter = new AttributeGetter(attributeName)
    ag2.setRequiredChecks(requiredChecks)
    ag2
  }

  override def addToPathMap(
      pathMap: PathMap,
      pathMapNodeSet: PathMap.PathMapNodeSet): PathMap.PathMapNodeSet = {
    var pmnSet = pathMapNodeSet
    if (pathMapNodeSet == null) {
      val cie: ContextItemExpression = new ContextItemExpression()
      pmnSet = new PathMap.PathMapNodeSet(pathMap.makeNewRoot(cie))
    }
    pathMapNodeSet.createArc(AxisInfo.ATTRIBUTE,
                             new NameTest(Type.ATTRIBUTE,
                                          attributeName,
                                          getConfiguration.getNamePool))
  }

  override def getImplementationMethod: Int = Expression.EVALUATE_METHOD

  override def evaluateItem(context: XPathContext): Item = {
    val item: Item = context.getContextItem
    if (item.isInstanceOf[TinyElementImpl]) {
// fast path
      val `val`: String = item
        .asInstanceOf[TinyElementImpl]
        .getAttributeValue(attributeName.getFingerprint)
      if (`val` == null) return null else return new UntypedAtomicValue(`val`)
    }
    if (item == null) {
// This doesn't actually happen, we don't create an AttributeGetter unless we know statically
      dynamicError("The context item for @" + attributeName.getDisplayName +
                     " is absent",
                   "XPDY0002",
                   context)
    }
    if (!(item.isInstanceOf[NodeInfo])) {
      typeError("The context item for @" + attributeName.getDisplayName +
                  " is not a node",
                "XPDY0002",
                context)
    }
    assert(item.isInstanceOf[NodeInfo])
    val node: NodeInfo = item.asInstanceOf[NodeInfo]
    if (node.getNodeKind == Type.ELEMENT) {
      val `val`: String = node.getAttributeValue(attributeName.getURI,
                                                 attributeName.getLocalPart)
      if (`val` == null) null else new UntypedAtomicValue(`val`)
    } else {
      null
    }
  }

  override def getExpressionName: String = "attGetter"

  override def toShortString: String = "@" + attributeName.getDisplayName

  override def toString: String =
    "data(@" + attributeName.getDisplayName + ")"

  override def equals(obj: Any): Boolean = obj match {
    case obj: AttributeGetter => obj.attributeName == attributeName
    case _ => false

  }

  override def computeHashCode(): Int = 83571 ^ attributeName.hashCode

  override def export(out: ExpressionPresenter): Unit = {
    out.startElement("attVal", this)
    out.emitAttribute("name", attributeName.getStructuredQName)
    out.emitAttribute("chk", "" + requiredChecks)
    out.endElement()
  }

}

// Copyright (c) 2018-2020 Saxonica Limited
// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is "Incompatible With Secondary Licenses", as defined by the Mozilla Public License, v. 2.0.
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
/**
  * An AttributeGetter is an expression that returns the value of a specific attribute
  * of the context item, provided that it is an untyped element node. That is,
  * it represents the expression data(./@name) assuming that all data is untyped.
  */
