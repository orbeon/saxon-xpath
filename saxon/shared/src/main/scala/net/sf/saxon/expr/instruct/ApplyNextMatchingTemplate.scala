////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
package net.sf.saxon.expr.instruct

import net.sf.saxon.expr.ContextItemExpression

import net.sf.saxon.expr.Expression

import net.sf.saxon.expr.Operand

import net.sf.saxon.expr.StaticProperty

import net.sf.saxon.expr.parser.ContextItemStaticInfo

import net.sf.saxon.expr.parser.ExpressionVisitor

import net.sf.saxon.expr.parser.PathMap

import net.sf.saxon.trans.XPathException

import java.util.ArrayList

import java.util.List

import scala.beans.{BeanProperty, BooleanBeanProperty}


abstract class ApplyNextMatchingTemplate
  extends Instruction
    with ITemplateCall {

  @BeanProperty
  var actualParams: Array[WithParam] = _

  @BeanProperty
  var tunnelParams: Array[WithParam] = _

  override def getImplementationMethod: Int =
    super.getImplementationMethod | Expression.WATCH_METHOD

  override def operands: java.lang.Iterable[Operand] = {
    val operanda: List[Operand] =
      new ArrayList[Operand](actualParams.length + tunnelParams.length)
    WithParam.gatherOperands(this, actualParams, operanda)
    WithParam.gatherOperands(this, tunnelParams, operanda)
    operanda
  }

  /*@NotNull*/

  override def simplify(): Expression = {
    WithParam.simplify(getActualParams)
    WithParam.simplify(getTunnelParams)
    this
  }

  /*@NotNull*/

  override def typeCheck(visitor: ExpressionVisitor,
                         contextInfo: ContextItemStaticInfo): Expression = {
    WithParam.typeCheck(actualParams, visitor, contextInfo)
    WithParam.typeCheck(tunnelParams, visitor, contextInfo)
    this
  }

  /*@NotNull*/

  override def optimize(visitor: ExpressionVisitor,
                        contextInfo: ContextItemStaticInfo): Expression = {
    WithParam.optimize(visitor, actualParams, contextInfo)
    WithParam.optimize(visitor, tunnelParams, contextInfo)
    this
  }

  override def getIntrinsicDependencies: Int = StaticProperty.DEPENDS_ON_CONTEXT_ITEM

  override def mayCreateNewNodes(): Boolean = true

  override def addToPathMap(
                             pathMap: PathMap,
                             pathMapNodeSet: PathMap.PathMapNodeSet): PathMap.PathMapNodeSet = {
    var pnSet = pathMapNodeSet
    if (pnSet == null) {
      val cie: ContextItemExpression = new ContextItemExpression()
      //cie.setContainer(getContainer());
      pnSet = new PathMap.PathMapNodeSet(pathMap.makeNewRoot(cie))
    }
    pnSet.addDescendants()
    new PathMap.PathMapNodeSet(pathMap.makeNewRoot(this))
  }

  // This logic is assuming the mode is streamable (so called templates can't return streamed nodes)
  //PathMap.PathMapNodeSet result = super.addToPathMap(pathMap, pathMapNodeSet);
  // This logic is assuming the mode is streamable (so called templates can't return streamed nodes)
  //PathMap.PathMapNodeSet result = super.addToPathMap(pathMap, pathMapNodeSet);

}

// Copyright (c) 2018-2020 Saxonica Limited
// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is "Incompatible With Secondary Licenses", as defined by the Mozilla Public License, v. 2.0.
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
/**
 * An xsl:apply-imports or xsl:next-match element in the stylesheet.
 */
