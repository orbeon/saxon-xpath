////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
package org.orbeon.saxon.expr.compat

import org.orbeon.saxon.utils.Configuration

import org.orbeon.saxon.expr.AtomicSequenceConverter

import org.orbeon.saxon.expr.Atomizer

import org.orbeon.saxon.expr.Expression

import org.orbeon.saxon.expr.FirstItemExpression

import org.orbeon.saxon.expr.parser.ExpressionVisitor

import org.orbeon.saxon.expr.parser.RetainedStaticContext

import org.orbeon.saxon.expr.parser.RoleDiagnostic

import org.orbeon.saxon.expr.parser.TypeChecker

import org.orbeon.saxon.functions.SystemFunction

import org.orbeon.saxon.model.BuiltInAtomicType

import org.orbeon.saxon.model.ItemType

import org.orbeon.saxon.model.NumericType

import org.orbeon.saxon.model.TypeHierarchy

import org.orbeon.saxon.trans.XPathException

import org.orbeon.saxon.value.Cardinality

import org.orbeon.saxon.value.SequenceType




class TypeChecker10 extends TypeChecker {

  override def staticTypeCheck(supplied: Expression,
                      req: SequenceType,
                      role: RoleDiagnostic,
                      visitor: ExpressionVisitor): Expression = {
    var suppliedExp = supplied
    if (suppliedExp.implementsStaticTypeCheck()) {
      return suppliedExp.staticTypeCheck(req, backwardsCompatible = true, role, visitor)
    }
    val config: Configuration = visitor.getConfiguration
    val th = config.getTypeHierarchy
// rule 1
    if (!Cardinality.allowsMany(req.getCardinality) && Cardinality.allowsMany(
      suppliedExp.getCardinality)) {
      val cexp: Expression =
        FirstItemExpression.makeFirstItemExpression(suppliedExp)
      cexp.adoptChildExpression(suppliedExp)
      suppliedExp = cexp
    }
// rule 2
    val reqItemType: ItemType = req.getPrimaryType
    if (req.getPrimaryType == BuiltInAtomicType.STRING && !Cardinality
          .allowsMany(req.getCardinality) &&
        !th.isSubType(suppliedExp.getItemType, BuiltInAtomicType.STRING)) {
      val rsc: RetainedStaticContext = new RetainedStaticContext(config)
      val fn: Expression = SystemFunction.makeCall("string", rsc, suppliedExp)
      try return fn.typeCheck(visitor, config.getDefaultContextItemStaticInfo)
      catch {
        case err: XPathException => {
          err.maybeSetLocation(suppliedExp.getLocation)
          err.setIsStaticError(true)
          throw err
        }

      }
    }
// rule 3
    if (reqItemType == NumericType.getInstance ||
        reqItemType == BuiltInAtomicType.DOUBLE && !Cardinality.allowsMany(
          req.getCardinality) &&
        !th.isSubType(suppliedExp.getItemType, BuiltInAtomicType.DOUBLE)) {
      val rsc: RetainedStaticContext = new RetainedStaticContext(config)
      val fn: Expression = SystemFunction.makeCall("number", rsc, suppliedExp)
      try return fn.typeCheck(visitor, config.getDefaultContextItemStaticInfo)
      catch {
        case err: XPathException => {
          err.maybeSetLocation(suppliedExp.getLocation)
          err.setIsStaticError(true)
          throw err
        }

      }
    }
    super.staticTypeCheck(suppliedExp, req, role, visitor)
  }
//        In a static function call, if XPath 1.0 compatibility mode is true and an argument of a static function is
//        not of the expected type, then the following conversions are applied sequentially to the argument value V:
//        (1) If the expected type calls for a single item or optional single item(examples:xs:
//        string, xs:string ?, xs:untypedAtomic, xs:untypedAtomic ?, node(), node() ?, item(), item() ?),then the value V
//        is effectively replaced by V[1].
//        (2) If the expected type is xs:string or xs:string?, then the value V is effectively replaced by fn:string(V).
//        (3) If the expected type is xs:double or xs:double?,then the value V is effectively replaced by fn:number(V).
//        We interpret this as including xs:numeric so that the intended effect is achieved with functions such as fn:floor().
//        In a static function call, if XPath 1.0 compatibility mode is true and an argument of a static function is
//        not of the expected type, then the following conversions are applied sequentially to the argument value V:
//        (1) If the expected type calls for a single item or optional single item(examples:xs:
//        string, xs:string ?, xs:untypedAtomic, xs:untypedAtomic ?, node(), node() ?, item(), item() ?),then the value V
//        is effectively replaced by V[1].
//        (2) If the expected type is xs:string or xs:string?, then the value V is effectively replaced by fn:string(V).
//        (3) If the expected type is xs:double or xs:double?,then the value V is effectively replaced by fn:number(V).
//        We interpret this as including xs:numeric so that the intended effect is achieved with functions such as fn:floor().

  override def makeArithmeticExpression(lhs: Expression,
                               operator: Int,
                               rhs: Expression): Expression =
    new ArithmeticExpression10(lhs, operator, rhs)

  override def makeGeneralComparison(lhs: Expression,
                            operator: Int,
                            rhs: Expression): Expression =
    new GeneralComparison10(lhs, operator, rhs)

  override def processValueOf(select: Expression, config: Configuration): Expression = {
    var selExp = select
    val th = config.getTypeHierarchy
    if (!selExp.getItemType.isPlainType) {
      selExp = Atomizer.makeAtomizer(selExp, null)
    }
    if (Cardinality.allowsMany(selExp.getCardinality)) {
      selExp = FirstItemExpression.makeFirstItemExpression(selExp)
    }
    if (!th.isSubType(selExp.getItemType, BuiltInAtomicType.STRING)) {
      selExp = new AtomicSequenceConverter(selExp, BuiltInAtomicType.STRING)
      selExp
        .asInstanceOf[AtomicSequenceConverter]
        .allocateConverterStatically(config, allowNull = false)
    }
    selExp
  }

}

// Copyright (c) 2018-2020 Saxonica Limited
// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is "Incompatible With Secondary Licenses", as defined by the Mozilla Public License, v. 2.0.
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
/**
  * This class provides type checking capability with XPath 1.0 backwards compatibility enabled.
  */
