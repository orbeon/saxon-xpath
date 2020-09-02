////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
package net.sf.saxon.trans

import net.sf.saxon.expr.instruct.Actor

import net.sf.saxon.om.StandardNames

import net.sf.saxon.pattern.LocalNameTest

import net.sf.saxon.pattern.NameTest

import net.sf.saxon.pattern.NamespaceTest

import net.sf.saxon.pattern.QNameTest

import scala.beans.{BeanProperty, BooleanBeanProperty}




class ComponentTest(@BeanProperty var componentKind: Int,
                    private var nameTest: QNameTest,
                    @BeanProperty var arity: Int) {

  def getQNameTest: QNameTest = nameTest

  def isPartialWildcard: Boolean =
    nameTest.isInstanceOf[LocalNameTest] || nameTest
      .isInstanceOf[NamespaceTest]

  def matches(component: Actor): Boolean = matches(component.getSymbolicName)

  def matches(sn: SymbolicName): Boolean =
    (componentKind == -1 || sn.getComponentKind == componentKind) &&
      nameTest.matches(sn.getComponentName) &&
      !((componentKind == StandardNames.XSL_FUNCTION) && arity != -1 &&
        arity != sn.asInstanceOf[SymbolicName.F].getArity)

  def getSymbolicNameIfExplicit: SymbolicName =
    if (nameTest.isInstanceOf[NameTest]) {
      if (componentKind == StandardNames.XSL_FUNCTION) {
        new SymbolicName.F(nameTest.asInstanceOf[NameTest].getMatchingNodeName,
                           arity)
      } else {
        new SymbolicName(componentKind,
                         nameTest.asInstanceOf[NameTest].getMatchingNodeName)
      }
    } else {
      null
    }

  override def equals(other: Any): Boolean =
    other.isInstanceOf[ComponentTest] &&
      other.asInstanceOf[ComponentTest].componentKind == componentKind &&
      other.asInstanceOf[ComponentTest].arity == arity &&
      other.asInstanceOf[ComponentTest].nameTest == nameTest

  override def hashCode: Int = componentKind ^ arity ^ nameTest.hashCode

}

// Copyright (c) 2018-2020 Saxonica Limited
// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is "Incompatible With Secondary Licenses", as defined by the Mozilla Public License, v. 2.0.
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
