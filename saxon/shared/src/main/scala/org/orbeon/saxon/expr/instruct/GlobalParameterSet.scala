////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
package org.orbeon.saxon.expr.instruct

import org.orbeon.saxon.utils.Configuration

import org.orbeon.saxon.expr.XPathContext

import org.orbeon.saxon.expr.parser.Loc

import org.orbeon.saxon.expr.parser.RoleDiagnostic

import org.orbeon.saxon.expr.parser.TypeChecker

import org.orbeon.saxon.om.GroundedValue

import org.orbeon.saxon.om.Sequence

import org.orbeon.saxon.om.StructuredQName

import org.orbeon.saxon.trans.XPathException

import org.orbeon.saxon.value.SequenceType

import java.util.Collection

import java.util.HashMap


class GlobalParameterSet {

  private var params: HashMap[StructuredQName, GroundedValue] = new HashMap(10)

  def this(parameterSet: GlobalParameterSet) = {
    this()
    this.params = new HashMap(parameterSet.params)
  }

  def put(qName: StructuredQName, value: GroundedValue): Unit = {
    if (value == null) {
      params.remove(qName)
    } else {
      params.put(qName, value)
    }
  }

  def get(qName: StructuredQName): GroundedValue = params.get(qName)

  def containsKey(qName: StructuredQName): Boolean = params.containsKey(qName)

  def convertParameterValue(qName: StructuredQName,
                            requiredType: SequenceType,
                            convert: Boolean,
                            context: XPathContext): GroundedValue = {
    var `val`: Sequence = get(qName)
    if (`val` == null) {
      return null
    }
    if (requiredType != null) {
      if (convert) {
        val role: RoleDiagnostic =
          new RoleDiagnostic(RoleDiagnostic.VARIABLE, qName.getDisplayName, -1)
        val config: Configuration = context.getConfiguration
        `val` = config.getTypeHierarchy.applyFunctionConversionRules(
          `val`,
          requiredType,
          role,
          Loc.NONE)
      } else {
        val err =
          TypeChecker.testConformance(`val`, requiredType, context)
        if (err != null) {
          throw err
        }
      }
    }
    `val`.materialize
  }

  def clear(): Unit = {
    params.clear()
  }

  def getKeys: Collection[StructuredQName] = params.keySet

  def getNumberOfKeys: Int = params.size

}

// Copyright (c) 2018-2020 Saxonica Limited
// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is "Incompatible With Secondary Licenses", as defined by the Mozilla Public License, v. 2.0.
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
/**
 * A GlobalParameterSet is a set of parameters supplied when invoking a stylesheet or query.
 * It is a collection of name-value pairs, the names being represented by StructuredQName objects.
 * The values are objects, as supplied by the caller: conversion of the object
 * to a required type takes place when the parameter is actually used.
 */
