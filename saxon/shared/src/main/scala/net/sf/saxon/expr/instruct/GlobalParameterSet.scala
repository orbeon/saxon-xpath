////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
package net.sf.saxon.expr.instruct

import net.sf.saxon.utils.Configuration

import net.sf.saxon.expr.XPathContext

import net.sf.saxon.expr.parser.Loc

import net.sf.saxon.expr.parser.RoleDiagnostic

import net.sf.saxon.expr.parser.TypeChecker

import net.sf.saxon.om.GroundedValue

import net.sf.saxon.om.Sequence

import net.sf.saxon.om.StructuredQName

import net.sf.saxon.trans.XPathException

import net.sf.saxon.value.SequenceType

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
      null
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
        val err: XPathException =
          TypeChecker.testConformance(`val`, requiredType, context)
        if (err != null) {
          throw err
        }
      }
    }
    `val`.materialize()
  }

  def clear(): Unit = {
    params.clear()
  }

  def getKeys(): Collection[StructuredQName] = params.keySet

  def getNumberOfKeys(): Int = params.size

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
