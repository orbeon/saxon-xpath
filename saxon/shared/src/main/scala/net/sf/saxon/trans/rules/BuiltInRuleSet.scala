////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
package net.sf.saxon.trans.rules

import net.sf.saxon.event.Outputter

import net.sf.saxon.expr.ContextOriginator

import net.sf.saxon.expr.XPathContext

import net.sf.saxon.expr.instruct.ParameterSet

import net.sf.saxon.om.Item

import net.sf.saxon.s9api.Location

import net.sf.saxon.trans.XPathException




/**
  * Defines a set of built-in template rules (rules for use when no user-defined template
  * rules match a given node)
  */
trait BuiltInRuleSet extends ContextOriginator {

  def process(item: Item,
              parameters: ParameterSet,
              tunnelParams: ParameterSet,
              output: Outputter,
              context: XPathContext,
              locationId: Location): Unit

  def getName(): String

  def getActionForParentNodes(nodeKind: Int): Array[Int]

  var DEEP_COPY: Int = 1

  var DEEP_SKIP: Int = 3

  var FAIL: Int = 4

  var SHALLOW_COPY: Int = 5

  var APPLY_TEMPLATES_TO_ATTRIBUTES: Int = 6

  var APPLY_TEMPLATES_TO_CHILDREN: Int = 7

}

// Copyright (c) 2018-2020 Saxonica Limited
// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is "Incompatible With Secondary Licenses", as defined by the Mozilla Public License, v. 2.0.
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
