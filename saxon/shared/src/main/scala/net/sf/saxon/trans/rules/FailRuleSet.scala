////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
package net.sf.saxon.trans.rules

import net.sf.saxon.event.Outputter

import net.sf.saxon.expr.XPathContext

import net.sf.saxon.expr.instruct.ParameterSet

import net.sf.saxon.om.Item

import net.sf.saxon.om.NodeInfo

import net.sf.saxon.s9api.Location

import net.sf.saxon.trans.XPathException

import net.sf.saxon.tree.util.Navigator

import FailRuleSet._




object FailRuleSet {

  private var THE_INSTANCE: FailRuleSet = new FailRuleSet()

  def getInstance(): FailRuleSet = THE_INSTANCE

}

/**
  * The built-in rule set introduced in XSLT 3.0, which raises an error when there is no user-supplied
  * template rule that matches a node.
  */
class FailRuleSet private () extends BuiltInRuleSet {

  def process(item: Item,
              parameters: ParameterSet,
              tunnelParams: ParameterSet,
              output: Outputter,
              context: XPathContext,
              locationId: Location): Unit = {
    val id: String =
      (if (item.isInstanceOf[NodeInfo])
         "the node " + Navigator.getPath(item.asInstanceOf[NodeInfo])
       else "the atomic value " + item.getStringValue)
    val err = new XPathException(
      "No user-defined template rule matches " + id,
      "XTDE0555")
    err.setLocator(locationId.saveLocation())
    throw err
  }

  override def getName(): String = "fail"

  /**
    * Get the default action for unmatched nodes
    *
    * @param nodeKind the node kind
    * @return the default action for unmatched element nodes: one of DEEP_COPY, APPLY_TEMPLATES, DEEP_SKIP, FAIL
    */
  def getActionForParentNodes(nodeKind: Int): Array[Int] = Array(FAIL)

}

// Copyright (c) 2018-2020 Saxonica Limited
// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is "Incompatible With Secondary Licenses", as defined by the Mozilla Public License, v. 2.0.
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
