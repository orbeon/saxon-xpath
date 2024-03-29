////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// Copyright (c) 2018-2020 Saxonica Limited
// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is "Incompatible With Secondary Licenses", as defined by the Mozilla Public License, v. 2.0.
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
package org.orbeon.saxon.trans.rules

import org.orbeon.saxon.event.Outputter
import org.orbeon.saxon.expr.XPathContext
import org.orbeon.saxon.expr.instruct.ParameterSet
import org.orbeon.saxon.om.{Item, NodeInfo}
import org.orbeon.saxon.s9api.Location
import org.orbeon.saxon.trans.XPathException
import org.orbeon.saxon.tree.util.Navigator


object FailRuleSet {
  val getInstance: FailRuleSet = new FailRuleSet
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
      item match {
        case info: NodeInfo => "the node " + Navigator.getPath(info)
        case _ => "the atomic value " + item.getStringValue
      }
    val err = new XPathException(
      "No user-defined template rule matches " + id,
      "XTDE0555")
    err.setLocator(locationId.saveLocation)
    throw err
  }

  def getName: String = "fail"

  /**
    * Get the default action for unmatched nodes
    *
    * @param nodeKind the node kind
    * @return the default action for unmatched element nodes: one of DEEP_COPY, APPLY_TEMPLATES, DEEP_SKIP, FAIL
    */
  def getActionForParentNodes(nodeKind: Int): Array[Int] = Array(FAIL)
}
