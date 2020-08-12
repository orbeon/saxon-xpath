////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
package net.sf.saxon.trans.rules

import net.sf.saxon.event.Outputter

import net.sf.saxon.expr.XPathContext

import net.sf.saxon.expr.instruct.ParameterSet

import net.sf.saxon.om.Item

import net.sf.saxon.om.NodeInfo

import net.sf.saxon.s9api.Location

import net.sf.saxon.trans.XPathException

import net.sf.saxon.trans.XmlProcessingIncident

import net.sf.saxon.tree.util.Navigator

import scala.beans.{BeanProperty, BooleanBeanProperty}




/**
  * A set of built-in template rules that performs the same action as an underlying set of rules,
  * but with the addition of a warning message saying that no user-defined template rules was found.
  * <p>XSLT 3.0 feature</p>
  */
class RuleSetWithWarnings(@BeanProperty var baseRuleSet: BuiltInRuleSet)
    extends BuiltInRuleSet {

  def process(item: Item,
              parameters: ParameterSet,
              tunnelParams: ParameterSet,
              output: Outputter,
              context: XPathContext,
              locationId: Location): Unit = {
    outputWarning(item, context)
    baseRuleSet.process(item,
                        parameters,
                        tunnelParams,
                        output,
                        context,
                        locationId)
  }

  override def getName(): String = baseRuleSet + " with warnings"

  def outputWarning(item: Item, context: XPathContext): Unit = {
    val id: String =
      if (item.isInstanceOf[NodeInfo])
        "the node " + Navigator.getPath(item.asInstanceOf[NodeInfo])
      else "the atomic value " + item.getStringValue
    val warning: XmlProcessingIncident =
      new XmlProcessingIncident("No user-defined template rule matches " + id,
                                "XTDE0555").asWarning()
    context.getController.getErrorReporter.report(warning)
  }

  /**
    * Get the default action for unmatched nodes
    *
    * @param nodeKind the node kind
    * @return the default action for unmatched element nodes: one of DEEP_COPY, APPLY_TEMPLATES, DEEP_SKIP, FAIL
    */
  def getActionForParentNodes(nodeKind: Int): Array[Int] =
    baseRuleSet.getActionForParentNodes(nodeKind)

}

// Copyright (c) 2018-2020 Saxonica Limited
// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is "Incompatible With Secondary Licenses", as defined by the Mozilla Public License, v. 2.0.
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
