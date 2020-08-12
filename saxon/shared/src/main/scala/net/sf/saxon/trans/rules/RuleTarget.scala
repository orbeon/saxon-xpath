////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
package net.sf.saxon.trans.rules

import net.sf.saxon.trace.ExpressionPresenter

import net.sf.saxon.trans.XPathException




/**
  * The target of a rule, typically a TemplateRule.
  */
trait RuleTarget {

  def export(presenter: ExpressionPresenter): Unit

  def registerRule(rule: Rule): Unit

}

// Copyright (c) 2018-2020 Saxonica Limited
// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is "Incompatible With Secondary Licenses", as defined by the Mozilla Public License, v. 2.0.
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
