////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
package net.sf.saxon.expr.flwor

import net.sf.saxon.event.Outputter

import net.sf.saxon.expr.XPathContext

import net.sf.saxon.trans.XPathException

import scala.beans.{BeanProperty, BooleanBeanProperty}




/**
  * Abtract class representing a tuple stream (used to evaluate a FLWOR expression) in push mode
  * (where the provider of tuples activates the consumer of those tuples)
  */
abstract class TuplePush  (
    @BeanProperty  var outputter: Outputter) {

  def processTuple(context: XPathContext): Unit

  def close(): Unit = {}
// default implementation takes no action
// default implementation takes no action

}

// Copyright (c) 2018-2020 Saxonica Limited
// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is "Incompatible With Secondary Licenses", as defined by the Mozilla Public License, v. 2.0.
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
