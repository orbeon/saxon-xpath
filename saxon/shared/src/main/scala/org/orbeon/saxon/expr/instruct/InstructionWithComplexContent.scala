////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
package org.orbeon.saxon.expr.instruct

import org.orbeon.saxon.expr.Expression




/**
  * Interface implemented by instructions that contain a sequence constructor evaluated
  * using the rules for "constructing complex content": for example, xsl:element, xsl:copy,
  * xsl:document, xsl:result-document
  */
trait InstructionWithComplexContent {

  def getContentExpression: Expression

}

// Copyright (c) 2018-2020 Saxonica Limited
// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is "Incompatible With Secondary Licenses", as defined by the Mozilla Public License, v. 2.0.
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
