////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
package org.orbeon.saxon.expr.instruct

import org.orbeon.saxon.expr.ContextOriginator




trait ITemplateCall extends ContextOriginator {

  /*@NotNull*/

  def getActualParams: Array[WithParam]

  /*@NotNull*/

  def getTunnelParams: Array[WithParam]

}

// Copyright (c) 2018-2020 Saxonica Limited
// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is "Incompatible With Secondary Licenses", as defined by the Mozilla Public License, v. 2.0.
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
/**
  * An interface satisfied by all instructions that invoke templates: apply-templates,
  * call-template. apply-imports, next-match
  */
