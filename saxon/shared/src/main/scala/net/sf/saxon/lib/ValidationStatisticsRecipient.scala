////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
package net.sf.saxon.lib

import net.sf.saxon.model.SchemaComponent

import net.sf.saxon.s9api.SaxonApiException

import java.util.Map




/**
  * Defines a class that is notified of validation statistics at the end of a validation episode
  */
trait ValidationStatisticsRecipient {

  def notifyValidationStatistics(
      statistics: Map[SchemaComponent, Integer]): Unit

}

// Copyright (c) 2018-2020 Saxonica Limited
// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is "Incompatible With Secondary Licenses", as defined by the Mozilla Public License, v. 2.0.
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
