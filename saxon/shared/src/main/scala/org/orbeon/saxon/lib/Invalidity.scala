////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
package org.orbeon.saxon.lib

import org.orbeon.saxon.om.AbsolutePath

import org.orbeon.saxon.om.NodeInfo

import javax.xml.transform.SourceLocator




/**
  * An {@code Invalidity} is a validation error, that is, a failure of the instance document
  * to conform to the schema being used for validation.
  *
  * <p>The class extends {@link SourceLocator}, which makes location information available (for example,
  * a line and column number). This information will generally be the location of the error as it appears
  * in error reports intended for a human reader. It is not necessarily the same as the location of the
  * invalid node. For example, if the schema type for element A does not allow a child element named B,
  * but in the instance document there is an A element with a B child, then the location information
  * may relate to element B, whereas it is A that is invalid.</p>
  */
trait Invalidity extends SourceLocator {

  def getSchemaPart: Int

  def getConstraintName: String

  def getConstraintClauseNumber: String

  /**
    * Get the constraint name and clause in the format defined in XML Schema Part 1 Appendix C (Outcome Tabulations).
    * This mandates the format validation-rule-name.clause-number
    *
    * @return the constraint reference, for example "cos-ct-extends.1.2"; or null if the reference
    * is not known.
    */
  def getConstraintReference: String

  def getInvalidNode: NodeInfo

  def getPath: AbsolutePath

  def getContextPath: AbsolutePath

  def getMessage: String

  def getErrorCode: String

}

// Copyright (c) 2018-2020 Saxonica Limited
// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is "Incompatible With Secondary Licenses", as defined by the Mozilla Public License, v. 2.0.
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
