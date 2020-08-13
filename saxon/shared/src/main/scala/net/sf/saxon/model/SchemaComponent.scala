////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// Copyright (c) 2018-2020 Saxonica Limited
// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is "Incompatible With Secondary Licenses", as defined by the Mozilla Public License, v. 2.0.
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
package net.sf.saxon.model

import net.sf.saxon.model.SchemaComponent.ValidationStatus.ValidationStatus


object SchemaComponent {

  object ValidationStatus extends Enumeration {

    val UNVALIDATED: ValidationStatus = new ValidationStatus()
    val FIXED_UP: ValidationStatus = new ValidationStatus()
    val VALIDATING: ValidationStatus = new ValidationStatus()
    val VALIDATED: ValidationStatus = new ValidationStatus()
    val INVALID: ValidationStatus = new ValidationStatus()
    val INCOMPLETE: ValidationStatus = new ValidationStatus()

    class ValidationStatus extends Val

    implicit def convertValue(v: Value): ValidationStatus =
      v.asInstanceOf[ValidationStatus]

  }

}

/**
  * This is a marker interface that represents any "schema component" as defined in the XML Schema
  * specification. This may be a user-defined schema component or a built-in schema component. Since
  * all built-in schema components are types, every SchemaComponent in practice is either a
  * {@link com.saxonica.ee.schema.UserSchemaComponent} or a {@link SchemaType} or both.
  */
trait SchemaComponent {

  def getValidationStatus(): ValidationStatus
  def getRedefinitionLevel(): Int

  // ORBEON: This is unused and causes unwanted recursion during initialization.
//  var COMPONENT_FUNCTION_TYPE: FunctionItemType = new SpecificFunctionType(
//    Array(SequenceType.SINGLE_STRING),
//    SequenceType.ANY_SEQUENCE)
}
