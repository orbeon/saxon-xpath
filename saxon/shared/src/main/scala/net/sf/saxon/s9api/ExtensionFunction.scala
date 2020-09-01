////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
package net.sf.saxon.s9api




/**
  * This is an interface for simple external/extension functions. Users can implement this
  * interface and register the implementation with the {@link net.sf.saxon.s9api.Processor}; the function will
  * then be available for calling from all queries, stylesheets, and XPath expressions compiled
  * under this Processor.
  * <p>Extension functions implemented using this interface are expected to be free of side-effects,
  * and to have no dependencies on the static or dynamic context. A richer interface for extension
  * functions is provided via the {@link net.sf.saxon.lib.ExtensionFunctionDefinition} class.</p>
  */
trait ExtensionFunction {

  /**
    * Return the name of the external function
    *
    * @return the name of the function, as a QName.
    */
  def getName: QName

  def getResultType: SequenceType = SequenceType.ANY

  def getArgumentTypes: Array[SequenceType]

  /*@NotNull*/

  def call(arguments: Array[XdmValue]): XdmValue

}

// Copyright (c) 2018-2020 Saxonica Limited
// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is "Incompatible With Secondary Licenses", as defined by the Mozilla Public License, v. 2.0.
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
