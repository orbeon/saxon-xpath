////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
package net.sf.saxon.lib




/**
  * This interface defines a method for checking whether a string is considered to be a valid URI.
  * <p>A user-supplied implementation of this class can be set in a customized instance of
  * {@link ConversionRules}, which can be set in the configuration using
  * {@link net.sf.saxon.Configuration#setConversionRules(ConversionRules)}</p>
  * <p>A user-supplied implementation can be written either from scratch, or by reference to the
  * system-supplied implementation {@link StandardURIChecker}.</p>
  */
trait URIChecker {

  def isValidURI(value: CharSequence): Boolean

}

// Copyright (c) 2018-2020 Saxonica Limited
// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is "Incompatible With Secondary Licenses", as defined by the Mozilla Public License, v. 2.0.
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
