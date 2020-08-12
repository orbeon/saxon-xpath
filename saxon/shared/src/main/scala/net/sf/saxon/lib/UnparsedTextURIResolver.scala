////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
package net.sf.saxon.lib

import net.sf.saxon.utils.Configuration

import net.sf.saxon.trans.XPathException

import java.io.Reader

import java.net.URI




trait UnparsedTextURIResolver {

  /*@NotNull*/

  def resolve(absoluteURI: URI,
              encoding: String,
              config: Configuration): Reader

}

// Copyright (c) 2018-2020 Saxonica Limited
// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is "Incompatible With Secondary Licenses", as defined by the Mozilla Public License, v. 2.0.
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
/**
  * An UnparsedTextURIResolver accepts an absolute URI and optionally an encoding name as input,
  * and returns a Reader as its result.
  */
