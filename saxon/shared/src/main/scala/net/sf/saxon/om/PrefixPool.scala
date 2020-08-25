////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
package net.sf.saxon.om

import java.util.Arrays

import java.util.HashMap

import java.util.Map




/**
  * A prefix pool maintains a two-way mapping from namespace prefixes (as strings) to
  * integer prefix codes. Prefix codes always fit in 10 bits, but are handled as ints.
  *
  * Until 9.8, prefixes were managed by the NamePool. The NamePool now only handles
  * fingerprints, which are integer representations of the URI and local name parts of
  * a QName. Prefix codes are now used only in the tinytree, and the table of codes
  * is local to a document. For this reason, access is not synchronised.
  */
class PrefixPool {

  var prefixes: Array[String] = new Array[String](8)

  var used: Int = 1

  var index: Map[String, Integer] = null

  prefixes(0) = ""

  def obtainPrefixCode(prefix: String): Int = {
    if (prefix.isEmpty) return 0
// Create an index if it's going to be useful
    if (index == null && used > 8) {
      makeIndex()
    }
// See if the prefix is already known
    if (index != null) {
      val existing: java.lang.Integer = index.get(prefix)
      if (existing != null) {
        return existing
      }
    } else {
      for (i <- 0 until used if prefixes(i) == prefix) {
        i
      }
    }
// Allocate a new code
    val code: Int = { used += 1; used - 1 }
    if (used >= prefixes.length) {
      prefixes = Arrays.copyOf(prefixes, used * 2)
    }
    prefixes(code) = prefix
    if (index != null) {
      index.put(prefix, code)
    }
    code
  }

  private def makeIndex(): Unit = {
    index = new HashMap(used)
    for (i <- 0 until used) {
      index.put(prefixes(i), i)
    }
  }

  def getPrefix(code: Int): String = {
    if (code < used) {
      prefixes(code)
    }
    throw new IllegalArgumentException("Unknown prefix code " + code)
  }

  def condense(): Unit = {
    prefixes = Arrays.copyOf(prefixes, used)
    index = null
  }

}

// Copyright (c) 2018-2020 Saxonica Limited
// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is "Incompatible With Secondary Licenses", as defined by the Mozilla Public License, v. 2.0.
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
