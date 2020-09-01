////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// Copyright (c) 2018-2020 Saxonica Limited
// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is "Incompatible With Secondary Licenses", as defined by the Mozilla Public License, v. 2.0.
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

package net.sf.saxon.lib

import java.net.{URI, URISyntaxException}

import net.sf.saxon.expr.sort.LRUCache
import net.sf.saxon.functions.IriToUri
import net.sf.saxon.lib.StandardURIChecker._
import net.sf.saxon.value.Whitespace


object StandardURIChecker {

  private val THE_INSTANCE: StandardURIChecker = new StandardURIChecker()

  def getInstance: StandardURIChecker = THE_INSTANCE

  /*@NotNull*/

  private val caches: ThreadLocal[LRUCache[CharSequence, Boolean]] =
    new ThreadLocal[LRUCache[CharSequence, Boolean]]()

  def main(args: Array[String]): Unit = {
    System.err.println(
      args(0) + " is valid? - " + getInstance.isValidURI(args(0)))
  }
}

/**
  * This class checks whether a string is a valid URI. Different checking rules can be chosen by including
  * a different URIChecker in the {@link ConversionRules} used when the value is checked.
  */
class StandardURIChecker extends URIChecker {

  def isValidURI(value: CharSequence): Boolean = {
    var cache: LRUCache[CharSequence, Boolean] = caches.get
    if (cache == null) {
      cache = new LRUCache[CharSequence, Boolean](50)
      caches.set(cache)
    }
    if (cache.get(value) != null) {
      return true
    }
    var sv: String = Whitespace.trim(value)
    // Allow zero-length strings (RFC2396 is ambivalent on this point)
    if (sv.isEmpty) {
      return true
    }
    // Allow a string if the java.net.URI class accepts it
    try {
      new URI(sv)
      cache.put(value, true)
      return true
    } catch {
      case _: URISyntaxException =>
        // keep trying
        // Note: it's expensive to throw exceptions on a success path, so we keep a cache.

    }
    // Allow a string if it can be escaped into a form that java.net.URI accepts
    sv = IriToUri.iriToUri(sv).toString
    try {
      new URI(sv)
      cache.put(value, true)
      true
    } catch {
      case _: URISyntaxException =>
        false
    }
  }

}

