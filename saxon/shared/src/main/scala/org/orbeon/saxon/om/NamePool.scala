////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// Copyright (c) 2018-2020 Saxonica Limited
// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is "Incompatible With Secondary Licenses", as defined by the Mozilla Public License, v. 2.0.
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
package org.orbeon.saxon.om

import org.orbeon.saxon.lib.NamespaceConstant

import java.util.concurrent.ConcurrentHashMap

import java.util.concurrent.atomic.AtomicInteger

import NamePool._


/**
 * A NamePool holds a collection of expanded names, each containing a namespace URI,
 * and a local name.
 *
 * Each expanded name is allocated a unique 20-bit fingerprint. The fingerprint enables
 * the URI and the local name to be determined. Some subsystems (notably the Tinytree) use
 * the top 10 bits to represent the prefix, but the NamePool is no longer concerned with
 * managing prefixes, and prefixes do not have global codes.
 *
 * The NamePool has been redesigned in Saxon 9.7 to make use of two Java
 * ConcurrentHashMaps, one from QNames to integers, one from integers to QNames.
 * This gives better scaleability in terms of multithreaded concurrency and in terms
 * of the capacity of the NamePool and retention of performance as the size of
 * the vocabulary increases.
 *
 * Fingerprints in the range 0 to 1023 are reserved for system use, and are allocated as constants
 * mainly to names in the XSLT and XML Schema namespaces: constants representing these names
 * are found in `StandardNames`.
 *
 * The fingerprint -1 is reserved to mean "not known" or inapplicable.
 *
 * Modified in 9.4 to remove namespace codes.
 *
 * Modified in 9.7 to remove URI codes.
 *
 * Modified in 9.8 to remove namecodes and all handling of prefixes.
 */
object NamePool {

  /**
   * FP_MASK is a mask used to obtain a fingerprint from a nameCode. Given a
   * nameCode nc, the fingerprint is <code>nc &amp; NamePool.FP_MASK</code>.
   * (In practice, Saxon code often uses the literal constant 0xfffff,
   * to extract the bottom 20 bits).\
   *
   * The difference between a fingerprint and a nameCode is that
   * a nameCode contains information
   * about the prefix of a name, the fingerprint depends only on the namespace
   * URI and local name. Note that the "null" nameCode (-1) does not produce
   * the "null" fingerprint (also -1) when this mask is applied.
   */
  val FP_MASK = 0xfffff

  val USER_DEFINED_MASK: Int = 0xffc00
  private val MAX_FINGERPRINT: Int = FP_MASK

  def isPrefixed(nameCode: Int): Boolean = (nameCode & 0x3ff00000) != 0

  /**
   * Unchecked Exception raised when some limit in the design of the name pool is exceeded
   */
  class NamePoolLimitException(message: String)
    extends RuntimeException(message)
}

class NamePool {

  private val qNameToInteger   : ConcurrentHashMap[StructuredQName, Integer] = new ConcurrentHashMap(1000)
  private val integerToQName   : ConcurrentHashMap[Integer, StructuredQName] = new ConcurrentHashMap(1000)
  private val unique           : AtomicInteger                               = new AtomicInteger(1024)
  private val suggestedPrefixes: ConcurrentHashMap[String, String]           = new ConcurrentHashMap

  def suggestPrefix(prefix: String, uri: String): Unit =
    suggestedPrefixes.put(uri, prefix)

  def getUnprefixedQName(nameCode: Int): StructuredQName = {
    val fp = nameCode & FP_MASK
    if ((fp & USER_DEFINED_MASK) == 0)
      StandardNames.getUnprefixedQName(fp)
    else
      integerToQName.get(fp)
  }

  def getStructuredQName(fingerprint: Int): StructuredQName =
    getUnprefixedQName(fingerprint)

  def suggestPrefixForURI(uri: String): String =
    if (uri == NamespaceConstant.XML)
      "xml"
    else
      suggestedPrefixes.get(uri)

  def allocateFingerprint(uri: String, local: String): Int =
    synchronized {
      if (NamespaceConstant.isReserved(uri) || NamespaceConstant.SAXON == uri) {
        val fp = StandardNames.getFingerprint(uri, local)
        if (fp != -1)
          return fp
      }

      val qName = new StructuredQName("", uri, local)
      var existing = qNameToInteger.get(qName)
      if (existing != null)
        return existing

      val next = unique.getAndIncrement
      if (next > MAX_FINGERPRINT)
        throw new NamePoolLimitException("Too many distinct names in NamePool")

  //    existing = qNameToInteger.putIfAbsent(qName, next)
      existing = qNameToInteger.get(qName)

      if (existing == null) {
        qNameToInteger.put(qName, next)
        integerToQName.put(next, qName)
        next
      } else {
        existing
      }
    }

  /*@NotNull*/
  def getURI(nameCode: Int): String = {
    val fp = nameCode & FP_MASK
    if ((fp & USER_DEFINED_MASK) == 0)
      return StandardNames.getURI(fp)
    getUnprefixedQName(fp).getURI
  }

  def getLocalName(nameCode: Int): String =
    getUnprefixedQName(nameCode).getLocalPart

  def getDisplayName(nameCode: Int): String =
    getStructuredQName(nameCode).getDisplayName

  def getClarkName(nameCode: Int): String =
    getUnprefixedQName(nameCode).getClarkName

  def getEQName(nameCode: Int): String = getUnprefixedQName(nameCode).getEQName

  def allocateClarkName(expandedName: String): Int = {
    var namespace: String = null
    var localName: String = null
    if (expandedName.charAt(0) == '{') {
      val closeBrace = expandedName.indexOf('}')
      if (closeBrace < 0)
        throw new IllegalArgumentException("No closing '}' in Clark name")
      namespace = expandedName.substring(1, closeBrace)
      if (closeBrace == expandedName.length)
        throw new IllegalArgumentException("Missing local part in Clark name")
      localName = expandedName.substring(closeBrace + 1)
    } else {
      namespace = ""
      localName = expandedName
    }
    allocateFingerprint(namespace, localName)
  }

  // A read-only version of allocate()
  def getFingerprint(uri: String, localName: String): Int = {
    if (NamespaceConstant.isReserved(uri) || uri == NamespaceConstant.SAXON) {
      val fp = StandardNames.getFingerprint(uri, localName)
      if (fp != -1)
        return fp
      // otherwise, look for the name in this namepool
    }
    val fp = qNameToInteger.get(new StructuredQName("", uri, localName))
    if (fp == null) -1 else fp
  }
}
