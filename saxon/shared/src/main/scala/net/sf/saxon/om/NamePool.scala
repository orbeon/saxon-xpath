////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
package net.sf.saxon.om

import net.sf.saxon.lib.NamespaceConstant

import java.util.concurrent.ConcurrentHashMap

import java.util.concurrent.atomic.AtomicInteger

import NamePool._


object NamePool {

  val FP_MASK: Int = 0xfffff

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

  private val qNameToInteger: ConcurrentHashMap[StructuredQName, Integer] =
    new ConcurrentHashMap(1000)

  private val integerToQName: ConcurrentHashMap[Integer, StructuredQName] =
    new ConcurrentHashMap(1000)

  private var unique: AtomicInteger = new AtomicInteger(1024)

  private var suggestedPrefixes: ConcurrentHashMap[String, String] =
    new ConcurrentHashMap()

  def suggestPrefix(prefix: String, uri: String): Unit = {
    suggestedPrefixes.put(uri, prefix)
  }

  def getUnprefixedQName(nameCode: Int): StructuredQName = {
    val fp: Int = nameCode & FP_MASK
    if ((fp & USER_DEFINED_MASK) == 0) {
      StandardNames.getUnprefixedQName(fp)
    }
    integerToQName.get(fp)
  }

  def getStructuredQName(fingerprint: Int): StructuredQName =
    getUnprefixedQName(fingerprint)

  def suggestPrefixForURI(uri: String): String = {
    if (uri == NamespaceConstant.XML) {
      return "xml"
    }
    suggestedPrefixes.get(uri)
  }

  def allocateFingerprint(uri: String, local: String): Int = synchronized {
    if (NamespaceConstant.isReserved(uri) || NamespaceConstant.SAXON == uri) {
      val fp: Int = StandardNames.getFingerprint(uri, local)
      if (fp != -1) {
        return fp
      }
    }
    val qName: StructuredQName = new StructuredQName("", uri, local)
    var existing: java.lang.Integer = qNameToInteger.get(qName)
    if (existing != null) {
      return existing
    }
    val next: Int = unique.getAndIncrement
    if (next > MAX_FINGERPRINT) {
      throw new NamePoolLimitException("Too many distinct names in NamePool")
    }
    existing = qNameToInteger.putIfAbsent(qName, next)
    if (existing == null) {
      integerToQName.put(next, qName)
      next
    } else {
      existing
    }
  }

  /*@NotNull*/

  def getURI(nameCode: Int): String = {
    val fp: Int = nameCode & FP_MASK
    if ((fp & USER_DEFINED_MASK) == 0) {
      StandardNames.getURI(fp)
    }
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
      val closeBrace: Int = expandedName.indexOf('}')
      if (closeBrace < 0) {
        throw new IllegalArgumentException("No closing '}' in Clark name")
      }
      namespace = expandedName.substring(1, closeBrace)
      if (closeBrace == expandedName.length) {
        throw new IllegalArgumentException("Missing local part in Clark name")
      }
      localName = expandedName.substring(closeBrace + 1)
    } else {
      namespace = ""
      localName = expandedName
    }
    allocateFingerprint(namespace, localName)
  }

  def getFingerprint(uri: String, localName: String): Int = {
    if (NamespaceConstant.isReserved(uri) || uri == NamespaceConstant.SAXON) {
      val fp: Int = StandardNames.getFingerprint(uri, localName)
      if (fp != -1) {
        return fp
      }
      // otherwise, look for the name in this namepool
      // otherwise, look for the name in this namepool
    }
    val fp: java.lang.Integer =
      qNameToInteger.get(new StructuredQName("", uri, localName))
    if (fp == null) -1 else fp
  }

  // A read-only version of allocate()
  // A read-only version of allocate()

}

// Copyright (c) 2018-2020 Saxonica Limited
// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is "Incompatible With Secondary Licenses", as defined by the Mozilla Public License, v. 2.0.
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
/**
 * A NamePool holds a collection of expanded names, each containing a namespace URI,
 * and a local name.
 * <p>Each expanded name is allocated a unique 20-bit fingerprint. The fingerprint enables
 * the URI and the local name to be determined. Some subsystems (notably the Tinytree) use
 * the top 10 bits to represent the prefix, but the NamePool is no longer concerned with
 * managing prefixes, and prefixes do not have global codes.</p>
 * <p>The NamePool has been redesigned in Saxon 9.7 to make use of two Java
 * ConcurrentHashMaps, one from QNames to integers, one from integers to QNames.
 * This gives better scaleability in terms of multithreaded concurrency and in terms
 * of the capacity of the NamePool and retention of performance as the size of
 * the vocabulary increases.</p>
 * <p>Fingerprints in the range 0 to 1023 are reserved for system use, and are allocated as constants
 * mainly to names in the XSLT and XML Schema namespaces: constants representing these names
 * are found in {@link StandardNames}.</p>
 * <p>The fingerprint -1 is reserved to mean "not known" or inapplicable.</p>
 * <p>Modified in 9.4 to remove namespace codes.</p>
 * <p>Modified in 9.7 to remove URI codes.</p>
 * <p>Modified in 9.8 to remove namecodes and all handling of prefixes.</p>
 */
