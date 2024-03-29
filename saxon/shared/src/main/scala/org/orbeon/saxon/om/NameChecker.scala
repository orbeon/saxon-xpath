////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// Copyright (c) 2018-2020 Saxonica Limited
// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is "Incompatible With Secondary Licenses", as defined by the Mozilla Public License, v. 2.0.
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
package org.orbeon.saxon.om

import org.orbeon.saxon.serialize.charcode.{UTF16CharacterSet, XMLCharacterData}
import org.orbeon.saxon.trans.{Err, XPathException}

import scala.util.control.Breaks._


/**
 * The NameChecker performs validation and analysis of XML names.
 *
 * <p>In releases prior to 9.6, there were two name checkers, one for XML 1.0 and
 * one for XML 1.1. However, XML 1.0 fifth edition uses the same rules for XML names
 * as XML 1.1, so they were actually checking the same rules for names (although they
 * were different when checking for valid characters). From 9.6, the name checker
 * no longer performs checks for valid XML characters, so only one name checker is
 * needed, and the methods have become static.</p>
 */
object NameChecker {

  def isQName(name: String): Boolean = {
    val colon = name.indexOf(':')
    if (colon < 0)
      isValidNCName(name)
    else
      colon != 0                              &&
      colon != name.length - 1                &&
      isValidNCName(name.substring(0, colon)) &&
      isValidNCName(name.substring(colon + 1))
  }

  def getPrefix(qname: String): String = {
    val colon = qname.indexOf(':')
    if (colon < 0)
      ""
    else
      qname.substring(0, colon)
  }

  def getQNameParts(qname: CharSequence): Array[String] = {
    val parts = Array.ofDim[String](2)
    var colon = -1
    val len = qname.length
    breakable {
      for (i <- 0 until len if qname.charAt(i) == ':') {
        colon = i
        break()
      }
    }
    if (colon < 0) {
      parts(0) = ""
      parts(1) = qname.toString
      if (! isValidNCName(parts(1)))
        throw new QNameException("Invalid QName " + Err.wrap(qname))
    } else {
      if (colon == 0)
        throw new QNameException("QName cannot start with colon: " + Err.wrap(qname))
      if (colon == len - 1)
        throw new QNameException("QName cannot end with colon: " + Err.wrap(qname))
      parts(0) = qname.subSequence(0, colon).toString
      parts(1) = qname.subSequence(colon + 1, len).toString
      if (! isValidNCName(parts(1))) {
        if (! isValidNCName(parts(0)))
          throw new QNameException(
            "Both the prefix " + Err.wrap(parts(0)) + " and the local part " + Err.wrap(parts(1)) + " are invalid"
          )
        else
          throw new QNameException("Invalid QName local part " + Err.wrap(parts(1)))
      }
    }
    parts
  }

  /*@NotNull*/
  def checkQNameParts(qname: CharSequence): Array[String] =
    try {
      val parts = getQNameParts(qname)
      if (parts(0).nonEmpty && ! isValidNCName(parts(0)))
        throw new XPathException("Invalid QName prefix " + Err.wrap(parts(0)))
      parts
    } catch {
      case e: QNameException =>
        val err = new XPathException(e.getMessage)
        err.setErrorCode("FORG0001")
        throw err
    }

  def isValidNCName(ncName: CharSequence): Boolean = {
    if (ncName.length == 0)
      return false
    var s = 1
    var ch = ncName.charAt(0)
    if (UTF16CharacterSet.isHighSurrogate(ch)) {
      if (! isNCNameStartChar(
        UTF16CharacterSet.combinePair(ch, ncName.charAt(1)))) {
        return false
      }
      s = 2
    } else {
      if (! isNCNameStartChar(ch))
        return false
    }
    var i = 0
    while (i < ncName.length) {
      ch = ncName.charAt(i)
      if (UTF16CharacterSet.isHighSurrogate(ch)) {
        i += 1
        if (! isNCNameChar(UTF16CharacterSet.combinePair(ch, ncName.charAt(i))))
          return false
      } else {
        if (! isNCNameChar(ch))
          return  false
      }
      i += 1
    }
    true
  }

  def isValidNmtoken(nmtoken: CharSequence): Boolean = {
    if (nmtoken.length == 0)
      return false
    var i = 0
    while (i < nmtoken.length) {
      val ch = nmtoken.charAt(i)
      if (UTF16CharacterSet.isHighSurrogate(ch)) {
        i += 1
        if (! isNCNameChar(UTF16CharacterSet.combinePair(ch, nmtoken.charAt(i))))
          return false
      } else {
        if (ch != ':' && ! isNCNameChar(ch))
          return false
      }
      i += 1
    }
    true
  }

  def isNCNameChar(ch: Int): Boolean =
    XMLCharacterData.isNCName11(ch)

  def isNCNameStartChar(ch: Int): Boolean =
    XMLCharacterData.isNCNameStart11(ch)
}
