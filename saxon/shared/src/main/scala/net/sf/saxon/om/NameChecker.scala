////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
package net.sf.saxon.om

import net.sf.saxon.serialize.charcode.UTF16CharacterSet

import net.sf.saxon.serialize.charcode.XMLCharacterData

import net.sf.saxon.trans.Err

import net.sf.saxon.trans.XPathException

import scala.util.control.Breaks._


object NameChecker {

  def isQName(name: String): Boolean = {
    val colon: Int = name.indexOf(':')
    if (colon < 0) {
      isValidNCName(name)
    }
    colon != 0 && colon != name.length - 1 && isValidNCName(
      name.substring(0, colon)) &&
      isValidNCName(name.substring(colon + 1))
  }

  def getPrefix(qname: String): String = {
    val colon: Int = qname.indexOf(':')
    if (colon < 0) {
      return ""
    }
    qname.substring(0, colon)
  }

  def getQNameParts(qname: CharSequence): Array[String] = {
    val parts: Array[String] = Array.ofDim[String](2)
    var colon: Int = -1
    val len: Int = qname.length
    breakable {
      for (i <- 0 until len if qname.charAt(i) == ':') {
        colon = i
        break()
      }
    }
    if (colon < 0) {
      parts(0) = ""
      parts(1) = qname.toString
      if (!isValidNCName(parts(1))) {
        throw new QNameException("Invalid QName " + Err.wrap(qname))
      }
    } else {
      if (colon == 0) {
        throw new QNameException(
          "QName cannot start with colon: " + Err.wrap(qname))
      }
      if (colon == len - 1) {
        throw new QNameException(
          "QName cannot end with colon: " + Err.wrap(qname))
      }
      parts(0) = qname.subSequence(0, colon).toString
      parts(1) = qname.subSequence(colon + 1, len).toString
      if (!isValidNCName(parts(1))) {
        if (!isValidNCName(parts(0))) {
          throw new QNameException(
            "Both the prefix " + Err.wrap(parts(0)) + " and the local part " +
              Err.wrap(parts(1)) +
              " are invalid")
        }
        throw new QNameException(
          "Invalid QName local part " + Err.wrap(parts(1)))
      }
    }
    parts
  }

  /*@NotNull*/

  def checkQNameParts(qname: CharSequence): Array[String] =
    try {
      val parts: Array[String] = getQNameParts(qname)
      if (parts(0).length > 0 && !isValidNCName(parts(0))) {
        throw new XPathException("Invalid QName prefix " + Err.wrap(parts(0)))
      }
      parts
    } catch {
      case e: QNameException => {
        val err = new XPathException(e.getMessage)
        err.setErrorCode("FORG0001")
        throw err
      }

    }

  def isValidNCName(ncName: CharSequence): Boolean = {
    if (ncName.length == 0) {
      return false
    }
    var s: Int = 1
    var ch: Char = ncName.charAt(0)
    if (UTF16CharacterSet.isHighSurrogate(ch)) {
      if (!isNCNameStartChar(
        UTF16CharacterSet.combinePair(ch, ncName.charAt(1)))) {
        return false
      }
      s = 2
    } else {
      if (!isNCNameStartChar(ch)) {
        return false
      }
    }
    var i: Int = 0
    while (i < ncName.length) {
      ch = ncName.charAt(i)
      if (UTF16CharacterSet.isHighSurrogate(ch)) {
        i = i + 1
        if (!isNCNameChar(UTF16CharacterSet.combinePair(ch, ncName.charAt(i)))) {
          return false
        }
      } else {
        if (!isNCNameChar(ch)) {
          return  false
        }
      }
      i = i + 1
    }
    true
  }

  def isValidNmtoken(nmtoken: CharSequence): Boolean = {
    if (nmtoken.length == 0) {
      return false
    }
    var i: Int = 0
    while (i < nmtoken.length) {
      val ch: Char = nmtoken.charAt(i)
      if (UTF16CharacterSet.isHighSurrogate(ch)) {
        i = i + 1
        if (!isNCNameChar(UTF16CharacterSet.combinePair(ch, nmtoken.charAt(i)))) {
          return false
        }
      } else {
        if (ch != ':' && !isNCNameChar(ch)) {
          return false
        }
      }
      i = i + 1
    }
    true
  }

  def isNCNameChar(ch: Int): Boolean = XMLCharacterData.isNCName11(ch)

  def isNCNameStartChar(ch: Int): Boolean =
    XMLCharacterData.isNCNameStart11(ch)

}

// Copyright (c) 2018-2020 Saxonica Limited
// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is "Incompatible With Secondary Licenses", as defined by the Mozilla Public License, v. 2.0.
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
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
