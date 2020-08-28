////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
package net.sf.saxon.om

import net.sf.saxon.lib.NamespaceConstant
import net.sf.saxon.query.XQueryParser
import net.sf.saxon.trans.XPathException
import net.sf.saxon.value.Whitespace


/**
 * Parser to handle QNames in either lexical QName or EQName syntax, including resolving any prefix against
 * a URIResolver. The parser can be instantiated with various options to control the returned error code,
 * the handling of defaults, etc.
 *
 * The QNameParser is immutable; its properties are set using the <code>withProperty()</code> fluent API style.
 */
class QNameParser(private var resolver: NamespaceResolver) {

  private var acceptEQName: Boolean = false

  private var errorOnBadSyntax: String = "XPST0003"

  private var errorOnUnresolvedPrefix: String = "XPST0081"

  private var unescaper: XQueryParser.Unescaper = null

  def withNamespaceResolver(resolver: NamespaceResolver): QNameParser = {
    val qp2: QNameParser = copy()
    qp2.resolver = resolver
    qp2
  }

  def withAcceptEQName(acceptEQName: Boolean): QNameParser = {
    if (acceptEQName == this.acceptEQName) return this
    val qp2: QNameParser = copy()
    qp2.acceptEQName = acceptEQName
    qp2
  }

  def withErrorOnBadSyntax(code: String): QNameParser = {
    if (code == errorOnBadSyntax) {
      return this
    }
    val qp2: QNameParser = copy()
    qp2.errorOnBadSyntax = code
    qp2
  }

  def withErrorOnUnresolvedPrefix(code: String): QNameParser = {
    if (code == errorOnUnresolvedPrefix) {
      return this
    }
    val qp2: QNameParser = copy()
    qp2.errorOnUnresolvedPrefix = code
    qp2
  }

  def withUnescaper(unescaper: XQueryParser.Unescaper): QNameParser = {
    val qp2: QNameParser = copy()
    qp2.unescaper = unescaper
    qp2
  }

  private def copy(): QNameParser = {
    val qp2: QNameParser = new QNameParser(resolver)
    qp2.acceptEQName = acceptEQName
    qp2.errorOnBadSyntax = errorOnBadSyntax
    qp2.errorOnUnresolvedPrefix = errorOnUnresolvedPrefix
    qp2.unescaper = unescaper
    qp2
  }

  def parse(_lexicalName: CharSequence, defaultNS: String): StructuredQName = {
    var lexicalName = _lexicalName
    lexicalName = Whitespace.trimWhitespace(lexicalName)
    if (acceptEQName && lexicalName.length >= 4 && lexicalName.charAt(0) == 'Q' &&
      lexicalName.charAt(1) == '{') {
      val name: String = lexicalName.toString
      val endBrace: Int = name.indexOf('}')
      if (endBrace < 0) {
        throw new XPathException("Invalid EQName: closing brace not found", errorOnBadSyntax)
      } else if (endBrace == name.length - 1) {
        throw new XPathException("Invalid EQName: local part is missing", errorOnBadSyntax)
      }
      var uri: String = name.substring(2, endBrace).toString
      //String uri = Whitespace.collapseWhitespace(name.substring(2, endBrace)).toString();
      if (unescaper != null && uri.contains("&")) {
        uri = unescaper.unescape(uri).toString
      }
      if (uri == NamespaceConstant.XMLNS) {
        throw new XPathException(
          "The string '" + NamespaceConstant.XMLNS + "' cannot be used as a namespace URI",
          "XQST0070")
      }
      val local: String = name.substring(endBrace + 1)
      checkLocalName(local)
      new StructuredQName("", uri, local)
    }
    val parts: Array[String] = NameChecker.getQNameParts(lexicalName)
    checkLocalName(parts(1))
    if (parts(0).isEmpty) {
      new StructuredQName("", defaultNS, parts(1))
    }
    val uri: String = resolver.getURIForPrefix(parts(0), useDefault = false)
    if (uri == null) {
      throw new XPathException(
        "Namespace prefix '" + parts(0) + "' has not been declared",
        errorOnUnresolvedPrefix)
    }
    new StructuredQName(parts(0), uri, parts(1))
  }

  private def checkLocalName(local: String): Unit = {
    if (!NameChecker.isValidNCName(local)) {
      throw new XPathException(
        "Invalid EQName: local part is not a valid NCName",
        errorOnBadSyntax)
    }
  }

}

// Copyright (c) 2013-2020 Saxonica Limited
// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is "Incompatible With Secondary Licenses", as defined by the Mozilla Public License, v. 2.0.
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////