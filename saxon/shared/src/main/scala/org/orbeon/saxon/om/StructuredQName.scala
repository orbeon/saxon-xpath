package org.orbeon.saxon.om

import org.orbeon.saxon.trans.XPathException

import org.orbeon.saxon.tree.util.FastStringBuffer

import org.orbeon.saxon.value.Whitespace

import javax.xml.namespace.QName

object StructuredQName {

  def fromClarkName(expandedName: String): StructuredQName = {
    var namespace: String = null
    var localName: String = null

    var expName = expandedName
    if (expName.startsWith("Q{"))
      expName = expName.substring(1)

    if (expName.charAt(0) == '{') {

      val closeBrace = expName.indexOf('}')
      if (closeBrace < 0)
        throw new IllegalArgumentException("No closing '}' in Clark name")

      namespace = expName.substring(1, closeBrace)
      if (closeBrace == expName.length)
        throw new IllegalArgumentException("Missing local part in Clark name")

      localName = expName.substring(closeBrace + 1)
    } else {
      namespace = ""
      localName = expName
    }
    new StructuredQName("", namespace, localName)
  }

  def fromLexicalQName(lexicalName: CharSequence,
                       useDefault: Boolean,
                       allowEQName: Boolean,
                       resolver: NamespaceResolver): StructuredQName = {
    var lexName = lexicalName
    lexName = Whitespace.trimWhitespace(lexName)
    if (allowEQName && lexName.length >= 4 && lexName.charAt(0) == 'Q' && lexName.charAt(1) == '{') {

      val name = lexName.toString

      val endBrace = name.indexOf('}')
      if (endBrace < 0)
        throw new XPathException("Invalid EQName: closing brace not found", "FOCA0002")
      else if (endBrace == name.length - 1)
        throw new XPathException("Invalid EQName: local part is missing", "FOCA0002")

      val uri = name.substring(2, endBrace)
      if (uri.contains("{"))
        throw new XPathException("Namespace URI must not contain '{'", "FOCA0002")

      val local = name.substring(endBrace + 1)
      if (! NameChecker.isValidNCName(local))
        throw new XPathException("Invalid EQName: local part is not a valid NCName", "FOCA0002")

      return new StructuredQName("", uri, local)
    }
    val parts = NameChecker.getQNameParts(lexName)
    val uri = resolver.getURIForPrefix(parts(0), useDefault)
    if (uri == null) {
      if (NameChecker.isValidNCName(parts(0))) {
        val de = new XPathException("Namespace prefix '" + parts(0) + "' has not been declared")
        de.setErrorCode("FONS0004")
        throw de
      } else {
        val de = new XPathException("Invalid namespace prefix '" + parts(0) + "'")
        de.setErrorCode("FOCA0002")
        throw de
      }
    }
    new StructuredQName(parts(0), uri, parts(1))
  }

  def fromEQName(eqName: CharSequence): StructuredQName = {
    var eqChName = eqName
    eqChName = Whitespace.trimWhitespace(eqChName)
    if (eqChName.length >= 4 && eqChName.charAt(0) == 'Q' && eqChName.charAt(1) == '{') {
      val name = eqChName.toString
      val endBrace = name.indexOf('}')
      if (endBrace < 0) {
        throw new IllegalArgumentException("Invalid EQName: closing brace not found")
      } else if (endBrace == name.length - 1) {
        throw new IllegalArgumentException("Invalid EQName: local part is missing")
      }
      val uri = name.substring(2, endBrace)
      val local = name.substring(endBrace + 1)
      new StructuredQName("", uri, local)
    } else {
      new StructuredQName("", "", eqChName.toString)
    }
  }

  def computeHashCode(uri: CharSequence, local: CharSequence): Int = {
    var h = 0x8004a00b
    val localLen = local.length
    val uriLen = uri.length
    val totalLen = localLen + uriLen
    h ^= totalLen
    h ^= uriLen
    var i = 0
    var j = uriLen
    while (i < localLen) {
      h ^= local.charAt(i) << (j & 0x1f)
      i += 1
      j += 1
    }
    h
  }

}

class StructuredQName private(var content: Array[Char],
                              var localNameStart: Int,
                              var prefixStart: Int)
  extends IdentityComparable {

  private var cachedHashCode: Int = -1

  def this(prefix: String, uri: String, localName: String) = {
    this(null, 0, 0)

    var uriStr = uri
    if (uriStr == null)
      uriStr = ""

    val plen = prefix.length
    val ulen = uriStr.length
    val llen = localName.length

    localNameStart = ulen
    prefixStart = ulen + llen
    content = Array.ofDim[Char](ulen + llen + plen)
    uriStr.getChars(0, ulen, content, 0)
    localName.getChars(0, llen, content, ulen)
    prefix.getChars(0, plen, content, ulen + llen)
  }

  def getPrefix: String =
    new String(content, prefixStart, content.length - prefixStart)

  def getURI: String =
    if (localNameStart == 0)
      ""
    else
      new String(content, 0, localNameStart)

  def hasURI(uri: String): Boolean = {
    if (localNameStart != uri.length) {
      return false
    }
    var i = localNameStart - 1
    while (i >= 0) {
      if (content(i) != uri.charAt(i))
        return false
        i -= 1
    }
    true
  }

  def getLocalPart: String =
    new String(content, localNameStart, prefixStart - localNameStart)

  def getDisplayName: String =
    if (prefixStart == content.length) {
      getLocalPart
    } else {
      val buff = new FastStringBuffer(
        content.length - localNameStart + 1)
      buff.append(content, prefixStart, content.length - prefixStart)
      buff.cat(':')
      buff.append(content, localNameStart, prefixStart - localNameStart)
      buff.toString
    }

  def getStructuredQName: StructuredQName = this

  def getClarkName: String = {
    val buff = new FastStringBuffer(
      content.length - prefixStart + 2)
    if (localNameStart > 0) {
      buff.cat('{')
      buff.append(content, 0, localNameStart)
      buff.cat('}')
    }
    buff.append(content, localNameStart, prefixStart - localNameStart)
    buff.toString
  }

  def getEQName: String = {
    val buff = new FastStringBuffer(
      content.length - prefixStart + 2)
    buff.append("Q{")
    if (localNameStart > 0) {
      buff.append(content, 0, localNameStart)
    }
    buff.cat('}')
    buff.append(content, localNameStart, prefixStart - localNameStart)
    buff.toString
  }

  override def toString: String = getDisplayName

  override def equals(other: Any): Boolean =
    other match {
      case sq2: StructuredQName if sq2 eq this => true
      case sq2: StructuredQName =>
        val c = sq2.cachedHashCode
        if (c != -1 && c != hashCode) {
          return false
        }
        if (localNameStart != sq2.localNameStart || prefixStart != sq2.prefixStart) {
          return false
        }
        var i = prefixStart - 1
        while (i >= 0) {
          if (content(i) != sq2.content(i)) {
            return false
          }
          i -= 1;
        }
        true
      case _ => false
    }

  override def hashCode: Int =
    if (cachedHashCode == -1) {
      var h: Int = 0x8004a00b
      h ^= prefixStart
      h ^= localNameStart
      for (i <- localNameStart until prefixStart) {
        h ^= content(i) << (i & 0x1f)
      }
      cachedHashCode = h
      cachedHashCode
    } else {
      cachedHashCode
    }

  def toJaxpQName: QName =
    new javax.xml.namespace.QName(getURI, getLocalPart, getPrefix)

  def getNamespaceBinding: NamespaceBinding =
    NamespaceBinding.makeNamespaceBinding(getPrefix, getURI)

  def isIdentical(other: IdentityComparable): Boolean =
    equals(other) && other.asInstanceOf[StructuredQName].getPrefix == getPrefix

  def identityHashCode(): Int = hashCode ^ getPrefix.hashCode
}
