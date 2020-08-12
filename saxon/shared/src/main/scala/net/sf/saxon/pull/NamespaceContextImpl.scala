


package net.sf.saxon.pull

import java.util

import net.sf.saxon.om.NamespaceResolver
import javax.xml.namespace.NamespaceContext
import java.util.ArrayList
import java.util.Iterator
import java.util.List



class NamespaceContextImpl(var resolver: NamespaceResolver)
  extends NamespaceContext
    with NamespaceResolver {

  def getURIForPrefix(prefix: String, useDefault: Boolean): String =
    resolver.getURIForPrefix(prefix, useDefault)

  def iteratePrefixes(): Iterator[String] = resolver.iteratePrefixes()

  def getPrefix(uri: String): String = {
    val prefixes: util.Iterator[String] = iteratePrefixes()
    while (prefixes.hasNext) {
      val p: String = prefixes.next()
      val u: String = resolver.getURIForPrefix(p, true)
      if (u == uri) {
        p
      }
    }
    null
  }

  def getNamespaceURI(prefix: String): String = {
    if (prefix.==("xmlns")) {
      "http://www.w3.org/2000/xmlns/"
    }
    resolver.getURIForPrefix(prefix, true)
  }
  def getPrefixes(uri: String): Iterator[String] = {
    val list: List[String] = new ArrayList[String](4)
    val prefixes: Iterator[String] = iteratePrefixes()
    prefixes.forEachRemaining((p) => {
      val u: String = resolver.getURIForPrefix(p, true)
      if (u == uri) {
        list.add(p)
      }
    })
    list.iterator()
  }

}