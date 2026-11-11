

package org.orbeon.saxon.pull

import java.{util => ju}
import javax.xml.namespace.NamespaceContext
import org.orbeon.saxon.om.NamespaceResolver
import scala.jdk.CollectionConverters._

class NamespaceContextImpl(var resolver: NamespaceResolver)
  extends NamespaceContext
    with NamespaceResolver {

  def getURIForPrefix(prefix: String, useDefault: Boolean): String =
    resolver.getURIForPrefix(prefix, useDefault)

  def iteratePrefixes: Iterator[String] = resolver.iteratePrefixes

  def getPrefix(uri: String): String =
    iteratePrefixes.find(p => resolver.getURIForPrefix(p, useDefault = true) == uri).orNull

  def getNamespaceURI(prefix: String): String = {
    if (prefix == "xmlns")
      return "http://www.w3.org/2000/xmlns/"
    resolver.getURIForPrefix(prefix, useDefault = true)
  }

  def getPrefixes(uri: String): ju.Iterator[String] =
    iteratePrefixes.filter(p => resolver.getURIForPrefix(p, useDefault = true) == uri).asJava
}