package org.orbeon.saxon.java

import java.io.InputStream
import org.xml.sax.XMLReader

object PlatformImpl {

  def loadParser(): XMLReader =
    javax.xml.parsers.SAXParserFactory.newInstance().newSAXParser().getXMLReader

  def locateResource(filename: String): InputStream = {
    val candidates = List(
      "org/orbeon/saxon/data/" + filename,
      "net/sf/saxon/data/" + filename,
      filename
    )
    var loader = Thread.currentThread.getContextClassLoader
    if (loader == null)
      loader = getClass.getClassLoader
    if (loader != null) {
      for (candidate <- candidates) {
        val in = loader.getResourceAsStream(candidate)
        if (in != null) return in
      }
    }
    null
  }
}
