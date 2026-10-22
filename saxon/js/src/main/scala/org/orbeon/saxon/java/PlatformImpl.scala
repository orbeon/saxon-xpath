package org.orbeon.saxon.java

import java.io.InputStream
import org.xml.sax.XMLReader

object PlatformImpl {

  def loadParser(): XMLReader =
    throw new UnsupportedOperationException("loadParser is not supported on Scala.js")

  def locateResource(filename: String): InputStream =
    null
}
