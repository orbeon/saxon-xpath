package javax.xml.transform.stream

import java.io.{InputStream, Reader}

import javax.xml.transform.Source


object StreamSource {
  val FEATURE = "http://javax.xml.transform.stream.StreamSource/feature"
}

class StreamSource extends Source {

  private var publicId: String = null
  private var systemId: String = null
  private var inputStream: InputStream = null
  private var reader: Reader = null

  def this(inputStream: InputStream) = {
    this()
    setInputStream(inputStream)
  }

  def this(inputStream: InputStream, systemId: String) = {
    this()
    setInputStream(inputStream)
    setSystemId(systemId)
  }

  def this(reader: Reader) = {
    this()
    setReader(reader)
  }

  def this(reader: Reader, systemId: String) = {
    this()
    setReader(reader)
    setSystemId(systemId)
  }

  def this(systemId: String) = {
    this()
    this.systemId = systemId
  }

  // ORBEON: No `File` support.
//  def this(f: File) = {
//    this()
//    //convert file to appropriate URI, f.toURI().toASCIIString()
//    //converts the URI to string as per rule specified in
//    //RFC 2396,
//    setSystemId(f.toURI.toASCIIString)
//  }

  def setInputStream(inputStream: InputStream): Unit =
    this.inputStream = inputStream

  def getInputStream: InputStream = inputStream

  def setReader(reader: Reader): Unit =
    this.reader = reader

  def getReader: Reader = reader

  def setPublicId(publicId: String): Unit =
    this.publicId = publicId

  def getPublicId: String = publicId

  def setSystemId(systemId: String): Unit =
    this.systemId = systemId

  def getSystemId: String = systemId

// ORBEON: No `File` support.
//  def setSystemId(f: File): Unit =
//    this.systemId = f.toURI.toASCIIString
}
