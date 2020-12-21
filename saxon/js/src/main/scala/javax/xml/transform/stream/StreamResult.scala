package javax.xml.transform.stream

import java.io.{OutputStream, Writer}

import javax.xml.transform.Result


object StreamResult {
  val FEATURE = "http://javax.xml.transform.stream.StreamResult/feature"
}

class StreamResult extends Result {

  private var systemId: String = null
  private var outputStream: OutputStream = null
  private var writer: Writer = null

  def this(outputStream: OutputStream) = {
    this()
    setOutputStream(outputStream)
  }

  def this(writer: Writer) = {
    this()
    setWriter(writer)
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

  def setOutputStream(outputStream: OutputStream): Unit =
    this.outputStream = outputStream

  def getOutputStream: OutputStream = outputStream

  def setWriter(writer: Writer): Unit =
    this.writer = writer

  def getWriter: Writer = writer

  override def setSystemId(systemId: String): Unit =
    this.systemId = systemId

  // ORBEON: No `File` support.
//  def setSystemId(f: File): Unit =
//    this.systemId = f.toURI.toASCIIString

  override def getSystemId: String = systemId
}
