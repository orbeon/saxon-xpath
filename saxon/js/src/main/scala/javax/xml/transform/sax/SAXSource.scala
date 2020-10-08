package javax.xml.transform.sax

import javax.xml.transform.Source
import javax.xml.transform.stream.StreamSource
import org.xml.sax.InputSource
import org.xml.sax.XMLReader


object SAXSource {

  val FEATURE = "http://javax.xml.transform.sax.SAXSource/feature"

  def sourceToInputSource(source: Source): InputSource = source match {
    case saxSource: SAXSource => saxSource.getInputSource
    case ss: StreamSource =>
      val isource = new InputSource(ss.getSystemId)
      isource.setByteStream(ss.getInputStream)
      isource.setCharacterStream(ss.getReader)
      isource.setPublicId(ss.getPublicId)
      isource
    case _ => null
  }
}

class SAXSource extends Source {

  private var reader: XMLReader = null
  private var inputSource: InputSource = null

  def this(reader: XMLReader, inputSource: InputSource) = {
    this()
    this.reader = reader
    this.inputSource = inputSource
  }

  def this(inputSource: InputSource) = {
    this()
    this.inputSource = inputSource
  }

  def setXMLReader(reader: XMLReader): Unit =
    this.reader = reader

  def getXMLReader: XMLReader = reader

  def setInputSource(inputSource: InputSource): Unit =
    this.inputSource = inputSource

  def getInputSource: InputSource = inputSource

  override def setSystemId(systemId: String): Unit =
    if (inputSource eq null)
      inputSource = new InputSource(systemId)
    else
      inputSource.setSystemId(systemId)

  override def getSystemId: String =
    if (inputSource eq null)
      null
    else
      inputSource.getSystemId
}
