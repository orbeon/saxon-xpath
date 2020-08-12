package net.sf.saxon.serialize

import net.sf.saxon.event.ProxyReceiver

import net.sf.saxon.event.Receiver

import net.sf.saxon.s9api.Location

import net.sf.saxon.trans.Err

import net.sf.saxon.trans.XPathException

import net.sf.saxon.tree.tiny.CharSlice

import net.sf.saxon.value.Base64BinaryValue

import net.sf.saxon.value.HexBinaryValue

import java.io.ByteArrayInputStream

import java.io.InputStreamReader

import java.util.Properties

class BinaryTextDecoder(next: Receiver, details: Properties)
  extends ProxyReceiver(next) {

  var outputEncoding: String = "utf8"

  this.setOutputProperties(details)

  def setOutputProperties(details: Properties): Unit = {
    outputEncoding = details.getProperty("encoding", "utf8")
  }

  override def processingInstruction(name: String,
                                     value: CharSequence,
                                     locationId: Location,
                                     properties: Int): Unit = {
    var encoding: String = null
    var nameStr = name
    var bytes: Array[Byte] = null
    val dot: Int = nameStr.indexOf('.')
    if (dot >= 0 && dot != nameStr.length - 1) {
      encoding = nameStr.substring(dot + 1)
      nameStr = nameStr.substring(0, dot)
    } else {
      encoding = outputEncoding
    }
    if (nameStr.==("hex")) {
      bytes = new HexBinaryValue(value).getBinaryValue
    } else if (nameStr.==("b64")) {
      bytes = new Base64BinaryValue(value).getBinaryValue
    }
    if (bytes != null) {
      val stream: ByteArrayInputStream = new ByteArrayInputStream(bytes)
      val reader: InputStreamReader = new InputStreamReader(stream, encoding)
      val array: Array[Char] = Array.ofDim[Char](bytes.length)
      val used: Int = reader.read(array, 0, array.length)
      nextReceiver.characters(new CharSlice(array, 0, used),
        locationId,
        properties)
    }
  }

}
