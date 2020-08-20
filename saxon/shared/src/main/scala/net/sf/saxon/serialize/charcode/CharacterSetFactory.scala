package net.sf.saxon.serialize.charcode

import net.sf.saxon.trans.XPathException

import javax.xml.transform.OutputKeys

import java.nio.charset.Charset

import java.nio.charset.IllegalCharsetNameException

import java.nio.charset.UnsupportedCharsetException

import java.util.HashMap

import java.util.Map

import java.util.Properties

import CharacterSetFactory._

import scala.jdk.CollectionConverters._

object CharacterSetFactory {

  private def normalizeCharsetName(name: String): String =
    name.replace("-", "").replace("_", "").toLowerCase()

  def main(args: Array[String]): Unit = {
    System.err.println(
      "Available Character Sets in the java.nio package for this Java VM:")
    for (s <- Charset.availableCharsets().keySet.asScala) {
      System.err.println("    " + s)
    }
    System.err.println("Registered Character Sets in Saxon:")
    val factory: CharacterSetFactory = new CharacterSetFactory()
    for ((key, value) <- factory.characterSets.asScala) {
      System.err.println("    " + key + " = " + value.getClass.getName)
    }
  }

}

class CharacterSetFactory {

  private var characterSets: HashMap[String, CharacterSet] =
    new HashMap[String, CharacterSet](10)

  val c: HashMap[String, CharacterSet] = characterSets

  val utf8: UTF8CharacterSet = UTF8CharacterSet.getInstance

  c.put("utf8", utf8)

  val utf16: UTF16CharacterSet = UTF16CharacterSet.getInstance

  c.put("utf16", utf16)

  val acs: ASCIICharacterSet = ASCIICharacterSet.getInstance

  c.put("ascii", acs)

  c.put("iso646", acs)

  c.put("usascii", acs)

  val lcs: ISO88591CharacterSet = ISO88591CharacterSet.getInstance

  c.put("iso88591", lcs)

  def setCharacterSetImplementation(encoding: String,
                                    charSet: CharacterSet): Unit = {
    characterSets.put(normalizeCharsetName(encoding), charSet)
  }

  def getCharacterSet(details: Properties): CharacterSet = {
    val encoding: String = details.getProperty(OutputKeys.ENCODING)
    if (encoding == null) {
      UTF8CharacterSet.getInstance
    }
    getCharacterSet(encoding)
  }

  def getCharacterSet(encoding: String): CharacterSet =
    if (encoding == null) {
      UTF8CharacterSet.getInstance
    } else {
      val encodingKey: String = normalizeCharsetName(encoding)
      val cs: CharacterSet = characterSets.get(encodingKey)
      if (cs != null) {
        return cs
      }
      var charset: Charset = null
      try {
        charset = Charset.forName(encoding)
        val res: CharacterSet = JavaCharacterSet.makeCharSet(charset)
        characterSets.put(encodingKey, res)
        res
      } catch {
        case err: IllegalCharsetNameException => {
          val e: XPathException = new XPathException(
            "Invalid encoding name: " + encoding)
          e.setErrorCode("SESU0007")
          throw e
        }

        case err: UnsupportedCharsetException => {
          val e: XPathException = new XPathException(
            "Unknown encoding requested: " + encoding)
          e.setErrorCode("SESU0007")
          throw e
        }

      }
    }

}
