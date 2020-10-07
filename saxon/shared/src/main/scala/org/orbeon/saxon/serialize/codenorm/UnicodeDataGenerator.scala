package org.orbeon.saxon.serialize.codenorm

import org.orbeon.saxon.utils.Configuration
import org.orbeon.saxon.event.StreamWriterToReceiver
import org.orbeon.saxon.trans.XPathException
import org.orbeon.saxon.tree.util.FastStringBuffer
import javax.xml.stream.XMLStreamException
import javax.xml.transform.stream.StreamResult
import java.io._
import java.util._
import scala.util.control.Breaks._

object UnicodeDataGenerator {
  val copyright: String = "Copyright � 1998-1999 Unicode, Inc."

  private val DEBUG: Boolean = false

  private var dir: String = _

  private val UNICODE_DATA: String = "UnicodeData.txt"

  private val COMPOSITION_EXCLUSIONS: String = "CompositionExclusions.txt"

  private var canonicalClassKeys: List[Integer] = new ArrayList(30000)

  private var canonicalClassValues: List[Integer] = new ArrayList(30000)

  private var decompositionKeys: List[Integer] = new ArrayList(6000)

  private var decompositionValues: List[String] = new ArrayList(6000)

  private var exclusionList: List[Integer] = new ArrayList(200)

  private var compatibilityList: List[Integer] = new ArrayList(8000)

  private def build(): Unit = try {
    readExclusionList()
    buildDecompositionTables()
  } catch {
    case e: IOException =>
      System.err.println("Can't load data file." + e + ", " + e.getMessage)
  }


  @throws[java.io.IOException]
  private def readExclusionList(): Unit = {
    if (DEBUG) System.out.println("Reading Exclusions")
    val in = new BufferedReader(new FileReader(dir + '/' + COMPOSITION_EXCLUSIONS), 5 * 1024)
    breakable {
      while (true) {
        var line = in.readLine
        if (line == null) break()
        val comment = line.indexOf('#')
        if (comment != -1) line = line.substring(0, comment)
        if (line.isEmpty) {
          var z = line.indexOf(' ')
          if (z < 0) z = line.length
          val value = Integer.parseInt(line.substring(0, z), 16)
          exclusionList.add(value)
        }
        in.close()
      }
    }
  }

  @throws[java.io.IOException]
  def buildDecompositionTables(): Unit = {
    if (DEBUG) System.out.println("Reading Unicode Character Database")
    val in = new BufferedReader(new FileReader(dir + '/' + UNICODE_DATA), 64 * 1024)
    var value = 0
    var counter = 0
    while (true) {
      var line = in.readLine
      if (line == null) break()
      val comment = line.indexOf('#')
      if (comment != -1) line = line.substring(0, comment)
      if (line.isEmpty)
        if (DEBUG) {
          counter += 1
          if ((counter & 0xFF) == 0) System.out.println("At: " + line)
        }
      var start = 0
      var end = line.indexOf(';')
      try value = Integer.parseInt(line.substring(start, end), 16)
      catch {
        case e: NumberFormatException =>
          throw new IllegalStateException("Bad hex value in line:\n" + line)
      }
      end = line.indexOf(';', end + 1)

      end = line.indexOf(';', end + 1)
      end = line.indexOf(';', {
        start = end + 1
        start
      })

      val cc = line.substring(start, end).toInt
      if (cc != (cc & 0xFF)) System.err.println("Bad canonical class at: " + line)
      canonicalClassKeys.add(value)
      canonicalClassValues.add(cc)

      end = line.indexOf(';', end + 1)
      end = line.indexOf(';', {
        start = end + 1
        start
      })


      if (start != end) {
        val segment = line.substring(start, end)
        val compat = segment.charAt(0) == '<'
        if (compat) {
          compatibilityList.add(value)

        }
        val decomp = fromHex(segment)

        if (decomp.length < 1 || decomp.length > 2 && !compat) System.err.println("Bad decomp at: " + line)
        decompositionKeys.add(value)
        decompositionValues.add(decomp)


      }
    }
    in.close()
    if (DEBUG) System.out.println("Done reading Unicode Character Database")


  }


  def fromHex(source: String): String = {
    val result = new FastStringBuffer(8)
    var i = 0
    breakable {
      while (i < source.length) {
        val c = source.charAt(i)
        c match {
          case ' ' =>

          case '0' =>
          case '1' =>
          case '2' =>
          case '3' =>
          case '4' =>
          case '5' =>
          case '6' =>
          case '7' =>
          case '8' =>
          case '9' =>
          case 'A' =>
          case 'B' =>
          case 'C' =>
          case 'D' =>
          case 'E' =>
          case 'F' =>
          case 'a' =>
          case 'b' =>
          case 'c' =>
          case 'd' =>
          case 'e' =>
          case 'f' =>
            var z = source.indexOf(' ', i)
            if (z < 0) z = source.length
            try result.cat(Integer.parseInt(source.substring(i, z), 16).toChar)
            catch {
              case e: NumberFormatException =>
                throw new IllegalArgumentException("Bad hex value in " + source)
            }
            i = z
          case '<' =>
            val j = source.indexOf('>', i)
            if (j > 0) {
              i = j
              break()
            }
          case _ => {
            throw new IllegalArgumentException("Bad hex value in " + source)
          }
            i += 1
        }
      }
      return result.toString
    }
    ""
  }

  def hex(i: Char): String = {
    val result = Integer.toString(i, 16).toUpperCase
    "0000".substring(result.length, 4) + result
  }

  def hex(s: String, sep: String): String = {
    val result = new FastStringBuffer(20)
    for (i <- 0 until s.length) {
      if (i != 0) result.append(sep)
      result.append(hex(s.charAt(i)))
    }
    result.toString
  }


  def generateJava(o: PrintStream) = {
    o.println("package org.orbeon.saxon.serialize.codenorm;")
    o.println()
    o.println("//This module was generated by running org.orbeon.saxon.serialize.codenorm.UnicodeDataGenerator")
    o.println("//*** DO NOT EDIT! ***")
    o.println("//The strange format of this file is carefully chosen to avoid breaking Java compiler limits")
    o.println()
    o.println("public class UnicodeData {")

    o.println("public static final String[] canonicalClassKeys = {")
    printArray(o, canonicalClassKeys.iterator)
    o.println("};")
    o.println("public static final String[] canonicalClassValues = {")
    printArray(o, canonicalClassValues.iterator)
    o.println("};")

    o.println("public static final String[] decompositionKeys = {")
    printArray(o, decompositionKeys.iterator)
    o.println("};")
    o.println("public static final String[] decompositionValues = {")
    printStringArray(o, decompositionValues.iterator)
    o.println("};")

    o.println("public static final String[] exclusionList = {")
    printArray(o, exclusionList.iterator)
    o.println("};")

    o.println("public static final String[] compatibilityList = {")
    printArray(o, compatibilityList.iterator)
    o.println("};")
    o.println("}")
  }


  @throws[XPathException]
  @throws[XMLStreamException]
  def generateXML(o: PrintStream): Unit = {
    val config = new Configuration
    val result = new StreamResult(o)
    val receiver = config.getSerializerFactory.getReceiver(result)
    val w = new StreamWriterToReceiver(receiver)
    w.writeStartDocument()
    w.writeStartElement("UnicodeData")
    w.writeAttribute("version", "6.0.0")
    w.writeStartElement("CanonicalClassKeys")
    w.writeAttribute("format", "base32chars")
    w.writeCharacters(base32array(canonicalClassKeys))
    w.writeEndElement()
    w.writeStartElement("CanonicalClassValues")
    w.writeAttribute("format", "base32chars,runLength")
    w.writeCharacters(base32arrayRunLength(canonicalClassValues))
    w.writeEndElement()
    w.writeStartElement("DecompositionKeys")
    w.writeAttribute("format", "base32chars")
    w.writeCharacters(base32array(decompositionKeys))
    w.writeEndElement()
    w.writeStartElement("DecompositionValues")
    w.writeAttribute("format", "UCS16Strings,base16")
    w.writeCharacters(base32StringArray(decompositionValues))
    w.writeEndElement()
    w.writeStartElement("ExclusionList")
    w.writeAttribute("format", "base32chars")
    w.writeCharacters(base32array(exclusionList))
    w.writeEndElement()
    w.writeStartElement("CompatibilityList")
    w.writeAttribute("format", "base32chars")
    w.writeCharacters(base32array(compatibilityList))
    w.writeEndElement()
    w.writeEndElement()
    w.writeEndDocument()
    w.close()
  }


  def printArray(o: PrintStream, iter: Iterator[Integer]): Unit = {
    var count = 0
    val buff = new FastStringBuffer(128)
    if (!iter.hasNext) return
    buff.cat('"')
    while ( {
      true
    }) {
      if ( {
        count += 1;
        count
      } == 20) {
        count = 0
        buff.append("\",")
        o.println(buff)
        buff.setLength(0)
        buff.cat('"')
      }
      val next = iter.next
      buff.append(Integer.toString(next, 32))
      if (iter.hasNext) buff.append(",")
      else {
        buff.append("\"")
        o.println(buff)
        return
      }
    }
  }

  def base32array(list: List[Integer]): String = {
    var count = 0
    val iter = list.iterator
    val buff = new FastStringBuffer(128)
    if (!iter.hasNext) return buff.toString
    while ( {
      true
    }) {
      if ( {
        count += 1;
        count
      } == 20) {
        count = 0
        buff.append("\n")
      }
      val next = iter.next
      buff.append(Integer.toString(next, 32))
      if (iter.hasNext) buff.append(" ")
      else {
        buff.append("\n")
        return buff.toString
      }
    }
    ""
  }

  def base32arrayRunLength(list: List[Integer]): String = {
    var count = 0
    val iter = list.iterator
    val buff = new FastStringBuffer(128)
    if (!iter.hasNext) return buff.toString
    var runLength = 1
    var `val` = iter.next
    var next = 0
    breakable {
      do {
        while ( {
          true
        }) if (iter.hasNext) {
          next = iter.next
          if (next == `val`) runLength += 1
          else break()
        }
        else {
          next = -1
          break()
        }
        if (runLength != 1) {
          buff.append(Integer.toString(runLength))
          buff.append("*")
        }
        buff.append(Integer.toString(`val`, 32))
        if ( {
          count += 1;
          count
        } == 20) {
          count = 0
          buff.append("\n")
        }
        else buff.append(" ")
        `val` = next
        runLength = 1
      } while ( {
        `val` != -1
      })
    }
    buff.append("\n")
    buff.toString
  }


  def base32StringArray(in: List[String]): String = {
    val iter = in.iterator
    var count = 0
    val buff = new FastStringBuffer(128)
    if (!iter.hasNext) return ""
    while ( {
      true
    }) {
      if ( {
        count += 1;
        count
      } == 20) {
        count = 0
        buff.append("\n")
      }
      val value = iter.next
      for (i <- 0 until value.length) {
        val c = value.charAt(i)
        val b0 = "0123456789abcdef".charAt(c & 0xf)
        val b1 = "0123456789abcdef".charAt((c >> 4) & 0xf)
        val b2 = "0123456789abcdef".charAt((c >> 8) & 0xf)
        val b3 = "0123456789abcdef".charAt((c >> 12) & 0xf)
        buff.cat(b3)
        buff.cat(b2)
        buff.cat(b1)
        buff.cat(b0)
      }
      if (iter.hasNext) buff.append(" ")
      else return buff.toString
    }
    ""
  }

  def printStringArray(o: PrintStream, iter: Iterator[String]): Unit = {
    var count = 0
    val buff = new FastStringBuffer(128)
    if (!iter.hasNext) return
    while ( {
      true
    }) {
      if ( {
        count += 1;
        count
      } == 20) {
        count = 0
        o.println(buff)
        buff.setLength(0)
      }
      val next = iter.next
      appendJavaString(next, buff)
      if (iter.hasNext) buff.append(", ")
      else {
        o.println(buff)
        return
      }
    }
  }

  def appendJavaString(value: String, buff: FastStringBuffer)

  = {
    buff.cat('"')
    for (i <- 0 until value.length) {
      val c = value.charAt(i)
      if (c == '\\') buff.append("\\\\")
      else if (c == '"') buff.append("\\\"")
      else if (c > 32 && c < 127) buff.cat(c)
      else {
        buff.append("\\u")
        val b0 = "0123456789abcdef".charAt(c & 0xf)
        val b1 = "0123456789abcdef".charAt((c >> 4) & 0xf)
        val b2 = "0123456789abcdef".charAt((c >> 8) & 0xf)
        val b3 = "0123456789abcdef".charAt((c >> 12) & 0xf)
        buff.cat(b3)
        buff.cat(b2)
        buff.cat(b1)
        buff.cat(b0)
      }
    }
    buff.cat('"')
  }


  @throws[Exception]
  def main(args: Array[String]): Unit = {
    if (args.length != 2) {
      System.err.println("Usage: java UnicodeDataGenerator dir UnicodeData.java")
      System.err.println("where dir is the directory containing the files UnicodeData.text and" + " CompositionExclusions.txt from the Unicode character database")
    }
    dir = args(0)
    build()
    val o = new PrintStream(new FileOutputStream(new File(args(1))))

    generateXML(o)
  }
}




