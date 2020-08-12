////////////////////////////////////////////////////////////////////////////////////////////////////////////////////// Copyright (c) 2018-2020 Saxonica Limited
//// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
//// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
//// This Source Code Form is "Incompatible With Secondary Licenses", as defined by the Mozilla Public License, v. 2.0.
//////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
///**
// * CDATAFilter: This ProxyReceiver converts character data to CDATA sections,
// * if the character data belongs to one of a set of element types to be handled this way.
// *
// * @author Michael Kay
// */
package net.sf.saxon.serialize

import java.util._

import javax.xml.transform.OutputKeys
import net.sf.saxon.event.{ProxyReceiver, Receiver, ReceiverOption}
import net.sf.saxon.expr.parser.Loc
import net.sf.saxon.lib.NamespaceConstant
import net.sf.saxon.model.SchemaType
import net.sf.saxon.om._
import net.sf.saxon.s9api.Location
import net.sf.saxon.serialize.charcode.{CharacterSet, UTF16CharacterSet}
import net.sf.saxon.tree.tiny.CharSlice
import net.sf.saxon.tree.util.FastStringBuffer

//remove if not needed

class CDATAFilter(next: Receiver) extends ProxyReceiver(next) {

  private var buffer: FastStringBuffer = new FastStringBuffer(
    FastStringBuffer.C256)

  private var stack: Stack[NodeName] = new Stack[NodeName]()

  private var nameList: Set[NodeName] = _

  private var characterSet: CharacterSet = _

  def setOutputProperties(details: Properties): Unit = {
    getCdataElements(details)
    characterSet =
      getConfiguration.getCharacterSetFactory.getCharacterSet(details)
  }

  override def startElement(elemName: NodeName,
                            `type`: SchemaType,
                            attributes: AttributeMap,
                            namespaces: NamespaceMap,
                            location: Location,
                            properties: Int): Unit = {
    flush()
    stack.push(elemName)
    nextReceiver.startElement(elemName,
      `type`,
      attributes,
      namespaces,
      location,
      properties)
  }

  override def endElement(): Unit = {
    flush()
    stack.pop()
    nextReceiver.endElement()
  }

  override def processingInstruction(target: String,
                                     data: CharSequence,
                                     locationId: Location,
                                     properties: Int): Unit = {
    flush()
    nextReceiver.processingInstruction(target, data, locationId, properties)
  }

  override def characters(chars: CharSequence,
                          locationId: Location,
                          properties: Int): Unit = {
    if (!ReceiverOption.contains(properties, ReceiverOption.DISABLE_ESCAPING)) {
      buffer.append(chars.toString)
    } else {
      flush()
      nextReceiver.characters(chars, locationId, properties)
    }
  }

  override def comment(chars: CharSequence,
                       locationId: Location,
                       properties: Int): Unit = {
    flush()
    nextReceiver.comment(chars, locationId, properties)
  }

  private def flush(): Unit = {
    var cdata: Boolean = false
    var end: Int = buffer.length
    if (end == 0) {
      return
    }
    if (stack.isEmpty) {
      cdata = false
    } else {
      val top: NodeName = stack.peek()
      cdata = isCDATA(top)
    }
    if (cdata) {
      if (getNextReceiver.isInstanceOf[UnicodeNormalizer]) {
        buffer = new FastStringBuffer(
          getNextReceiver
            .asInstanceOf[UnicodeNormalizer]
            .normalize(buffer, true))
        end = buffer.length
      }
      var start: Int = 0
      var k: Int = 0
      while (k < end) {
        var next: Int = buffer.charAt(k)
        var skip: Int = 1
        if (UTF16CharacterSet.isHighSurrogate(next.toChar)) {
          next =
            UTF16CharacterSet.combinePair(next.toChar, buffer.charAt(k + 1))
          skip = 2
        }
        if (next != 0 && characterSet.inCharset(next)) {
          { k += 1; k - 1 }
        } else {
          val array: Array[Char] = Array.ofDim[Char](k - start)
          buffer.getChars(start, k, array, 0)
          flushCDATA(array, k - start)
          while (k < end) {
            nextReceiver.characters(buffer.subSequence(k, k + skip),
              Loc.NONE,
              ReceiverOption.DISABLE_CHARACTER_MAPS)
            k += skip
            if (k >= end) {
              //break
            }
            next = buffer.charAt(k)
            skip = 1
            if (UTF16CharacterSet.isHighSurrogate(next.toChar)) {
              next = UTF16CharacterSet.combinePair(next.toChar,
                buffer.charAt(k + 1))
              skip = 2
            }
            if (characterSet.inCharset(next)) {
              //break
            }
          }
          start = k
        }
      }
      val rest: Array[Char] = Array.ofDim[Char](end - start)
      buffer.getChars(start, end, rest, 0)
      flushCDATA(rest, end - start)
    } else {
      nextReceiver.characters(buffer, Loc.NONE, ReceiverOption.NONE)
    }
    buffer.setLength(0)
  }

  private def flushCDATA(array: Array[Char], len: Int): Unit = {
    if (len == 0) {
      return
    }
    val chprop: Int = ReceiverOption.DISABLE_ESCAPING | ReceiverOption.DISABLE_CHARACTER_MAPS
    val loc: Location = Loc.NONE
    nextReceiver.characters("<![CDATA[", loc, chprop)
    var i: Int = 0
    var doneto: Int = 0
    while (i < len - 2) {
      if (array(i) == ']' && array(i + 1) == ']' && array(i + 2) == '>') {
        nextReceiver.characters(new CharSlice(array, doneto, i + 2 - doneto),
          loc,
          chprop)
        nextReceiver.characters("]]><![CDATA[", loc, chprop)
        doneto = i + 2
      } else if (array(i) == 0) {
        nextReceiver.characters(new CharSlice(array, doneto, i - doneto),
          loc,
          chprop)
        doneto = i + 1
      }
      { i += 1; i - 1 }
    }
    nextReceiver.characters(new CharSlice(array, doneto, len - doneto),
      loc,
      chprop)
    nextReceiver.characters("]]>", loc, chprop)
  }
  // Check that the character data doesn't include the substring "]]>"
  // Also get rid of any zero bytes inserted by character map expansion
  // Check that the character data doesn't include the substring "]]>"
  // Also get rid of any zero bytes inserted by character map expansion

   def isCDATA(elementName: NodeName): Boolean =
    nameList.contains(elementName)

  private def getCdataElements(details: Properties): Unit = {
    val isHTML: Boolean = "html" == details.getProperty(OutputKeys.METHOD)
    val isHTML5: Boolean = isHTML && "5.0" == details.getProperty(
      OutputKeys.VERSION)
    val isHTML4: Boolean = isHTML && !isHTML5
    val cdata: String = details.getProperty(OutputKeys.CDATA_SECTION_ELEMENTS)
    if (cdata == null) {
      // this doesn't happen, but there's no harm allowing for it
      nameList = new HashSet[NodeName](0)
      return
    }
    nameList = new HashSet[NodeName](10)
    val st2: StringTokenizer = new StringTokenizer(cdata, " \t\n\r", false)
    while (st2.hasMoreTokens()) {
      val expandedName: String = st2.nextToken()
      val sq: StructuredQName = StructuredQName.fromClarkName(expandedName)
      val uri: String = sq.getURI
      if (!isHTML || (isHTML4 && uri.!=("")) ||
        (isHTML5 && uri.!=("") && uri != NamespaceConstant.XHTML)) {
        nameList.add(new FingerprintedQName("", sq.getURI, sq.getLocalPart))
      }
    }
  }

}

