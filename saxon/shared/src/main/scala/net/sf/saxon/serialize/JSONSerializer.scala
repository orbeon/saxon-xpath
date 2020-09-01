////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// Copyright (c) 2018-2020 Saxonica Limited
// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is "Incompatible With Secondary Licenses", as defined by the Mozilla Public License, v. 2.0.
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
/**
 * This class implements the JSON serialization method defined in XSLT+XQuery Serialization 3.1.
 *
 * @author Michael H. Kay
 */

package net.sf.saxon.serialize

import net.sf.saxon.event.PipelineConfiguration
import net.sf.saxon.event.ReceiverWithOutputProperties
import net.sf.saxon.event.SequenceWriter
import net.sf.saxon.lib.SaxonOutputKeys
import net.sf.saxon.ma.arrays.ArrayItem
import net.sf.saxon.ma.map.KeyValuePair
import net.sf.saxon.ma.map.MapItem
import net.sf.saxon.om.GroundedValue
import net.sf.saxon.om.Item
import net.sf.saxon.om.NodeInfo
import net.sf.saxon.serialize.charcode.CharacterSet
import net.sf.saxon.serialize.codenorm.Normalizer
import net.sf.saxon.trans.Err
import net.sf.saxon.trans.XPathException
import net.sf.saxon.value.AtomicValue
import net.sf.saxon.value.StringValue
import javax.xml.transform.OutputKeys
import javax.xml.transform.stream.StreamResult
import java.io.StringWriter
import java.util._
import scala.jdk.CollectionConverters._
import net.sf.saxon.query.QueryResult
import scala.jdk.CollectionConverters._

class JSONSerializer(pipe: PipelineConfiguration,
                     private var emitter: JSONEmitter,
                     var outputProperties: Properties)
  extends SequenceWriter(pipe)
    with ReceiverWithOutputProperties {

  private var allowDuplicateKeys: Boolean = false

  private var nodeOutputMethod: String = "xml"

  private var level: Int = 0

  private var topLevelCount: Int = 0

  private var maxLineLength: Int = 80

  private var characterSet: CharacterSet = _

  private var isIndenting: Boolean = _

  private var propertySorter: Comparator[AtomicValue] = _

  private var unfailing: Boolean = false

  def setOutputProperties(details: Properties): Unit = {
    this.outputProperties = details
    if ("yes" == details.getProperty(SaxonOutputKeys.ALLOW_DUPLICATE_NAMES)) {
      allowDuplicateKeys = true
    }
    if ("yes" == details.getProperty(OutputKeys.INDENT)) {
      isIndenting = true
    }
    if ("yes" == details.getProperty(SaxonOutputKeys.UNFAILING)) {
      unfailing = true
      allowDuplicateKeys = true
    }
    val jnom: String =
      details.getProperty(SaxonOutputKeys.JSON_NODE_OUTPUT_METHOD)
    if (jnom != null) {
      nodeOutputMethod = jnom
    }
    val max: String = details.getProperty(SaxonOutputKeys.LINE_LENGTH)
    if (max != null) {
      try maxLineLength = java.lang.Integer.parseInt(max)
      catch {
        case err: NumberFormatException => {}

      }
    }
  }

  override def getOutputProperties(): Properties = outputProperties

  def setPropertySorter(sorter: Comparator[AtomicValue]): Unit = {
    this.propertySorter = sorter
  }

  def setNormalizer(normalizer: Normalizer): Unit = {
    emitter.setNormalizer(normalizer)
  }

  def setCharacterMap(map: CharacterMap): Unit = {
    emitter.setCharacterMap(map)
  }

  private def isOneLinerArray(array: ArrayItem): Boolean = {
    var totalSize: Int = 0
    if (array.arrayLength() < 2) {
      return true
    }
    for (member <- array.members()) {
      if (!(member.isInstanceOf[AtomicValue])) {
        return false
      }
      totalSize += member.asInstanceOf[AtomicValue].getStringValueCS.length +
        1
      if (totalSize > maxLineLength) {
        false
      }
    }
    true
  }

  private def isOneLinerMap(map: MapItem): Boolean = {
    var totalSize: Int = 0
    if (map.size < 2) {
      return true
    }
    for (entry <- map.keyValuePairs().asScala) {
      if (!(entry.value.isInstanceOf[AtomicValue])) {
        return false
      }
      totalSize += entry.key.getStringValueCS.length +
        entry.value.asInstanceOf[AtomicValue].getStringValueCS.length +
        4
      if (totalSize > maxLineLength) {
        false
      }
    }
    true
  }

  private def serializeNode(node: NodeInfo): String = {
    val sw: StringWriter = new StringWriter()
    val props: Properties = new Properties()
    props.setProperty("method", nodeOutputMethod)
    props.setProperty("indent", "no")
    props.setProperty("omit-xml-declaration", "yes")
    QueryResult.serialize(node, new StreamResult(sw), props)
    sw.toString.trim()
  }

  private def writeSequence(seq: GroundedValue): Unit = {
    val len: Int = seq.getLength
    if (len == 0) {
      emitter.writeAtomicValue(null)
    } else if (len == 1) {
      level += 1
      write(seq.head)
      level -= 1
    } else {
      throw new XPathException(
        "JSON serialization: cannot handle a sequence of length " +
          len +
          Err.depictSequence(seq),
        "SERE0023")
    }
  }

  /**
   * End of the document.
   */
  override def close(): Unit = {
    if (topLevelCount == 0) {
      emitter.writeAtomicValue(null)
    }
    emitter.close()
    super.close()
  }

  @throws[XPathException]
  override def write(item: Item) = {
    topLevelCount += 1
    if (level == 0 && topLevelCount >= 2) throw new XPathException("JSON output method cannot handle sequences of two or more items", "SERE0023")
    if (item.isInstanceOf[AtomicValue]) emitter.writeAtomicValue(item.asInstanceOf[AtomicValue])
    else if (item.isInstanceOf[MapItem]) {
      var keys: Set[String] = null
      if (!allowDuplicateKeys) keys = new HashSet[String]
      val oneLiner = !isIndenting || isOneLinerMap(item.asInstanceOf[MapItem])
      emitter.startMap(oneLiner)
      val first = true
      val keyList = new ArrayList[AtomicValue]
      for (pair <- item.asInstanceOf[MapItem].keyValuePairs.asScala) {
        keyList.add(pair.key)
      }
      if (propertySorter != null) keyList.sort(propertySorter)
      for (key: AtomicValue <- keyList.asScala) {
        val stringKey = key.getStringValue
        emitter.writeKey(stringKey)
        if (!allowDuplicateKeys && !keys.add(stringKey))
          throw new XPathException("Key value \"" + stringKey + "\" occurs more than once in JSON map", "SERE0022")
        val value = item.asInstanceOf[MapItem].get(key)
        writeSequence(value.materialize)
      }
      emitter.endMap()
    }
    else if (item.isInstanceOf[ArrayItem]) {
      val oneLiner = !isIndenting || isOneLinerArray(item.asInstanceOf[ArrayItem])
      emitter.startArray(oneLiner)
      val first = true
      for (member <- item.asInstanceOf[ArrayItem].members) {
        writeSequence(member.materialize)
      }
      emitter.endArray()
    }
    else if (item.isInstanceOf[NodeInfo]) {
      val s = serializeNode(item.asInstanceOf[NodeInfo])
      emitter.writeAtomicValue(new StringValue(s))
    }
    else if (unfailing) {
      val s = item.getStringValue
      emitter.writeAtomicValue(new StringValue(s))
    }
    else throw new XPathException("JSON output method cannot handle an item of type " + item.getClass, "SERE0021")
  }
}