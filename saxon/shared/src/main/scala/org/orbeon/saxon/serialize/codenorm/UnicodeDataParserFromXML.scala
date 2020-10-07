package org.orbeon.saxon.serialize.codenorm

import org.orbeon.saxon.lib.ParseOptions
import org.orbeon.saxon.lib.Validation
import org.orbeon.saxon.om.AxisInfo
import org.orbeon.saxon.om.NodeInfo
import org.orbeon.saxon.om.TreeInfo
import org.orbeon.saxon.pattern.NodeKindTest
import org.orbeon.saxon.trans.XPathException
import org.orbeon.saxon.tree.iter.AxisIterator
import org.orbeon.saxon.z.IntHashMap
import org.orbeon.saxon.z.IntToIntHashMap
import org.orbeon.saxon.z.IntToIntMap
import javax.xml.transform.stream.StreamSource
import java.io.InputStream
import java.util.ArrayList
import java.util.BitSet
import java.util.List
import java.util.StringTokenizer

import org.orbeon.saxon.utils.Configuration

object UnicodeDataParserFromXML {

  def build(config: Configuration): NormalizerData = {
    val in: InputStream = Configuration.locateResource("normalizationData.xml",
      new ArrayList(),
      new ArrayList())
    if (in == null) {
      throw new XPathException("Unable to read normalizationData.xml file")
    }
    val isExcluded: BitSet = new BitSet(128000)
    val isCompatibility: BitSet = new BitSet(128000)
    val options: ParseOptions = new ParseOptions()
    options.setSchemaValidationMode(Validation.SKIP)
    options.setDTDValidationMode(Validation.SKIP)
    val doc: TreeInfo = config.buildDocumentTree(
      new StreamSource(in, "normalizationData.xml"),
      options)
    var canonicalClassKeys: NodeInfo = null
    var canonicalClassValues: NodeInfo = null
    var decompositionKeys: NodeInfo = null
    var decompositionValues: NodeInfo = null
    val iter: AxisIterator =
      doc.getRootNode.iterateAxis(AxisInfo.DESCENDANT, NodeKindTest.ELEMENT)
    var item: NodeInfo = null
    while (({
      item = iter.next()
      item
    }) != null) item.getLocalPart match {
      case "CanonicalClassKeys" => canonicalClassKeys = item
      case "CanonicalClassValues" => canonicalClassValues = item
      case "DecompositionKeys" => decompositionKeys = item
      case "DecompositionValues" => decompositionValues = item
      case "ExclusionList" =>
        readExclusionList(item.getStringValue, isExcluded)
      case "CompatibilityList" =>
        readCompatibilityList(item.getStringValue, isCompatibility)

    }
    val canonicalClass: IntToIntMap = new IntToIntHashMap(400)
    canonicalClass.setDefaultValue(0)
    readCanonicalClassTable(canonicalClassKeys.getStringValue,
      canonicalClassValues.getStringValue,
      canonicalClass)
    val decompose: IntHashMap[String] = new IntHashMap[String](18000)
    val compose: IntToIntMap = new IntToIntHashMap(15000)
    compose.setDefaultValue(NormalizerData.NOT_COMPOSITE)
    readDecompositionTable(decompositionKeys.getStringValue,
      decompositionValues.getStringValue,
      decompose,
      compose,
      isExcluded,
      isCompatibility)
    new NormalizerData(canonicalClass,
      decompose,
      compose,
      isCompatibility,
      isExcluded)
  }

  private def readExclusionList(s: String, isExcluded: BitSet): Unit = {
    val st: StringTokenizer = new StringTokenizer(s)
    while (st.hasMoreTokens()) {
      val tok: String = st.nextToken()
      val value: Int = java.lang.Integer.parseInt(tok, 32)
      isExcluded.set(value)
    }
  }

  private def readCompatibilityList(s: String, isCompatible: BitSet): Unit = {
    val st: StringTokenizer = new StringTokenizer(s)
    while (st.hasMoreTokens()) {
      val tok: String = st.nextToken()
      val value: Int = java.lang.Integer.parseInt(tok, 32)
      isCompatible.set(value)
    }
  }

  private def readCanonicalClassTable(keyString: String,
                                      valueString: String,
                                      canonicalClasses: IntToIntMap): Unit = {
    val keys: List[Integer] = new ArrayList[Integer](5000)
    var st: StringTokenizer = new StringTokenizer(keyString)
    while (st.hasMoreTokens()) {
      val tok: String = st.nextToken()
      val value: Int = java.lang.Integer.parseInt(tok, 32)
      keys.add(value)
    }
    var k: Int = 0
    st = new StringTokenizer(valueString)
    while (st.hasMoreTokens()) {
      val tok: String = st.nextToken()
      var clss: Int = 0
      var repeat: Int = 1
      val star: Int = tok.indexOf('*')
      if (star < 0) {
        clss = java.lang.Integer.parseInt(tok, 32)
      } else {
        repeat = java.lang.Integer.parseInt(tok.substring(0, star))
        clss = java.lang.Integer.parseInt(tok.substring(star + 1), 32)
      }
      for (i <- 0 until repeat) {
        canonicalClasses.put(keys.get({
          k += 1; k - 1
        }), clss)
      }
    }
  }

  private def readDecompositionTable(decompositionKeyString: String,
                                     decompositionValuesString: String,
                                     decompose: IntHashMap[String],
                                     compose: IntToIntMap,
                                     isExcluded: BitSet,
                                     isCompatibility: BitSet): Unit = {
    var k: Int = 0
    val values: List[String] = new ArrayList[String](1000)
    var st: StringTokenizer = new StringTokenizer(decompositionValuesString)
    while (st.hasMoreTokens()) {
      val tok: String = st.nextToken()
      val value: StringBuilder = new StringBuilder()
      var c: Int = 0
      while (c < tok.length) {
        val h0: Char = tok.charAt({
          c += 1; c - 1
        })
        val h1: Char = tok.charAt({
          c += 1; c - 1
        })
        val h2: Char = tok.charAt({
          c += 1; c - 1
        })
        val h3: Char = tok.charAt({
          c += 1; c - 1
        })
        val code: Int = ("0123456789abcdef".indexOf(h0) << 12) + ("0123456789abcdef"
          .indexOf(h1) << 8) +
          ("0123456789abcdef".indexOf(h2) << 4) +
          "0123456789abcdef".indexOf(h3)
        value.append(code.toChar)
      }
      values.add(value.toString)
    }
    st = new StringTokenizer(decompositionKeyString)
    while (st.hasMoreTokens()) {
      val tok: String = st.nextToken()
      val key: Int = java.lang.Integer.parseInt(tok, 32)
      val value: String = values.get({
        k += 1; k - 1
      })
      decompose.put(key, value)
      if (!isCompatibility.get(key) && !isExcluded.get(key)) {
        var first: Char = 0
        var second: Char = value.charAt(0)
        if (value.length > 1) {
          first = second
          second = value.charAt(1)
        }
        val pair: Int = (first << 16) | second
        compose.put(pair, key)
      }
    }
    for (i <- 0 until SCount) {
      val TIndex: Int = i % TCount
      var first: Char = 0
      var second: Char = 0
      if (TIndex != 0) {
        first = (SBase + i - TIndex).toChar
        second = (TBase + TIndex).toChar
      } else {
        first = (LBase + i / NCount).toChar
        second = (VBase + (i % NCount) / TCount).toChar
      }
      val pair: Int = (first << 16) | second
      val key: Int = i + SBase
      decompose.put(key, String.valueOf(first) + second)
      compose.put(pair, key)
    }
  }

  private val SBase: Int = 0xAC00

  private val LBase: Int = 0x1100

  private val VBase: Int = 0x1161

  private val TBase: Int = 0x11A7

  private val LCount: Int = 19

  private val VCount: Int = 21

  private val TCount: Int = 28

  private val NCount: Int = VCount * TCount

  private val SCount: Int = LCount * NCount

}
