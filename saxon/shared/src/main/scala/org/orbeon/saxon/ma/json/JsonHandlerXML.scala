////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// Copyright (c) 2018-2020 Saxonica Limited
// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is "Incompatible With Secondary Licenses", as defined by the Mozilla Public License, v. 2.0.
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
package org.orbeon.saxon.ma.json

import java.util.{HashMap, HashSet, Stack}

import javax.xml.transform.sax.SAXSource
import org.orbeon.saxon.event.{Builder, ComplexContentOutputter, Outputter, ReceiverOption}
import org.orbeon.saxon.expr.XPathContext
import org.orbeon.saxon.expr.parser.Loc
import org.orbeon.saxon.lib.{NamespaceConstant, StandardEntityResolver}
import org.orbeon.saxon.ma.json.JsonHandlerXML._
import org.orbeon.saxon.model.Untyped.Untyped
import org.orbeon.saxon.model.{AnySimpleType, _}
import org.orbeon.saxon.om._
import org.orbeon.saxon.trans.XPathException
import org.orbeon.saxon.utils.Configuration
import org.xml.sax.InputSource


object JsonHandlerXML {

  private val SCHEMA_URI: String = "http://www.w3.org/2005/xpath-functions.xsd"

  private val JSON_NS: String = NamespaceConstant.FN

  val PREFIX: String = ""

  private val UNTYPED: Untyped = Untyped.getInstance

  private val SIMPLE_TYPE: AnySimpleType.type = AnySimpleType

  private val BOOLEAN_TYPE: BuiltInAtomicType = BuiltInAtomicType.BOOLEAN

  private val STRING_TYPE: BuiltInAtomicType = BuiltInAtomicType.STRING

}
class JsonHandlerXML( var xpathContext: XPathContext, staticBaseUri: String, flags: Int) extends JsonHandler {

  private var builder: Builder = xpathContext.getController.makeBuilder

  private var out: Outputter = new ComplexContentOutputter(builder)

  private var keys: Stack[String] = _

  private var inMap: Stack[Boolean] = new Stack()

  private var allowAnyTopLevel: Boolean = _

  var validate: Boolean = _

  private var checkForDuplicates: Boolean = _

  private var namePool: NamePool = _

  private var mapQN: FingerprintedQName = _

  private var arrayQN: FingerprintedQName = _

  private var stringQN: FingerprintedQName = _

  private var numberQN: FingerprintedQName = _

  private var booleanQN: FingerprintedQName = _

  private var nullQN: FingerprintedQName = _

  private var keyQN: FingerprintedQName = _

  private var escapedQN: FingerprintedQName = _

  private var escapedKeyQN: FingerprintedQName = _

  var types: HashMap[String, SchemaType] = _

  private var mapKeys: Stack[HashSet[String]] = new Stack()

  /**
   * Create a QName in null namespace
   *
   * @param s the local name
   * @return the QName
   */
  private def qname(s: String): FingerprintedQName = {
    val fp: FingerprintedQName = new FingerprintedQName("", "", s)
    fp.obtainFingerprint(namePool)
    fp
  }

  /**
   * Create a QName in the JSON namespace
   *
   * @param s the local name
   * @return the QName
   */
  private def qnameNS(s: String): FingerprintedQName = {
    val fp: FingerprintedQName = new FingerprintedQName(PREFIX, JSON_NS, s)
    fp.obtainFingerprint(namePool)
    fp
  }

  init(xpathContext, flags)

  builder.setSystemId(staticBaseUri)

  builder.setTiming(false)

  out.open()

  out.startDocument(ReceiverOption.NONE)

  private def init(context: XPathContext, flags: Int): Unit = {
    keys = new Stack[String]()
    this.xpathContext = context
    charChecker = context.getConfiguration.getValidCharacterChecker
    escape = (flags & JsonParser.ESCAPE) != 0
    allowAnyTopLevel = (flags & JsonParser.ALLOW_ANY_TOP_LEVEL) != 0
    validate = (flags & JsonParser.VALIDATE) != 0
    checkForDuplicates = validate || (flags & JsonParser.DUPLICATES_RETAINED) == 0
    types = new HashMap()
    namePool = context.getConfiguration.getNamePool
    mapQN = qnameNS("map")
    arrayQN = qnameNS("array")
    stringQN = qnameNS("string")
    numberQN = qnameNS("number")
    booleanQN = qnameNS("boolean")
    nullQN = qnameNS("null")
    keyQN = qname("key")
    escapedQN = qname("escaped")
    escapedKeyQN = qname("escaped-key")
    if (validate) {
      var config: Configuration = context.getConfiguration
      config.synchronized {
        config.checkLicensedFeature(Configuration.LicenseFeature.SCHEMA_VALIDATION, "validation", -1)
        if (!config.isSchemaAvailable(JSON_NS)) {
          val is: InputSource =
            new StandardEntityResolver(config).resolveEntity(null, SCHEMA_URI)
          if (config.isTiming) {
            config.getLogger.info(
              "Loading a schema from resources for: " + JSON_NS)
          }
          config.addSchemaSource(new SAXSource(is))
        }
      }
      val typeNames: Array[String] = Array(
        "mapType",
        "arrayType",
        "stringType",
        "numberType",
        "booleanType",
        "nullType",
        "mapWithinMapType",
        "arrayWithinMapType",
        "stringWithinMapType",
        "numberWithinMapType",
        "booleanWithinMapType",
        "nullWithinMapType"
      )
      for (t <- typeNames) {
        setType(t,
          config.getSchemaType(new StructuredQName(PREFIX, JSON_NS, t)))
      }
    }
  }

  /**
   * Record a SchemaType for a particular name
   *
   * @param name the name to be used for the type, e.g. "arrayType"
   * @param st   the schema type to be used for typing such entities
   */
  def setType(name: String, st: SchemaType): Unit = {
    types.put(name, st)
  }

  /**
   * Set the key to be written for the next entry in an object/map
   *
   * @param unEscaped the key for the entry (null implies no key) in unescaped form (backslashes,
   *                  if present, do not signal an escape sequence)
   * @param reEscaped the key for the entry (null implies no key) in reescaped form. In this form
   *                  special characters are represented as backslash-escaped sequences if the escape
   *                  option is yes; if escape=no, the reEscaped form is the same as the unEscaped form.
   * @return true if the key is already present in the map, false if it is not
   */
  override def setKey(unEscaped: String, reEscaped: String): Boolean = {
    this.keys.push(unEscaped)
    checkForDuplicates && !mapKeys.peek().add(reEscaped)
  }

  /**
   * Return the complete parsed result
   *
   * @return the XML document for this JSON
   * @throws XPathException if an error occurs downstream
   */
  override def getResult(): Item = {
    out.endDocument()
    out.close()
    builder.getCurrentRoot
  }

  /**
   * Check whether a string contains an escape sequence
   *
   * @param literal the string to be checked
   * @return true if the string contains a backslash
   */
  private def containsEscape(literal: String): Boolean =
    literal.indexOf('\\') >= 0

  private def isInMap: Boolean = !inMap.isEmpty && inMap.peek()

  private def startElement(qn: FingerprintedQName, typeName: String): Unit = {
    startElement(qn, types.get(typeName))
  }

  private def startElement(qn: FingerprintedQName, st: SchemaType): Unit = {
    out.startElement(qn,
      if (validate && st != null) st else UNTYPED,
      Loc.NONE,
      ReceiverOption.NONE)
    if (isInMap) {
      var k: String = keys.pop()
      k = reEscape(k)
      if (escape) {
        markAsEscaped(k, isKey = true)
      }
      out.attribute(keyQN,
        if (validate) STRING_TYPE else SIMPLE_TYPE,
        k,
        Loc.NONE,
        ReceiverOption.NONE)
    }
  }

  private def startContent(): Unit = {
    out.startContent()
  }

  private def characters(s: String): Unit = {
    out.characters(s, Loc.NONE, ReceiverOption.NONE)
  }

  private def endElement(): Unit = {
    out.endElement()
  }

  override def startArray(): Unit = {
    startElement(arrayQN, if (isInMap) "arrayWithinMapType" else "arrayType")
    inMap.push(false)
    startContent()
  }

  override def endArray(): Unit = {
    inMap.pop()
    endElement()
  }

  override def startMap(): Unit = {
    startElement(mapQN, if (isInMap) "mapWithinMapType" else "mapType")
    if (checkForDuplicates) {
      mapKeys.push(new HashSet())
    }
    inMap.push(true)
    startContent()
  }

  override def endMap(): Unit = {
    inMap.pop()
    if (checkForDuplicates) {
      mapKeys.pop()
    }
    endElement()
  }

  override def writeNumeric(asString: String, asDouble: Double): Unit = {
    startElement(numberQN,
      if (isInMap) "numberWithinMapType" else "numberType")
    startContent()
    characters(asString)
    endElement()
  }

  override def writeString(`val`: String): Unit = {
    startElement(stringQN,
      if (isInMap) "stringWithinMapType" else "stringType")
    val escaped: CharSequence = reEscape(`val`)
    if (escape) {
      markAsEscaped(escaped, isKey = false)
    }
    startContent()
    characters(escaped.toString)
    endElement()
  }

   override def markAsEscaped(escaped: CharSequence,
                                       isKey: Boolean): Unit = {
    if (containsEscape(escaped.toString) && escape) {
      val name: NodeName = if (isKey) escapedKeyQN else escapedQN
      out.attribute(name,
        if (validate) BOOLEAN_TYPE else SIMPLE_TYPE,
        "true",
        Loc.NONE,
        ReceiverOption.NONE)
    }
  }

  override def writeBoolean(value: Boolean): Unit = {
    startElement(booleanQN,
      if (isInMap) "booleanWithinMapType" else "booleanType")
    startContent()
    characters(java.lang.Boolean.toString(value))
    endElement()
  }

  override def writeNull(): Unit = {
    startElement(nullQN, if (isInMap) "nullWithinMapType" else "nullType")
    startContent()
    endElement()
  }

}
