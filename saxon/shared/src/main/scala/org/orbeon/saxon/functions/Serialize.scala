////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// Copyright (c) 2018-2020 Saxonica Limited
// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is "Incompatible With Secondary Licenses", as defined by the Mozilla Public License, v. 2.0.
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
package org.orbeon.saxon.functions

import org.orbeon.saxon.event.SequenceCopier
import org.orbeon.saxon.expr.parser.{Loc, RoleDiagnostic}
import org.orbeon.saxon.expr.{StaticProperty, XPathContext}
import org.orbeon.saxon.lib.{NamespaceConstant, SaxonOutputKeys}
import org.orbeon.saxon.ma.map.{HashTrieMap, MapItem, MapType}
import org.orbeon.saxon.model.{BuiltInAtomicType, Type}
import org.orbeon.saxon.om._
import org.orbeon.saxon.regex.UnicodeString
import org.orbeon.saxon.serialize.{CharacterMap, CharacterMapIndex, SerializationParamsHandler, SerializationProperties}
import org.orbeon.saxon.trans.{Err, XPathException}
import org.orbeon.saxon.value._
import org.orbeon.saxon.z.IntHashMap

import java.io.StringWriter
import java.util
import java.util.Properties
import javax.xml.transform.OutputKeys
import javax.xml.transform.stream.StreamResult
import scala.jdk.CollectionConverters._
import scala.util.control.Breaks._


/**
 * Implementation of fn:serialize() as defined in XPath 3.1
 */
object Serialize {
  def makeOptionsParameter: OptionsParameter = {
    val listOfQNames = BuiltInAtomicType.QNAME.zeroOrMore
    val op = new OptionsParameter
    op.addAllowedOption("allow-duplicate-names",        SequenceType.SINGLE_BOOLEAN) //yes-no-param-type
    op.addAllowedOption("byte-order-mark",              SequenceType.SINGLE_BOOLEAN)
    op.addAllowedOption("cdata-section-elements",       listOfQNames, EmptySequence.getInstance)
    //QNames-param-type - sequence or an array of xs:QName values. Note that an array will be converted to a sequence, as required
    op.addAllowedOption("doctype-public",               SequenceType.SINGLE_STRING) //doctype-public-param-type pubid-char-string-type
    op.addAllowedOption("doctype-system",               SequenceType.SINGLE_STRING) //doctype-system-param-type system-id-string-type
    op.addAllowedOption("encoding",                     SequenceType.SINGLE_STRING) //encoding-param-type encoding-string-type
    op.addAllowedOption("escape-uri-attributes",        SequenceType.SINGLE_BOOLEAN)
    op.addAllowedOption("html-version",                 SequenceType.SINGLE_DECIMAL) //decimal-param-type
    op.addAllowedOption("include-content-type",         SequenceType.SINGLE_BOOLEAN)
    op.addAllowedOption("indent",                       SequenceType.SINGLE_BOOLEAN)
    op.addAllowedOption("item-separator",               SequenceType.SINGLE_STRING) //string-param-type
    op.addAllowedOption("json-node-output-method",      SequenceType.SINGLE_STRING)
    //json-node-output-method-param-type  json-node-output-method-type - xs:string or xs:QName
    op.addAllowedOption("media-type",                   SequenceType.SINGLE_STRING)
    op.addAllowedOption("method",                       SequenceType.SINGLE_STRING)
    //method-param-type method-type - xs:string or xs:QName
    op.addAllowedOption("normalization-form",           SequenceType.SINGLE_STRING)
    //NMTOKEN-param-type  BuiltInAtomicType.NMTOKEN
    op.addAllowedOption("omit-xml-declaration",         SequenceType.SINGLE_BOOLEAN)
    op.addAllowedOption("standalone",                   SequenceType.OPTIONAL_BOOLEAN) //yes-no-omit-type
    op.addAllowedOption("suppress-indentation",         listOfQNames)
    op.addAllowedOption("undeclare-prefixes",           SequenceType.SINGLE_BOOLEAN)
    op.addAllowedOption("use-character-maps",           SequenceType.makeSequenceType(MapType.ANY_MAP_TYPE, StaticProperty.EXACTLY_ONE))
    //use-character-maps-param-type
    op.addAllowedOption("version",                      SequenceType.SINGLE_STRING)
    op.addAllowedOption(sx("attribute-order"),          SequenceType.ATOMIC_SEQUENCE)
    //eqnames
    op.addAllowedOption(sx("character-representation"), SequenceType.SINGLE_STRING) //string
    op.addAllowedOption(sx("double-space"),             listOfQNames)
    op.addAllowedOption(sx("indent-spaces"),            SequenceType.SINGLE_INTEGER) //integer
    op.addAllowedOption(sx("line-length"),              SequenceType.SINGLE_INTEGER)
    //requiredTypes.put("next-in-chain", SequenceType.SINGLE_STRING); //uri
    op.addAllowedOption(sx("property-order"),           SequenceType.STRING_SEQUENCE)
    op.addAllowedOption(sx("recognize-binary"),         SequenceType.SINGLE_BOOLEAN) //boolean
    op.addAllowedOption(sx("require-well-formed"),      SequenceType.SINGLE_BOOLEAN)
    op.addAllowedOption(sx("single-quotes"),            SequenceType.SINGLE_BOOLEAN)
    op.addAllowedOption(sx("supply-source-locator"),    SequenceType.SINGLE_BOOLEAN)
    op
  }

  private def sx(s: String): String = "Q{" + NamespaceConstant.SAXON + "}" + s

  private val requiredTypes = new util.HashMap[String, SequenceType](40)
  private val requiredTypesSaxon = new util.HashMap[String, SequenceType](20)

  /**
   * By the option parameter conventions, check the 'options' supplied in a character map:
   * 1. any string is allowed as an option, QNames not recognised by product are ignored;
   * 2. validate the types of the option values supplied (required type is always xs:string).
   */
  @throws[XPathException]
  private def checkCharacterMapOptions(map: MapItem, context: XPathContext): MapItem = {

    val th = context.getConfiguration.getTypeHierarchy

    for (pair <- map.keyValuePairs.asScala) {
      val key = pair.key
      if (!key.isInstanceOf[StringValue])
        throw new XPathException("Keys in a character map must all be strings. Found a value of type " + key.getItemType, "XPTY0004")
      if (key.asInstanceOf[StringValue].getStringLength != 1)
        throw new XPathException("Keys in a character map must all be one-character strings. Found " + Err.wrap(key.toString), "SEPM0016")
      if (!SequenceType.SINGLE_STRING.matches(pair.value, th))
        throw new XPathException("Values in a character map must all be single strings. Found " + Err.wrap(key.toString), "XPTY0004")
    }
    map
  }

  @throws[XPathException]
  def toCharacterMap(charMap: MapItem): CharacterMap = {
    val iterator = charMap.keys
    var charKey: AtomicValue = null
    val intHashMap = new IntHashMap[String]
    while ( {
      charKey = iterator.next()
      charKey
    } != null) {
      val ch = charKey.getStringValue
      val str = charMap.get(charKey).head.getStringValue
      val chValue = UnicodeString.makeUnicodeString(ch)
      if (chValue.uLength != 1)
        throw new XPathException("In the serialization parameter for the character map, each character to be mapped " +
          "must be a single Unicode character", "SEPM0016")
      val code = chValue.uCharAt(0)
      val prev = intHashMap.put(code, str)
      if (prev != null) {
        // This should never happen in this case because keys in a HashTrieMap must be unique
        throw new XPathException("In the serialization parameters, the character map contains two entries for the character \\u" + Integer.toHexString(65536 + code).substring(1), "SEPM0018")
      }
    }
    val name = new StructuredQName("output", NamespaceConstant.OUTPUT, "serialization-parameters")
    new CharacterMap(name, intHashMap)
  }

  requiredTypes.put("allow-duplicate-names", SequenceType.SINGLE_BOOLEAN)
  requiredTypes.put("byte-order-mark", SequenceType.SINGLE_BOOLEAN)
  requiredTypes.put("cdata-section-elements", BuiltInAtomicType.QNAME.zeroOrMore)
  requiredTypes.put("doctype-public", SequenceType.SINGLE_STRING)
  requiredTypes.put("doctype-system", SequenceType.SINGLE_STRING)
  requiredTypes.put("encoding", SequenceType.SINGLE_STRING)
  requiredTypes.put("escape-uri-attributes", SequenceType.SINGLE_BOOLEAN)
  requiredTypes.put("html-version", SequenceType.SINGLE_DECIMAL)
  requiredTypes.put("include-content-type", SequenceType.SINGLE_BOOLEAN)
  requiredTypes.put("indent", SequenceType.SINGLE_BOOLEAN)
  requiredTypes.put("item-separator", SequenceType.SINGLE_STRING)
  requiredTypes.put("json-node-output-method", SequenceType.SINGLE_STRING)
  requiredTypes.put("media-type", SequenceType.SINGLE_STRING)
  requiredTypes.put("method", SequenceType.SINGLE_STRING)
  requiredTypes.put("normalization-form", SequenceType.SINGLE_STRING)
  requiredTypes.put("omit-xml-declaration", SequenceType.SINGLE_BOOLEAN)
  requiredTypes.put("standalone", SequenceType.OPTIONAL_BOOLEAN)
  requiredTypes.put("suppress-indentation", BuiltInAtomicType.QNAME.zeroOrMore)
  requiredTypes.put("undeclare-prefixes", SequenceType.SINGLE_BOOLEAN)
  requiredTypes.put("use-character-maps", SequenceType.makeSequenceType(MapType.ANY_MAP_TYPE, StaticProperty.EXACTLY_ONE))
  requiredTypes.put("version", SequenceType.SINGLE_STRING)
  requiredTypesSaxon.put("attribute-order", BuiltInAtomicType.QNAME.zeroOrMore)
  requiredTypesSaxon.put("character-representation", SequenceType.SINGLE_STRING)
  requiredTypesSaxon.put("double-space", BuiltInAtomicType.QNAME.zeroOrMore)
  requiredTypesSaxon.put("indent-spaces", SequenceType.SINGLE_INTEGER)
  requiredTypesSaxon.put("line-length", SequenceType.SINGLE_INTEGER)
  requiredTypesSaxon.put("recognize-binary", SequenceType.SINGLE_BOOLEAN)
  requiredTypesSaxon.put("require-well-formed", SequenceType.SINGLE_BOOLEAN)
  requiredTypesSaxon.put("single-quotes", SequenceType.SINGLE_BOOLEAN)
  requiredTypesSaxon.put("supply-source-locator", SequenceType.SINGLE_BOOLEAN)
  requiredTypesSaxon.put("suppress-indentation", BuiltInAtomicType.QNAME.zeroOrMore)
}

class Serialize extends SystemFunction {
  private val paramNames = Array[String](
    "allow-duplicate-names", "byte-order-mark", "cdata-section-elements", "doctype-public", "doctype-system",
    "encoding", "escape-uri-attributes", "html-version", "include-content-type", "indent", "item-separator",
    "json-node-output-method", "media-type", "method", "normalization-form", "omit-xml-declaration", "standalone",
    "suppress-indentation", "undeclare-prefixes", "use-character-maps", "version"
  )

  private def isParamName(string: String): Boolean = {
    for (s <- paramNames) {
      if (s == string) return true
    }
    false
  }

  private val paramNamesSaxon = Array[String](
    "attribute-order", "character-representation", "double-space", "indent-spaces", "line-length", /*"next-in-chain",*/
    "property-order", "recognize-binary", "require-well-formed", "single-quotes", "supply-source-locator",
    "suppress-indentation"
  )

  private def isParamNameSaxon(string: String): Boolean = {
    for (s <- paramNamesSaxon)
      if (s == string)
        return true
    false
  }

  /**
   * Check the options supplied:
   * 1. ignore any other options not in the specs;
   * 2. validate the types of the option values supplied.
   */
  @throws[XPathException]
  private def checkOptions(map: MapItem, context: XPathContext): HashTrieMap = {
    var result = new HashTrieMap
    val th = context.getConfiguration.getTypeHierarchy
    val keysIterator = map.keys
    var key: AtomicValue = null
    while ({
      key = keysIterator.next()
      key
    } != null)
      breakable {
        key match {
          case _: StringValue    =>
            val keyName = key.getStringValue
            if (isParamName(keyName)) {
              val role = new RoleDiagnostic(RoleDiagnostic.OPTION, keyName, 0)
              role.setErrorCode("XPTY0004")
              val converted = th.applyFunctionConversionRules(
                map.get(key), Serialize.requiredTypes.get(keyName), role, Loc.NONE
              )
              result = result.addEntry(key, converted.materialize)
            }
          case qNameValue: QNameValue =>
            if (key.getComponent(AccessorFn.Component.NAMESPACE).getStringValue == "")
              throw new XPathException(
                "A serialization parameter supplied with a QName key must have non-absent namespace", "SEPM0017"
              )
            else if (key.getComponent(AccessorFn.Component.NAMESPACE).getStringValue == "http://saxon.sf.net/") {
              // Capture Saxon serialization parameters
              val keyName = qNameValue.getLocalName
              if (isParamNameSaxon(keyName)) {
                val role      = new RoleDiagnostic(RoleDiagnostic.OPTION, keyName, 0)
                val converted = th.applyFunctionConversionRules(
                  map.get(key), Serialize.requiredTypesSaxon.get(keyName), role, Loc.NONE
                )
                result = result.addEntry(key, converted.materialize)
              }
            }
          // Implementation-defined serialization parameters in an unrecognised namespace are ignored.
          case _ => break()
        }
      }
    result
  }

  @throws[XPathException]
  private def toYesNoTypeString(seqVal: Sequence) = {
    val booleanValue = seqVal.head.asInstanceOf[BooleanValue].getBooleanValue
    if (booleanValue)
      "yes"
    else
      "no"
  }

  @throws[XPathException]
  private def toYesNoOmitTypeString(seqVal: Sequence) =
    if (seqVal.isInstanceOf[EmptySequence.type])
      "omit"
    else if (seqVal.head.isInstanceOf[BooleanValue])
      toYesNoTypeString(seqVal)
    else
      "" // otherwise invalid

  @throws[XPathException]
  private def toQNamesTypeString(seqVal: Sequence, allowStar: Boolean) = {
    val iterator = seqVal.iterate()
    var item: Item = null
    val stringVal = new StringBuilder
    while ({
      item = iterator.next()
      item
    } != null)
      item match {
        case qNameValue: QNameValue =>
          stringVal
            .append(" Q{")
            .append(qNameValue.getComponent(AccessorFn.Component.NAMESPACE).getStringValue)
            .append('}')
            .append(qNameValue.getComponent(AccessorFn.Component.LOCALNAME).getStringValue)
        case _                      =>
         if (allowStar && item.isInstanceOf[StringValue] && item.getStringValue == "*")
           stringVal.append(" *")
         else
           throw new XPathException(
             "Invalid serialization parameter value: expected sequence of QNames " + (if (allowStar) "(or *) " else ""),
             "SEPM0017"
           )
      }
    stringVal.toString
  }

  @throws[XPathException]
  private def toSpaceSeparatedString(seqVal: Sequence) = {
    val iterator = seqVal.iterate()
    var item: Item = null
    val stringVal = new StringBuilder
    while ({
      item = iterator.next()
      item
    } != null
    ) stringVal.append(" ").append(item.getStringValue)
    stringVal.toString
  }

  @throws[XPathException]
  private def toMethodTypeString(seqVal: Sequence) = {
    var stringVal: String = null
    seqVal.head match {
      case qNameValue: QNameValue =>
        stringVal = "{" + qNameValue.getComponent(AccessorFn.Component.NAMESPACE
        ).toString + "}" + qNameValue.getComponent(AccessorFn.Component.LOCALNAME)
      case _ => stringVal = seqVal.head.getStringValue
    }
    stringVal
  }

  @throws[XPathException]
  private def toCharacterMap(seqVal: Sequence, context: XPathContext) = {
    val charMap = Serialize.checkCharacterMapOptions(seqVal.head.asInstanceOf[MapItem], context)
    Serialize.toCharacterMap(charMap)
  }

  @throws[XPathException]
  private def serializationParamsFromMap(map: util.Map[String, Sequence], context: XPathContext): SerializationProperties = {
    var seqVal: Sequence = null
    val props = new Properties
    val charMapIndex = new CharacterMapIndex
    if (({
      seqVal = map.get("allow-duplicate-names")
      seqVal
    }) != null) props.setProperty("allow-duplicate-names", toYesNoTypeString(seqVal))
    if (({
      seqVal = map.get("byte-order-mark")
      seqVal
    }) != null) props.setProperty("byte-order-mark", toYesNoTypeString(seqVal))
    if (({
      seqVal = map.get("cdata-section-elements")
      seqVal
    }) != null) props.setProperty("cdata-section-elements", toQNamesTypeString(seqVal, allowStar = false))
    if (({
      seqVal = map.get("doctype-public")
      seqVal
    }) != null) props.setProperty("doctype-public", seqVal.head.getStringValue)
    if (({
      seqVal = map.get("doctype-system")
      seqVal
    }) != null) props.setProperty("doctype-system", seqVal.head.getStringValue)
    if (({
      seqVal = map.get("encoding")
      seqVal
    }) != null) props.setProperty("encoding", seqVal.head.getStringValue)
    if (({
      seqVal = map.get("escape-uri-attributes")
      seqVal
    }) != null) props.setProperty("escape-uri-attributes", toYesNoTypeString(seqVal))
    if (({
      seqVal = map.get("html-version")
      seqVal
    }) != null) props.setProperty("html-version", seqVal.head.getStringValue)
    if (({
      seqVal = map.get("include-content-type")
      seqVal
    }) != null) props.setProperty("include-content-type", toYesNoTypeString(seqVal))
    if (({
      seqVal = map.get("indent")
      seqVal
    }) != null) props.setProperty("indent", toYesNoTypeString(seqVal))
    if (({
      seqVal = map.get("item-separator")
      seqVal
    }) != null) props.setProperty("item-separator", seqVal.head.getStringValue)
    if (({
      seqVal = map.get("json-node-output-method")
      seqVal
    }) != null) props.setProperty("json-node-output-method", toMethodTypeString(seqVal))
    if (({
      seqVal = map.get("media-type")
      seqVal
    }) != null) props.setProperty("media-type", seqVal.head.getStringValue)
    if (({
      seqVal = map.get("method")
      seqVal
    }) != null) props.setProperty(OutputKeys.METHOD, toMethodTypeString(seqVal))
    if (({
      seqVal = map.get("normalization-form")
      seqVal
    }) != null) props.setProperty("normalization-form", seqVal.head.getStringValue) //NMTOKEN param type
    if (({
      seqVal = map.get("omit-xml-declaration")
      seqVal
    }) != null) props.setProperty("omit-xml-declaration", toYesNoTypeString(seqVal))
    if (({
      seqVal = map.get("standalone")
      seqVal
    }) != null) props.setProperty("standalone", toYesNoOmitTypeString(seqVal))
    if (({
      seqVal = map.get("suppress-indentation")
      seqVal
    }) != null) props.setProperty("suppress-indentation", toQNamesTypeString(seqVal, allowStar = false))
    if (({
      seqVal = map.get("undeclare-prefixes")
      seqVal
    }) != null) props.setProperty("undeclare-prefixes", toYesNoTypeString(seqVal))
    if (({
      seqVal = map.get("use-character-maps")
      seqVal
    }) != null) {
      val characterMap = toCharacterMap(seqVal, context)
      charMapIndex.putCharacterMap(new StructuredQName("", "", "charMap"), characterMap)
      props.setProperty(SaxonOutputKeys.USE_CHARACTER_MAPS, "charMap")
    }
    if (({
      seqVal = map.get("version")
      seqVal
    }) != null) props.setProperty("version", seqVal.head.getStringValue)
    // Saxon extension serialization parameters
    if (({
      seqVal = map.get(Serialize.sx("attribute-order"))
      seqVal
    }) != null) props.setProperty(SaxonOutputKeys.ATTRIBUTE_ORDER, toQNamesTypeString(seqVal, allowStar = true))
    if (({
      seqVal = map.get(Serialize.sx("canonical"))
      seqVal
    }) != null) props.setProperty(SaxonOutputKeys.CANONICAL, toYesNoTypeString(seqVal))
    if (({
      seqVal = map.get(Serialize.sx("character-representation"))
      seqVal
    }) != null) props.setProperty(SaxonOutputKeys.CHARACTER_REPRESENTATION, seqVal.head.getStringValue)
    if (({
      seqVal = map.get(Serialize.sx("double-space"))
      seqVal
    }) != null) props.setProperty(SaxonOutputKeys.DOUBLE_SPACE, toQNamesTypeString(seqVal, allowStar = false))
    if (({
      seqVal = map.get(Serialize.sx("indent-spaces"))
      seqVal
    }) != null) props.setProperty(SaxonOutputKeys.INDENT_SPACES, seqVal.head.getStringValue)
    if (({
      seqVal = map.get(Serialize.sx("line-length"))
      seqVal
    }) != null) props.setProperty(SaxonOutputKeys.LINE_LENGTH, seqVal.head.getStringValue)
    if (({
      seqVal = map.get(Serialize.sx("property-order"))
      seqVal
    }) != null) props.setProperty(SaxonOutputKeys.PROPERTY_ORDER, toSpaceSeparatedString(seqVal))
    if (({
      seqVal = map.get(Serialize.sx("recognize-binary"))
      seqVal
    }) != null) props.setProperty(SaxonOutputKeys.RECOGNIZE_BINARY, toYesNoTypeString(seqVal))
    if (({
      seqVal = map.get(Serialize.sx("require-well-formed"))
      seqVal
    }) != null) props.setProperty(SaxonOutputKeys.REQUIRE_WELL_FORMED, toYesNoTypeString(seqVal))
    if (({
      seqVal = map.get(Serialize.sx("single-quotes"))
      seqVal
    }) != null) props.setProperty(SaxonOutputKeys.SINGLE_QUOTES, toYesNoTypeString(seqVal))
    if (({
      seqVal = map.get(Serialize.sx("supply-source-locator"))
      seqVal
    }) != null) props.setProperty(SaxonOutputKeys.SUPPLY_SOURCE_LOCATOR, toYesNoTypeString(seqVal))
    new SerializationProperties(props, charMapIndex)
  }

  @throws[XPathException]
  override def call(context: XPathContext, arguments: Array[Sequence]): Sequence =
    evalSerialize(arguments(0).iterate(), if (arguments.length == 1) null else arguments(1).head, context)

  @throws[XPathException]
  private def evalSerialize(iter: SequenceIterator, param: Item, context: XPathContext): StringValue = {
    // The default parameter values are implementation-defined when an output:serialization-parameters
    // element is used (or when the argument is omitted), but are fixed by this specification in the
    // case where a map (including an empty map) is supplied for the argument.
    val params =
    if (param != null)
      param match {
        case paramNode: NodeInfo =>
          if (paramNode.getNodeKind != Type.ELEMENT || !(NamespaceConstant.OUTPUT == paramNode.getURI) || !("serialization-parameters" == paramNode.getLocalPart))
            throw new XPathException(
              "Second argument to fn:serialize() must be an element named {" + NamespaceConstant.OUTPUT + "}serialization-parameters",
              "XPTY0004"
            )
          val sph = new SerializationParamsHandler
          sph.setSerializationParams(paramNode)
          sph.getSerializationProperties
        case item: MapItem =>
          // If any parameters are supplied as QNames in the Saxon namespace, convert them to EQName strings
          var paramMap = item
          val keyIter = item.keys
          var k: AtomicValue = null
          while ({
            k = keyIter.next()
            k
          } != null)
            k match {
              case qNameValue: QNameValue =>
                val s = qNameValue.getStructuredQName.getEQName
                paramMap = paramMap.addEntry(new StringValue(s), paramMap.get(k))
              case _ =>
            }
          val checkedOptions = getDetails.optionDetails.processSuppliedOptions(paramMap, context)
          serializationParamsFromMap(checkedOptions, context)
        case _ =>
          throw new XPathException(
            "Second argument to fn:serialize() must either be an element named {" + NamespaceConstant.OUTPUT + "}serialization-parameters, or a map (if using XPath 3.1)",
            "XPTY0004"
          )
      }
    else
      new SerializationProperties(new Properties)

    val props = params.getProperties
    if (props.getProperty(OutputKeys.METHOD) == null)
      props.setProperty(OutputKeys.METHOD, "xml")
    if (props.getProperty(OutputKeys.OMIT_XML_DECLARATION) == null)
      props.setProperty(OutputKeys.OMIT_XML_DECLARATION, "true")
    // TODO add more spec-defined defaults here (for both cases)

    try {
      val result = new StringWriter
      val sf     = context.getConfiguration.getSerializerFactory
      val pipe   = context.getConfiguration.makePipelineConfiguration
      val out    = sf.getReceiver(new StreamResult(result), params, pipe)
      SequenceCopier.copySequence(iter, out)
      new StringValue(result.toString)
    } catch {
      case e: XPathException =>
        e.maybeSetErrorCode("SENR0001")
        throw e
    }
  }
}
