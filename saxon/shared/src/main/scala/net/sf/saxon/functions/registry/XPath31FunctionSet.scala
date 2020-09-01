////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
package net.sf.saxon.functions.registry

import net.sf.saxon.functions._
import net.sf.saxon.functions.hof.{LoadXqueryModule, RandomNumberGenerator, Sort_3}
import net.sf.saxon.functions.registry.BuiltInFunctionSet._
import net.sf.saxon.ma.json.{JsonDoc, JsonToXMLFn, ParseJsonFn, XMLToJsonFn}
import net.sf.saxon.ma.map.MapType
import net.sf.saxon.model.{AnyItemType, BuiltInAtomicType, SpecificFunctionType, Type}
import net.sf.saxon.pattern.{AnyNodeTest, NodeKindTest}
import net.sf.saxon.value.{SequenceType, StringValue}


object XPath31FunctionSet {

  private val THE_INSTANCE: XPath31FunctionSet = new XPath31FunctionSet()

  def getInstance(): XPath31FunctionSet = THE_INSTANCE
}

class XPath31FunctionSet private() extends BuiltInFunctionSet {

  init()

  private def init(): Unit = {
    importFunctionSet(XPath20FunctionSet.getInstance)
    importFunctionSet(XPath30FunctionSet.getInstance)
    var ft: SpecificFunctionType = null
    register("collation-key",
      1,
      classOf[CollationKeyFn],
      BuiltInAtomicType.BASE64_BINARY,
      OPT,
      DCOLL).arg(0, BuiltInAtomicType.STRING, ONE, null)
    register("collation-key",
      2,
      classOf[CollatingFunctionFree],
      BuiltInAtomicType.BASE64_BINARY,
      OPT,
      DCOLL)
      .arg(0, BuiltInAtomicType.STRING, ONE, null)
      .arg(1, BuiltInAtomicType.STRING, ONE, null)
    register("contains-token",
      2,
      classOf[ContainsToken],
      BuiltInAtomicType.BOOLEAN,
      ONE,
      DCOLL)
      .arg(0, BuiltInAtomicType.STRING, STAR, null)
      .arg(1, BuiltInAtomicType.STRING, ONE, null)
    register("contains-token",
      3,
      classOf[CollatingFunctionFree],
      BuiltInAtomicType.BOOLEAN,
      ONE,
      BASE)
      .arg(0, BuiltInAtomicType.STRING, STAR, null)
      .arg(1, BuiltInAtomicType.STRING, ONE, null)
      .arg(2, BuiltInAtomicType.STRING, ONE, null)
    register("copy-of",
      0,
      classOf[CopyOfFn],
      AnyItemType,
      STAR,
      NEW)
    register("copy-of",
      1,
      classOf[CopyOfFn],
      AnyItemType,
      STAR,
      NEW).arg(0, AnyItemType, STAR | ABS, EMPTY)
    register("default-language",
      0,
      classOf[DynamicContextAccessor.DefaultLanguage],
      BuiltInAtomicType.LANGUAGE,
      ONE,
      DLANG)
    register("generate-id",
      0,
      classOf[ContextItemAccessorFunction],
      BuiltInAtomicType.STRING,
      ONE,
      CITEM | LATE)
    register("generate-id",
      1,
      classOf[GenerateId_1],
      BuiltInAtomicType.STRING,
      ONE,
      0).arg(0, Type.NODE_TYPE, OPT | INS, StringValue.EMPTY_STRING)
    register("has-children",
      0,
      classOf[ContextItemAccessorFunction],
      BuiltInAtomicType.BOOLEAN,
      ONE,
      CITEM | LATE)
    register("has-children",
      1,
      classOf[HasChildren_1],
      BuiltInAtomicType.BOOLEAN,
      OPT,
      0).arg(0, AnyNodeTest.getInstance, OPT | INS, null)
    register("head", 1, classOf[HeadFn], AnyItemType, OPT, FILTER)
      .arg(0, AnyItemType, STAR | TRA, null)
    register("innermost",
      1,
      classOf[Innermost],
      AnyNodeTest.getInstance,
      STAR,
      0).arg(0, AnyNodeTest.getInstance, STAR | NAV, null)
    register("json-doc",
      1,
      classOf[JsonDoc],
      AnyItemType,
      OPT,
      LATE).arg(0, BuiltInAtomicType.STRING, OPT, null)
    register("json-doc",
      2,
      classOf[JsonDoc],
      AnyItemType,
      OPT,
      LATE)
      .arg(0, BuiltInAtomicType.STRING, OPT, null)
      .arg(1, MapType.ANY_MAP_TYPE, ONE, null)
      .optionDetails(ParseJsonFn.OPTION_DETAILS)
    register("json-to-xml",
      1,
      classOf[JsonToXMLFn],
      AnyItemType,
      OPT,
      LATE | NEW).arg(0, BuiltInAtomicType.STRING, OPT, null)
    register("json-to-xml",
      2,
      classOf[JsonToXMLFn],
      AnyItemType,
      OPT,
      LATE | NEW)
      .arg(0, BuiltInAtomicType.STRING, OPT, null)
      .arg(1, MapType.ANY_MAP_TYPE, ONE, null)
      .optionDetails(JsonToXMLFn.OPTION_DETAILS)
    register("load-xquery-module",
      1,
      classOf[LoadXqueryModule],
      MapType.ANY_MAP_TYPE,
      ONE,
      LATE).arg(0, BuiltInAtomicType.STRING, ONE, null)
    register("load-xquery-module",
      2,
      classOf[LoadXqueryModule],
      MapType.ANY_MAP_TYPE,
      ONE,
      LATE)
      .arg(0, BuiltInAtomicType.STRING, ONE, null)
      .arg(1, MapType.ANY_MAP_TYPE, ONE, EMPTY)
      .optionDetails(LoadXqueryModule.makeOptionsParameter())
    register("parse-ietf-date",
      1,
      classOf[ParseIetfDate],
      BuiltInAtomicType.DATE_TIME,
      OPT,
      0).arg(0, BuiltInAtomicType.STRING, OPT, EMPTY)
    register("parse-json",
      1,
      classOf[ParseJsonFn],
      AnyItemType,
      OPT,
      0).arg(0, BuiltInAtomicType.STRING, OPT, EMPTY)
    register("parse-json",
      2,
      classOf[ParseJsonFn],
      AnyItemType,
      OPT,
      0)
      .arg(0, BuiltInAtomicType.STRING, OPT, EMPTY)
      .arg(1, MapType.ANY_MAP_TYPE, ONE, null)
      .optionDetails(ParseJsonFn.OPTION_DETAILS)
    register("parse-xml",
      1,
      classOf[ParseXml],
      NodeKindTest.DOCUMENT,
      OPT,
      LATE).arg(0, BuiltInAtomicType.STRING, OPT, EMPTY)
    register("random-number-generator",
      0,
      classOf[RandomNumberGenerator],
      RandomNumberGenerator.RETURN_TYPE,
      ONE,
      LATE)
    register("random-number-generator",
      1,
      classOf[RandomNumberGenerator],
      RandomNumberGenerator.RETURN_TYPE,
      ONE,
      LATE).arg(0, BuiltInAtomicType.ANY_ATOMIC, OPT, null)
    register("parse-xml-fragment",
      1,
      classOf[ParseXmlFragment],
      NodeKindTest.DOCUMENT,
      OPT,
      LATE).arg(0, BuiltInAtomicType.STRING, OPT, EMPTY)
    register("serialize",
      2,
      classOf[Serialize],
      BuiltInAtomicType.STRING,
      ONE,
      0)
      .arg(0, AnyItemType, STAR, null)
      .arg(1, Type.ITEM_TYPE, OPT, null)
      .optionDetails(Serialize.makeOptionsParameter)
    register("snapshot",
      0,
      classOf[ContextItemAccessorFunction],
      AnyItemType,
      STAR,
      CITEM | LATE | NEW)
    register("snapshot",
      1,
      classOf[SnapshotFn],
      AnyNodeTest.getInstance,
      STAR,
      NEW).arg(0, AnyItemType, STAR | ABS, EMPTY)
    register("sort", 1, classOf[Sort_1], AnyItemType, STAR, 0).arg(
      0,
      AnyItemType,
      STAR,
      null)
    register("sort", 2, classOf[Sort_2], AnyItemType, STAR, 0)
      .arg(0, AnyItemType, STAR, null)
      .arg(1, BuiltInAtomicType.STRING, OPT, null)
    ft = new SpecificFunctionType(Array(SequenceType.SINGLE_ITEM),
      SequenceType.ATOMIC_SEQUENCE)
    register("sort", 3, classOf[Sort_3], AnyItemType, STAR, 0)
      .arg(0, AnyItemType, STAR, null)
      .arg(1, BuiltInAtomicType.STRING, OPT, null)
      .arg(2, ft, ONE, null)
    register(
      "string-join",
      1,
      classOf[StringJoin],
      BuiltInAtomicType.STRING,
      ONE,
      0).arg(0, BuiltInAtomicType.ANY_ATOMIC, STAR, StringValue.EMPTY_STRING)
    register("string-join",
      2,
      classOf[StringJoin],
      BuiltInAtomicType.STRING,
      ONE,
      0)
      .arg(0, BuiltInAtomicType.ANY_ATOMIC, STAR, StringValue.EMPTY_STRING)
      .arg(1, BuiltInAtomicType.STRING, ONE, null)
    register("tokenize",
      1,
      classOf[Tokenize_1],
      BuiltInAtomicType.STRING,
      STAR,
      0).arg(0, BuiltInAtomicType.STRING, OPT, EMPTY)
    register("trace", 1, classOf[Trace], Type.ITEM_TYPE, STAR, AS_ARG0 | LATE)
      .arg(0, Type.ITEM_TYPE, STAR | TRA, null)
//    register("transform",
//      1,
//      classOf[TransformFn],
//      MapType.ANY_MAP_TYPE,
//      ONE,
//      LATE)
//      .arg(0, MapType.ANY_MAP_TYPE, ONE, EMPTY)
//      .optionDetails(TransformFn.makeOptionsParameter())
    register("xml-to-json",
      1,
      classOf[XMLToJsonFn],
      BuiltInAtomicType.STRING,
      OPT,
      LATE).arg(0, AnyNodeTest.getInstance, OPT | ABS, EMPTY)
    register("xml-to-json",
      2,
      classOf[XMLToJsonFn],
      BuiltInAtomicType.STRING,
      OPT,
      LATE)
      .arg(0, AnyNodeTest.getInstance, OPT | ABS, EMPTY)
      .arg(1, MapType.ANY_MAP_TYPE, ONE | ABS, null)
      .optionDetails(XMLToJsonFn.makeOptionsParameter())
  }

  // The copy-of function is defined in XSLT 3.0, but we choose to make it available also in XPath/XQuery
  // The snapshot function is defined in XSLT 3.0, but we choose to make it available also in XPath/XQuery
  // The copy-of function is defined in XSLT 3.0, but we choose to make it available also in XPath/XQuery
  // The snapshot function is defined in XSLT 3.0, but we choose to make it available also in XPath/XQuery

}

// Copyright (c) 2018-2020 Saxonica Limited
// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is "Incompatible With Secondary Licenses", as defined by the Mozilla Public License, v. 2.0.
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
/**
 * Function signatures (and pointers to implementations) of the functions defined in XPath 3.1 without the
 * Higher-Order-Functions feature
 */
