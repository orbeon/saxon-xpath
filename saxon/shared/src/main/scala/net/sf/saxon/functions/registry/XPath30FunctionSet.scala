////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// Copyright (c) 2018-2020 Saxonica Limited
// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is "Incompatible With Secondary Licenses", as defined by the Mozilla Public License, v. 2.0.
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
/**
  * Function signatures (and pointers to implementations) of the functions defined in XPath 3.0 without the
  * Higher-Order-Functions feature
  */

package net.sf.saxon.functions.registry

import net.sf.saxon.functions._
import net.sf.saxon.functions.hof._
import net.sf.saxon.functions.registry.BuiltInFunctionSet._
import net.sf.saxon.ma.arrays.ArrayItemType
import net.sf.saxon.model._
import net.sf.saxon.pattern.{AnyNodeTest, NodeKindTest}
import net.sf.saxon.value.{BooleanValue, SequenceType, StringValue}

//remove if not needed

object XPath30FunctionSet {

  private val THE_INSTANCE: XPath30FunctionSet = new XPath30FunctionSet()
  def getInstance(): XPath30FunctionSet = THE_INSTANCE
}

class XPath30FunctionSet private () extends BuiltInFunctionSet {

  init()

  private def init(): Unit = {
    importFunctionSet(XPath20FunctionSet.getInstance)
    register("analyze-string",
             2,
             classOf[RegexFunctionSansFlags],
             NodeKindTest.ELEMENT,
             ONE,
             LATE | NEW)
      .arg(0, BuiltInAtomicType.STRING, OPT, null)
      .arg(1, BuiltInAtomicType.STRING, ONE, null)
    register("analyze-string",
             3,
             classOf[AnalyzeStringFn],
             NodeKindTest.ELEMENT,
             ONE,
             LATE | NEW)
      .arg(0, BuiltInAtomicType.STRING, OPT, null)
      .arg(1, BuiltInAtomicType.STRING, ONE, null)
      .arg(2, BuiltInAtomicType.STRING, ONE, null)
    register("apply", 2, classOf[ApplyFn], AnyItemType, STAR, LATE)
      .arg(0, AnyFunctionType.getInstance, OPT, null)
      .arg(1, ArrayItemType.ANY_ARRAY_TYPE, ONE, null)
    register("available-environment-variables",
             0,
             classOf[AvailableEnvironmentVariables],
             BuiltInAtomicType.STRING,
             STAR,
             LATE)
    register("data",
             0,
             classOf[ContextItemAccessorFunction],
             BuiltInAtomicType.ANY_ATOMIC,
             STAR,
             CITEM | LATE)
    register("document-uri",
             0,
             classOf[ContextItemAccessorFunction],
             BuiltInAtomicType.ANY_URI,
             OPT,
             CITEM | LATE)
    register("element-with-id",
             1,
             classOf[SuperId.ElementWithId],
             NodeKindTest.ELEMENT,
             STAR,
             CITEM | LATE | UO).arg(0, BuiltInAtomicType.STRING, STAR, EMPTY)
    register("element-with-id",
             2,
             classOf[SuperId.ElementWithId],
             NodeKindTest.ELEMENT,
             STAR,
             UO)
      .arg(0, BuiltInAtomicType.STRING, STAR, EMPTY)
      .arg(1, Type.NODE_TYPE, ONE, null)
    register("environment-variable",
             1,
             classOf[EnvironmentVariable],
             BuiltInAtomicType.STRING,
             OPT,
             LATE).arg(0, BuiltInAtomicType.STRING, ONE, null)
    val predicate: SpecificFunctionType = new SpecificFunctionType(
      Array(SequenceType.SINGLE_ITEM),
      SequenceType.SINGLE_BOOLEAN)
    register("filter",
             2,
             classOf[FilterFn],
             AnyItemType,
             STAR,
             AS_ARG0 | LATE)
      .arg(0, AnyItemType, STAR | TRA, EMPTY)
      .arg(1, predicate, ONE, null)
    val foldLeftArg: SpecificFunctionType = new SpecificFunctionType(
      Array(SequenceType.ANY_SEQUENCE, SequenceType.SINGLE_ITEM),
      SequenceType.ANY_SEQUENCE)
    register("fold-left",
             3,
             classOf[FoldLeftFn],
             AnyItemType,
             STAR,
             LATE)
      .arg(0, AnyItemType, STAR, null)
      .arg(1, AnyItemType, STAR, null)
      .arg(2, foldLeftArg, ONE, null)
    val foldRightArg: SpecificFunctionType = new SpecificFunctionType(
      Array(SequenceType.SINGLE_ITEM, SequenceType.ANY_SEQUENCE),
      SequenceType.ANY_SEQUENCE)
    register("fold-right",
             3,
             classOf[FoldRightFn],
             AnyItemType,
             STAR,
             LATE)
      .arg(0, AnyItemType, STAR, null)
      .arg(1, AnyItemType, STAR, null)
      .arg(2, foldRightArg, ONE, null)
    val forEachArg: SpecificFunctionType = new SpecificFunctionType(
      Array(SequenceType.SINGLE_ITEM),
      SequenceType.ANY_SEQUENCE)
    register("for-each",
             2,
             classOf[ForEachFn],
             AnyItemType,
             STAR,
             LATE)
      .arg(0, AnyItemType, STAR, EMPTY)
      .arg(1, forEachArg, ONE, null)
    val forEachPairArg: SpecificFunctionType = new SpecificFunctionType(
      Array(SequenceType.SINGLE_ITEM, SequenceType.SINGLE_ITEM),
      SequenceType.ANY_SEQUENCE)
    register("for-each-pair",
             3,
             classOf[ForEachPairFn],
             AnyItemType,
             STAR,
             LATE)
      .arg(0, AnyItemType, STAR, EMPTY)
      .arg(1, AnyItemType, STAR, EMPTY)
      .arg(2, forEachPairArg, ONE, null)
    register("format-date",
             2,
             classOf[FormatDate],
             BuiltInAtomicType.STRING,
             OPT,
             CARD0)
      .arg(0, BuiltInAtomicType.DATE, OPT, null)
      .arg(1, BuiltInAtomicType.STRING, ONE, null)
    register("format-date",
             5,
             classOf[FormatDate],
             BuiltInAtomicType.STRING,
             OPT,
             CARD0)
      .arg(0, BuiltInAtomicType.DATE, OPT, null)
      .arg(1, BuiltInAtomicType.STRING, ONE, null)
      .arg(2, BuiltInAtomicType.STRING, OPT, null)
      .arg(3, BuiltInAtomicType.STRING, OPT, null)
      .arg(4, BuiltInAtomicType.STRING, OPT, null)
    register("format-dateTime",
             2,
             classOf[FormatDate],
             BuiltInAtomicType.STRING,
             OPT,
             CARD0)
      .arg(0, BuiltInAtomicType.DATE_TIME, OPT, null)
      .arg(1, BuiltInAtomicType.STRING, ONE, null)
    register("format-dateTime",
             5,
             classOf[FormatDate],
             BuiltInAtomicType.STRING,
             OPT,
             CARD0)
      .arg(0, BuiltInAtomicType.DATE_TIME, OPT, null)
      .arg(1, BuiltInAtomicType.STRING, ONE, null)
      .arg(2, BuiltInAtomicType.STRING, OPT, null)
      .arg(3, BuiltInAtomicType.STRING, OPT, null)
      .arg(4, BuiltInAtomicType.STRING, OPT, null)
    register("format-integer",
             2,
             classOf[FormatInteger],
             AnyItemType,
             ONE,
             0)
      .arg(0, BuiltInAtomicType.INTEGER, OPT, null)
      .arg(1, BuiltInAtomicType.STRING, ONE, null)
    register("format-integer",
             3,
             classOf[FormatInteger],
             AnyItemType,
             ONE,
             0)
      .arg(0, BuiltInAtomicType.INTEGER, OPT, null)
      .arg(1, BuiltInAtomicType.STRING, ONE, null)
      .arg(2, BuiltInAtomicType.STRING, OPT, null)
    register("format-number",
             2,
             classOf[FormatNumber],
             BuiltInAtomicType.STRING,
             ONE,
             LATE)
      .arg(0, NumericType.getInstance, OPT, null)
      .arg(1, BuiltInAtomicType.STRING, ONE, null)
    register("format-number",
             3,
             classOf[FormatNumber],
             BuiltInAtomicType.STRING,
             ONE,
             NS | LATE)
      .arg(0, NumericType.getInstance, OPT, null)
      .arg(1, BuiltInAtomicType.STRING, ONE, null)
      .arg(2, BuiltInAtomicType.STRING, OPT, null)
    register("format-time",
             2,
             classOf[FormatDate],
             BuiltInAtomicType.STRING,
             OPT,
             CARD0)
      .arg(0, BuiltInAtomicType.TIME, OPT, null)
      .arg(1, BuiltInAtomicType.STRING, ONE, null)
    register("format-time",
             5,
             classOf[FormatDate],
             BuiltInAtomicType.STRING,
             OPT,
             CARD0)
      .arg(0, BuiltInAtomicType.TIME, OPT, null)
      .arg(1, BuiltInAtomicType.STRING, ONE, null)
      .arg(2, BuiltInAtomicType.STRING, OPT, null)
      .arg(3, BuiltInAtomicType.STRING, OPT, null)
      .arg(4, BuiltInAtomicType.STRING, OPT, null)
    register("function-arity",
             1,
             classOf[FunctionArity],
             BuiltInAtomicType.INTEGER,
             ONE,
             0).arg(0, AnyFunctionType.getInstance, ONE, null)
    register("function-lookup",
             2,
             classOf[FunctionLookup],
             AnyFunctionType.getInstance,
             OPT,
             FOCUS | DEPENDS_ON_STATIC_CONTEXT | LATE)
      .arg(0, BuiltInAtomicType.QNAME, ONE, null)
      .arg(1, BuiltInAtomicType.INTEGER, ONE, null)
    register("function-name",
             1,
             classOf[FunctionName],
             BuiltInAtomicType.QNAME,
             OPT,
             0).arg(0, AnyFunctionType.getInstance, ONE, null)
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
    register("nilled",
             0,
             classOf[ContextItemAccessorFunction],
             BuiltInAtomicType.BOOLEAN,
             OPT,
             CITEM | LATE)
    register("node-name",
             0,
             classOf[ContextItemAccessorFunction],
             BuiltInAtomicType.QNAME,
             OPT,
             CITEM | LATE)
    register(
      "outermost",
      1,
      classOf[Outermost],
      AnyNodeTest.getInstance,
      STAR,
      AS_ARG0 | FILTER).arg(0, AnyNodeTest.getInstance, STAR | TRA, null)
    register("parse-xml",
             1,
             classOf[ParseXml],
             NodeKindTest.DOCUMENT,
             OPT,
             LATE).arg(0, BuiltInAtomicType.STRING, OPT, EMPTY)
    register("parse-xml-fragment",
             1,
             classOf[ParseXmlFragment],
             NodeKindTest.DOCUMENT,
             OPT,
             LATE).arg(0, BuiltInAtomicType.STRING, OPT, EMPTY)
    register("path",
             0,
             classOf[ContextItemAccessorFunction],
             BuiltInAtomicType.STRING,
             OPT,
             CITEM | LATE)
    register("path", 1, classOf[Path_1], BuiltInAtomicType.STRING, OPT, 0).arg(
      0,
      AnyNodeTest.getInstance,
      OPT | NAV,
      null)
    register("round",
             2,
             classOf[Round],
             NumericType.getInstance,
             OPT,
             AS_PRIM_ARG0)
      .arg(0, NumericType.getInstance, OPT, EMPTY)
      .arg(1, BuiltInAtomicType.INTEGER, ONE, null)
    register("serialize",
             1,
             classOf[Serialize],
             BuiltInAtomicType.STRING,
             ONE,
             0).arg(0, AnyItemType, STAR, null)
    register("sort", 1, classOf[Sort_1], AnyItemType, STAR, 0).arg(
      0,
      AnyItemType,
      STAR,
      null)
    register(
      "string-join",
      1,
      classOf[StringJoin],
      BuiltInAtomicType.STRING,
      ONE,
      0).arg(0, BuiltInAtomicType.ANY_ATOMIC, STAR, StringValue.EMPTY_STRING)
    register(
      "tail",
      1,
      classOf[TailFn],
      AnyItemType,
      STAR,
      AS_ARG0 | FILTER).arg(0, AnyItemType, STAR | TRA, null)
    register("unparsed-text",
             1,
             classOf[UnparsedText],
             BuiltInAtomicType.STRING,
             OPT,
             BASE | LATE).arg(0, BuiltInAtomicType.STRING, OPT, null)
    register("unparsed-text",
             2,
             classOf[UnparsedText],
             BuiltInAtomicType.STRING,
             OPT,
             BASE | LATE)
      .arg(0, BuiltInAtomicType.STRING, OPT, null)
      .arg(1, BuiltInAtomicType.STRING, ONE, null)
    register(
      "unparsed-text-available",
      1,
      classOf[UnparsedTextAvailable],
      BuiltInAtomicType.BOOLEAN,
      ONE,
      BASE | LATE).arg(0, BuiltInAtomicType.STRING, OPT, BooleanValue.FALSE)
    register("unparsed-text-available",
             2,
             classOf[UnparsedTextAvailable],
             BuiltInAtomicType.BOOLEAN,
             ONE,
             BASE | LATE)
      .arg(0, BuiltInAtomicType.STRING, OPT, BooleanValue.FALSE)
      .arg(1, BuiltInAtomicType.STRING, ONE, null)
    register("unparsed-text-lines",
             1,
             classOf[UnparsedTextLines],
             BuiltInAtomicType.STRING,
             STAR,
             BASE | LATE).arg(0, BuiltInAtomicType.STRING, OPT, null)
    register("unparsed-text-lines",
             2,
             classOf[UnparsedTextLines],
             BuiltInAtomicType.STRING,
             STAR,
             BASE | LATE)
      .arg(0, BuiltInAtomicType.STRING, OPT, null)
      .arg(1, BuiltInAtomicType.STRING, ONE, null)
    register("uri-collection",
             0,
             classOf[UriCollection],
             BuiltInAtomicType.ANY_URI,
             STAR,
             LATE)
    register("uri-collection",
             1,
             classOf[UriCollection],
             BuiltInAtomicType.ANY_URI,
             STAR,
             LATE).arg(0, BuiltInAtomicType.STRING, OPT, null)
  }
//        register("serialize", 2, Serialize.class, BuiltInAtomicType.STRING, ONE, XPATH30, 0)
//                .arg(0, AnyItemType(), STAR, null)
//                .arg(1, NodeKindTest.ELEMENT, OPT, null);
//        register("string-join", 2, StringJoin.class, BuiltInAtomicType.STRING, ONE, CORE, 0)
//                .arg(0, BuiltInAtomicType.ANY_ATOMIC, STAR, StringValue.EMPTY_STRING)
//                .arg(1, BuiltInAtomicType.STRING, ONE, null);
//        register("serialize", 2, Serialize.class, BuiltInAtomicType.STRING, ONE, XPATH30, 0)
//                .arg(0, AnyItemType(), STAR, null)
//                .arg(1, NodeKindTest.ELEMENT, OPT, null);
//        register("string-join", 2, StringJoin.class, BuiltInAtomicType.STRING, ONE, CORE, 0)
//                .arg(0, BuiltInAtomicType.ANY_ATOMIC, STAR, StringValue.EMPTY_STRING)
//                .arg(1, BuiltInAtomicType.STRING, ONE, null);

}
