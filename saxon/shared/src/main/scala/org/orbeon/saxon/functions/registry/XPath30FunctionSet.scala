////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// Copyright (c) 2018-2020 Saxonica Limited
// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is "Incompatible With Secondary Licenses", as defined by the Mozilla Public License, v. 2.0.
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
package org.orbeon.saxon.functions.registry

import org.orbeon.saxon.functions._
import org.orbeon.saxon.functions.hof._
import org.orbeon.saxon.functions.registry.BuiltInFunctionSet._
import org.orbeon.saxon.ma.arrays.ArrayItemType
import org.orbeon.saxon.model._
import org.orbeon.saxon.pattern.{AnyNodeTest, NodeKindTest}
import org.orbeon.saxon.value.{BooleanValue, SequenceType, StringValue}


/**
  * Function signatures (and pointers to implementations) of the functions defined in XPath 3.0 without the
  * Higher-Order-Functions feature
  */
object XPath30FunctionSet {
  val getInstance: XPath30FunctionSet = new XPath30FunctionSet
}

class XPath30FunctionSet private () extends BuiltInFunctionSet {
  locally {
    importFunctionSet(XPath20FunctionSet.getInstance)
    register("analyze-string",
             2,
             () => new RegexFunctionSansFlags,
             NodeKindTest.ELEMENT,
             ONE,
             LATE | NEW)
      .arg(0, BuiltInAtomicType.STRING, OPT, null)
      .arg(1, BuiltInAtomicType.STRING, ONE, null)
    register("analyze-string",
             3,
             () => new AnalyzeStringFn,
             NodeKindTest.ELEMENT,
             ONE,
             LATE | NEW)
      .arg(0, BuiltInAtomicType.STRING, OPT, null)
      .arg(1, BuiltInAtomicType.STRING, ONE, null)
      .arg(2, BuiltInAtomicType.STRING, ONE, null)
    register("apply", 2, () => new ApplyFn, AnyItemType, STAR, LATE)
      .arg(0, AnyFunctionType, OPT, null)
      .arg(1, ArrayItemType.ANY_ARRAY_TYPE, ONE, null)
    register("available-environment-variables",
             0,
             () => new AvailableEnvironmentVariables,
             BuiltInAtomicType.STRING,
             STAR,
             LATE)
    register("data",
             0,
             () => new ContextItemAccessorFunction,
             BuiltInAtomicType.ANY_ATOMIC,
             STAR,
             CITEM | LATE)
    register("document-uri",
             0,
             () => new ContextItemAccessorFunction,
             BuiltInAtomicType.ANY_URI,
             OPT,
             CITEM | LATE)
    register("element-with-id",
             1,
             () => new SuperId.ElementWithId,
             NodeKindTest.ELEMENT,
             STAR,
             CITEM | LATE | UO).arg(0, BuiltInAtomicType.STRING, STAR, EMPTY)
    register("element-with-id",
             2,
             () => new SuperId.ElementWithId,
             NodeKindTest.ELEMENT,
             STAR,
             UO)
      .arg(0, BuiltInAtomicType.STRING, STAR, EMPTY)
      .arg(1, Type.NODE_TYPE, ONE, null)
    register("environment-variable",
             1,
             () => new EnvironmentVariable,
             BuiltInAtomicType.STRING,
             OPT,
             LATE).arg(0, BuiltInAtomicType.STRING, ONE, null)
    val predicate: SpecificFunctionType = new SpecificFunctionType(
      Array(SequenceType.SINGLE_ITEM),
      SequenceType.SINGLE_BOOLEAN)
    register("filter",
             2,
             () => new FilterFn,
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
             () => new FoldLeftFn,
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
             () => new FoldRightFn,
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
             () => new ForEachFn,
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
             () => new ForEachPairFn,
             AnyItemType,
             STAR,
             LATE)
      .arg(0, AnyItemType, STAR, EMPTY)
      .arg(1, AnyItemType, STAR, EMPTY)
      .arg(2, forEachPairArg, ONE, null)
    register("format-date",
             2,
             () => new FormatDate,
             BuiltInAtomicType.STRING,
             OPT,
             CARD0)
      .arg(0, BuiltInAtomicType.DATE, OPT, null)
      .arg(1, BuiltInAtomicType.STRING, ONE, null)
    register("format-date",
             5,
             () => new FormatDate,
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
             () => new FormatDate,
             BuiltInAtomicType.STRING,
             OPT,
             CARD0)
      .arg(0, BuiltInAtomicType.DATE_TIME, OPT, null)
      .arg(1, BuiltInAtomicType.STRING, ONE, null)
    register("format-dateTime",
             5,
             () => new FormatDate,
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
             () => new FormatInteger,
             AnyItemType,
             ONE,
             0)
      .arg(0, BuiltInAtomicType.INTEGER, OPT, null)
      .arg(1, BuiltInAtomicType.STRING, ONE, null)
    register("format-integer",
             3,
             () => new FormatInteger,
             AnyItemType,
             ONE,
             0)
      .arg(0, BuiltInAtomicType.INTEGER, OPT, null)
      .arg(1, BuiltInAtomicType.STRING, ONE, null)
      .arg(2, BuiltInAtomicType.STRING, OPT, null)
    register("format-number",
             2,
             () => new FormatNumber,
             BuiltInAtomicType.STRING,
             ONE,
             LATE)
      .arg(0, NumericType.getInstance, OPT, null)
      .arg(1, BuiltInAtomicType.STRING, ONE, null)
    register("format-number",
             3,
             () => new FormatNumber,
             BuiltInAtomicType.STRING,
             ONE,
             NS | LATE)
      .arg(0, NumericType.getInstance, OPT, null)
      .arg(1, BuiltInAtomicType.STRING, ONE, null)
      .arg(2, BuiltInAtomicType.STRING, OPT, null)
    register("format-time",
             2,
             () => new FormatDate,
             BuiltInAtomicType.STRING,
             OPT,
             CARD0)
      .arg(0, BuiltInAtomicType.TIME, OPT, null)
      .arg(1, BuiltInAtomicType.STRING, ONE, null)
    register("format-time",
             5,
             () => new FormatDate,
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
             () => new FunctionArity,
             BuiltInAtomicType.INTEGER,
             ONE,
             0).arg(0, AnyFunctionType, ONE, null)
    register("function-lookup",
             2,
             () => new FunctionLookup,
             AnyFunctionType,
             OPT,
             FOCUS | DEPENDS_ON_STATIC_CONTEXT | LATE)
      .arg(0, BuiltInAtomicType.QNAME, ONE, null)
      .arg(1, BuiltInAtomicType.INTEGER, ONE, null)
    register("function-name",
             1,
             () => new FunctionName,
             BuiltInAtomicType.QNAME,
             OPT,
             0).arg(0, AnyFunctionType, ONE, null)
    register("generate-id",
             0,
             () => new ContextItemAccessorFunction,
             BuiltInAtomicType.STRING,
             ONE,
             CITEM | LATE)
    register("generate-id",
             1,
             () => new GenerateId_1,
             BuiltInAtomicType.STRING,
             ONE,
             0).arg(0, Type.NODE_TYPE, OPT | INS, StringValue.EMPTY_STRING)
    register("has-children",
             0,
             () => new ContextItemAccessorFunction,
             BuiltInAtomicType.BOOLEAN,
             ONE,
             CITEM | LATE)
    register("has-children",
             1,
             () => new HasChildren_1,
             BuiltInAtomicType.BOOLEAN,
             OPT,
             0).arg(0, AnyNodeTest, OPT | INS, null)
    register("head", 1, () => new HeadFn, AnyItemType, OPT, FILTER)
      .arg(0, AnyItemType, STAR | TRA, null)
    register("innermost",
             1,
             () => new Innermost,
             AnyNodeTest,
             STAR,
             0).arg(0, AnyNodeTest, STAR | NAV, null)
    register("nilled",
             0,
             () => new ContextItemAccessorFunction,
             BuiltInAtomicType.BOOLEAN,
             OPT,
             CITEM | LATE)
    register("node-name",
             0,
             () => new ContextItemAccessorFunction,
             BuiltInAtomicType.QNAME,
             OPT,
             CITEM | LATE)
    register(
      "outermost",
      1,
      () => new Outermost,
      AnyNodeTest,
      STAR,
      AS_ARG0 | FILTER).arg(0, AnyNodeTest, STAR | TRA, null)
    register("parse-xml",
             1,
             () => new ParseXml,
             NodeKindTest.DOCUMENT,
             OPT,
             LATE).arg(0, BuiltInAtomicType.STRING, OPT, EMPTY)
    register("parse-xml-fragment",
             1,
             () => new ParseXmlFragment,
             NodeKindTest.DOCUMENT,
             OPT,
             LATE).arg(0, BuiltInAtomicType.STRING, OPT, EMPTY)
    register("path",
             0,
             () => new ContextItemAccessorFunction,
             BuiltInAtomicType.STRING,
             OPT,
             CITEM | LATE)
    register("path", 1, () => new Path_1, BuiltInAtomicType.STRING, OPT, 0).arg(
      0,
      AnyNodeTest,
      OPT | NAV,
      null)
    register("round",
             2,
             () => new Round,
             NumericType.getInstance,
             OPT,
             AS_PRIM_ARG0)
      .arg(0, NumericType.getInstance, OPT, EMPTY)
      .arg(1, BuiltInAtomicType.INTEGER, ONE, null)
    register("serialize",
             1,
             () => new Serialize,
             BuiltInAtomicType.STRING,
             ONE,
             0).arg(0, AnyItemType, STAR, null)
    register("sort", 1, () => new Sort_1, AnyItemType, STAR, 0).arg(
      0,
      AnyItemType,
      STAR,
      null)
    register(
      "string-join",
      1,
      () => new StringJoin,
      BuiltInAtomicType.STRING,
      ONE,
      0).arg(0, BuiltInAtomicType.ANY_ATOMIC, STAR, StringValue.EMPTY_STRING)
    register(
      "tail",
      1,
      () => new TailFn,
      AnyItemType,
      STAR,
      AS_ARG0 | FILTER).arg(0, AnyItemType, STAR | TRA, null)
    register("unparsed-text",
             1,
             () => new UnparsedText,
             BuiltInAtomicType.STRING,
             OPT,
             BASE | LATE).arg(0, BuiltInAtomicType.STRING, OPT, null)
    register("unparsed-text",
             2,
             () => new UnparsedText,
             BuiltInAtomicType.STRING,
             OPT,
             BASE | LATE)
      .arg(0, BuiltInAtomicType.STRING, OPT, null)
      .arg(1, BuiltInAtomicType.STRING, ONE, null)
    register(
      "unparsed-text-available",
      1,
      () => new UnparsedTextAvailable,
      BuiltInAtomicType.BOOLEAN,
      ONE,
      BASE | LATE).arg(0, BuiltInAtomicType.STRING, OPT, BooleanValue.FALSE)
    register("unparsed-text-available",
             2,
             () => new UnparsedTextAvailable,
             BuiltInAtomicType.BOOLEAN,
             ONE,
             BASE | LATE)
      .arg(0, BuiltInAtomicType.STRING, OPT, BooleanValue.FALSE)
      .arg(1, BuiltInAtomicType.STRING, ONE, null)
    register("unparsed-text-lines",
             1,
             () => new UnparsedTextLines,
             BuiltInAtomicType.STRING,
             STAR,
             BASE | LATE).arg(0, BuiltInAtomicType.STRING, OPT, null)
    register("unparsed-text-lines",
             2,
             () => new UnparsedTextLines,
             BuiltInAtomicType.STRING,
             STAR,
             BASE | LATE)
      .arg(0, BuiltInAtomicType.STRING, OPT, null)
      .arg(1, BuiltInAtomicType.STRING, ONE, null)
    register("uri-collection",
             0,
             () => new UriCollection,
             BuiltInAtomicType.ANY_URI,
             STAR,
             LATE)
    register("uri-collection",
             1,
             () => new UriCollection,
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
