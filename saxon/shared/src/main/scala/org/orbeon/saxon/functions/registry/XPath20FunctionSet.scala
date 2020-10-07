package org.orbeon.saxon.functions.registry

import org.orbeon.saxon.functions.Error

import org.orbeon.saxon.functions._

import org.orbeon.saxon.model.BuiltInAtomicType

import org.orbeon.saxon.model.NumericType

import org.orbeon.saxon.model.Type

import org.orbeon.saxon.pattern.NodeKindTest

import org.orbeon.saxon.value.BooleanValue

import org.orbeon.saxon.value.DoubleValue

import org.orbeon.saxon.value.Int64Value

import org.orbeon.saxon.value.StringValue

import BuiltInFunctionSet._

object XPath20FunctionSet {

  private var THE_INSTANCE: XPath20FunctionSet = new XPath20FunctionSet()

  def getInstance: XPath20FunctionSet = THE_INSTANCE

}

class XPath20FunctionSet private() extends BuiltInFunctionSet {

  init()

  private def init(): Unit = {
    register("abs",
      1,
      classOf[Abs],
      NumericType.getInstance,
      OPT,
      AS_PRIM_ARG0).arg(0, NumericType.getInstance, OPT, EMPTY)
    register("adjust-date-to-timezone",
      1,
      classOf[Adjust_1],
      BuiltInAtomicType.DATE,
      OPT,
      LATE | CARD0).arg(0, BuiltInAtomicType.DATE, OPT, EMPTY)
    register("adjust-date-to-timezone",
      2,
      classOf[Adjust_2],
      BuiltInAtomicType.DATE,
      OPT,
      CARD0)
      .arg(0, BuiltInAtomicType.DATE, OPT, EMPTY)
      .arg(1, BuiltInAtomicType.DAY_TIME_DURATION, OPT, null)
    register("adjust-dateTime-to-timezone",
      1,
      classOf[Adjust_1],
      BuiltInAtomicType.DATE_TIME,
      OPT,
      LATE | CARD0).arg(0, BuiltInAtomicType.DATE_TIME, OPT, EMPTY)
    register("adjust-dateTime-to-timezone",
      2,
      classOf[Adjust_2],
      BuiltInAtomicType.DATE_TIME,
      OPT,
      CARD0)
      .arg(0, BuiltInAtomicType.DATE_TIME, OPT, EMPTY)
      .arg(1, BuiltInAtomicType.DAY_TIME_DURATION, OPT, null)
    register("adjust-time-to-timezone",
      1,
      classOf[Adjust_1],
      BuiltInAtomicType.TIME,
      OPT,
      LATE | CARD0).arg(0, BuiltInAtomicType.TIME, OPT, EMPTY)
    register("adjust-time-to-timezone",
      2,
      classOf[Adjust_2],
      BuiltInAtomicType.TIME,
      OPT,
      CARD0)
      .arg(0, BuiltInAtomicType.TIME, OPT, EMPTY)
      .arg(1, BuiltInAtomicType.DAY_TIME_DURATION, OPT, null)
    register("avg", 1, classOf[Average], BuiltInAtomicType.ANY_ATOMIC, OPT, UO)
      .arg(0, BuiltInAtomicType.ANY_ATOMIC, STAR, EMPTY)
    register("base-uri",
      0,
      classOf[ContextItemAccessorFunction],
      BuiltInAtomicType.ANY_URI,
      OPT,
      CITEM | BASE | LATE)
    register("base-uri",
      1,
      classOf[BaseUri_1],
      BuiltInAtomicType.ANY_URI,
      OPT,
      BASE).arg(0, Type.NODE_TYPE, OPT | INS, EMPTY)
    register("boolean",
      1,
      classOf[BooleanFn],
      BuiltInAtomicType.BOOLEAN,
      ONE,
      0).arg(0, Type.ITEM_TYPE, STAR | INS, null)
    register("ceiling",
      1,
      classOf[Ceiling],
      NumericType.getInstance,
      OPT,
      AS_PRIM_ARG0).arg(0, NumericType.getInstance, OPT, EMPTY)
    register("codepoint-equal",
      2,
      classOf[CodepointEqual],
      BuiltInAtomicType.BOOLEAN,
      OPT,
      0)
      .arg(0, BuiltInAtomicType.STRING, OPT, EMPTY)
      .arg(1, BuiltInAtomicType.STRING, OPT, EMPTY)
    register("codepoints-to-string",
      1,
      classOf[CodepointsToString],
      BuiltInAtomicType.STRING,
      ONE,
      0).arg(0, BuiltInAtomicType.INTEGER, STAR, null)
    register("collection",
      0,
      classOf[CollectionFn],
      Type.ITEM_TYPE,
      STAR,
      BASE | LATE)
    register("collection",
      1,
      classOf[CollectionFn],
      Type.ITEM_TYPE,
      STAR,
      BASE | LATE).arg(0, BuiltInAtomicType.STRING, OPT, null)
    register("compare",
      2,
      classOf[Compare],
      BuiltInAtomicType.INTEGER,
      OPT,
      DCOLL)
      .arg(0, BuiltInAtomicType.STRING, OPT, EMPTY)
      .arg(1, BuiltInAtomicType.STRING, OPT, EMPTY)
    register("compare",
      3,
      classOf[CollatingFunctionFree],
      BuiltInAtomicType.INTEGER,
      OPT,
      BASE)
      .arg(0, BuiltInAtomicType.STRING, OPT, EMPTY)
      .arg(1, BuiltInAtomicType.STRING, OPT, EMPTY)
      .arg(2, BuiltInAtomicType.STRING, ONE, null)
    register("concat", -1, classOf[Concat], BuiltInAtomicType.STRING, ONE, 0)
      .arg(0, BuiltInAtomicType.ANY_ATOMIC, OPT, null)
    register("contains",
      2,
      classOf[Contains],
      BuiltInAtomicType.BOOLEAN,
      ONE,
      DCOLL)
      .arg(0, BuiltInAtomicType.STRING, OPT, null)
      .arg(1, BuiltInAtomicType.STRING, OPT, BooleanValue.TRUE)
    register("contains",
      3,
      classOf[CollatingFunctionFree],
      BuiltInAtomicType.BOOLEAN,
      ONE,
      BASE)
      .arg(0, BuiltInAtomicType.STRING, OPT, null)
      .arg(1, BuiltInAtomicType.STRING, OPT, BooleanValue.TRUE)
      .arg(2, BuiltInAtomicType.STRING, ONE, null)
    register("count", 1, classOf[Count], BuiltInAtomicType.INTEGER, ONE, UO)
      .arg(0, Type.ITEM_TYPE, STAR | INS, Int64Value.ZERO)
    register("current-date",
      0,
      classOf[DynamicContextAccessor.CurrentDate],
      BuiltInAtomicType.DATE,
      ONE,
      LATE)
    register("current-dateTime",
      0,
      classOf[DynamicContextAccessor.CurrentDateTime],
      BuiltInAtomicType.DATE_TIME,
      ONE,
      LATE)
    register("current-time",
      0,
      classOf[DynamicContextAccessor.CurrentTime],
      BuiltInAtomicType.TIME,
      ONE,
      LATE)
    register("data", 1, classOf[Data_1], BuiltInAtomicType.ANY_ATOMIC, STAR, 0)
      .arg(0, Type.ITEM_TYPE, STAR | ABS, EMPTY)
    register("dateTime",
      2,
      classOf[DateTimeConstructor],
      BuiltInAtomicType.DATE_TIME,
      OPT,
      0)
      .arg(0, BuiltInAtomicType.DATE, OPT, EMPTY)
      .arg(1, BuiltInAtomicType.TIME, OPT, EMPTY)
    register("day-from-date",
      1,
      classOf[AccessorFn.DayFromDate],
      BuiltInAtomicType.INTEGER,
      OPT,
      CARD0).arg(0, BuiltInAtomicType.DATE, OPT, EMPTY)
    register("day-from-dateTime",
      1,
      classOf[AccessorFn.DayFromDateTime],
      BuiltInAtomicType.INTEGER,
      OPT,
      CARD0).arg(0, BuiltInAtomicType.DATE_TIME, OPT, EMPTY)
    register("days-from-duration",
      1,
      classOf[AccessorFn.DaysFromDuration],
      BuiltInAtomicType.INTEGER,
      OPT,
      CARD0).arg(0, BuiltInAtomicType.DURATION, OPT, EMPTY)
    register("deep-equal",
      2,
      classOf[DeepEqual],
      BuiltInAtomicType.BOOLEAN,
      ONE,
      DCOLL)
      .arg(0, Type.ITEM_TYPE, STAR | ABS, null)
      .arg(1, Type.ITEM_TYPE, STAR | ABS, null)
    register("deep-equal",
      3,
      classOf[CollatingFunctionFree],
      BuiltInAtomicType.BOOLEAN,
      ONE,
      BASE)
      .arg(0, Type.ITEM_TYPE, STAR, null)
      .arg(1, Type.ITEM_TYPE, STAR, null)
      .arg(2, BuiltInAtomicType.STRING, ONE, null)
    register("default-collation",
      0,
      classOf[StaticContextAccessor.DefaultCollation],
      BuiltInAtomicType.STRING,
      ONE,
      DCOLL)
    register("distinct-values",
      1,
      classOf[DistinctValues],
      BuiltInAtomicType.ANY_ATOMIC,
      STAR,
      DCOLL | UO).arg(0, BuiltInAtomicType.ANY_ATOMIC, STAR, EMPTY)
    register("distinct-values",
      2,
      classOf[CollatingFunctionFree],
      BuiltInAtomicType.ANY_ATOMIC,
      STAR,
      BASE | UO)
      .arg(0, BuiltInAtomicType.ANY_ATOMIC, STAR, EMPTY)
      .arg(1, BuiltInAtomicType.STRING, ONE, null)
    register("doc", 1, classOf[Doc], NodeKindTest.DOCUMENT, OPT, BASE | LATE)
      .arg(0, BuiltInAtomicType.STRING, OPT, EMPTY)
    register(
      "doc-available",
      1,
      classOf[DocAvailable],
      BuiltInAtomicType.BOOLEAN,
      ONE,
      BASE | LATE).arg(0, BuiltInAtomicType.STRING, OPT, BooleanValue.FALSE)
    register("document-uri",
      1,
      classOf[DocumentUri_1],
      BuiltInAtomicType.ANY_URI,
      OPT,
      LATE).arg(0, Type.NODE_TYPE, OPT | INS, EMPTY)
    register("element-with-id",
      1,
      classOf[SuperId.ElementWithId],
      NodeKindTest.ELEMENT,
      STAR,
      CDOC | LATE | UO).arg(0, BuiltInAtomicType.STRING, STAR, EMPTY)
    register("element-with-id",
      2,
      classOf[SuperId.ElementWithId],
      NodeKindTest.ELEMENT,
      STAR,
      UO)
      .arg(0, BuiltInAtomicType.STRING, STAR, EMPTY)
      .arg(1, Type.NODE_TYPE, ONE, null)
    register("empty", 1, classOf[Empty], BuiltInAtomicType.BOOLEAN, ONE, UO)
      .arg(0, Type.ITEM_TYPE, STAR | INS, BooleanValue.TRUE)
    register("encode-for-uri",
      1,
      classOf[EncodeForUri],
      BuiltInAtomicType.STRING,
      ONE,
      0).arg(0, BuiltInAtomicType.STRING, OPT, StringValue.EMPTY_STRING)
    register("ends-with",
      2,
      classOf[EndsWith],
      BuiltInAtomicType.BOOLEAN,
      ONE,
      DCOLL)
      .arg(0, BuiltInAtomicType.STRING, OPT, null)
      .arg(1, BuiltInAtomicType.STRING, OPT, BooleanValue.TRUE)
    register("ends-with",
      3,
      classOf[CollatingFunctionFree],
      BuiltInAtomicType.BOOLEAN,
      ONE,
      BASE)
      .arg(0, BuiltInAtomicType.STRING, OPT, null)
      .arg(1, BuiltInAtomicType.STRING, OPT, BooleanValue.TRUE)
      .arg(2, BuiltInAtomicType.STRING, ONE, null)
    register("escape-html-uri",
      1,
      classOf[EscapeHtmlUri],
      BuiltInAtomicType.STRING,
      ONE,
      0).arg(0, BuiltInAtomicType.STRING, OPT, StringValue.EMPTY_STRING)
    register("error", 0, classOf[Error], Type.ITEM_TYPE, OPT, LATE)
    register("error", 1, classOf[Error], Type.ITEM_TYPE, OPT, LATE).arg(
      0,
      BuiltInAtomicType.QNAME,
      OPT,
      null)
    register("error", 2, classOf[Error], Type.ITEM_TYPE, OPT, LATE)
      .arg(0, BuiltInAtomicType.QNAME, OPT, null)
      .arg(1, BuiltInAtomicType.STRING, ONE, null)
    register("error", 3, classOf[Error], Type.ITEM_TYPE, OPT, LATE)
      .arg(0, BuiltInAtomicType.QNAME, OPT, null)
      .arg(1, BuiltInAtomicType.STRING, ONE, null)
      .arg(2, Type.ITEM_TYPE, STAR, null)
    register("exactly-one",
      1,
      classOf[TreatFn.ExactlyOne],
      Type.ITEM_TYPE,
      ONE,
      AS_ARG0 | FILTER).arg(0, Type.ITEM_TYPE, STAR | TRA, null)
    register("exists", 1, classOf[Exists], BuiltInAtomicType.BOOLEAN, ONE, UO)
      .arg(0, Type.ITEM_TYPE, STAR | INS, BooleanValue.FALSE)
    register("false",
      0,
      classOf[ConstantFunction.False],
      BuiltInAtomicType.BOOLEAN,
      ONE,
      0)
    register("floor",
      1,
      classOf[Floor],
      NumericType.getInstance,
      OPT,
      AS_PRIM_ARG0).arg(0, NumericType.getInstance, OPT, EMPTY)
    register("hours-from-dateTime",
      1,
      classOf[AccessorFn.HoursFromDateTime],
      BuiltInAtomicType.INTEGER,
      OPT,
      CARD0).arg(0, BuiltInAtomicType.DATE_TIME, OPT, EMPTY)
    register("hours-from-duration",
      1,
      classOf[AccessorFn.HoursFromDuration],
      BuiltInAtomicType.INTEGER,
      OPT,
      CARD0).arg(0, BuiltInAtomicType.DURATION, OPT, EMPTY)
    register("hours-from-time",
      1,
      classOf[AccessorFn.HoursFromTime],
      BuiltInAtomicType.INTEGER,
      OPT,
      CARD0).arg(0, BuiltInAtomicType.TIME, OPT, EMPTY)
    register("id",
      1,
      classOf[SuperId.Id],
      NodeKindTest.ELEMENT,
      STAR,
      CDOC | LATE | UO).arg(0, BuiltInAtomicType.STRING, STAR, EMPTY)
    register("id",
      2,
      classOf[SuperId.Id],
      NodeKindTest.ELEMENT,
      STAR,
      LATE | UO)
      .arg(0, BuiltInAtomicType.STRING, STAR, EMPTY)
      .arg(1, Type.NODE_TYPE, ONE | NAV, null)
    register("idref", 1, classOf[Idref], Type.NODE_TYPE, STAR, CDOC | LATE)
      .arg(0, BuiltInAtomicType.STRING, STAR, EMPTY)
    register("idref", 2, classOf[Idref], Type.NODE_TYPE, STAR, LATE)
      .arg(0, BuiltInAtomicType.STRING, STAR, EMPTY)
      .arg(1, Type.NODE_TYPE, ONE | NAV, null)
    register("implicit-timezone",
      0,
      classOf[DynamicContextAccessor.ImplicitTimezone],
      BuiltInAtomicType.DAY_TIME_DURATION,
      ONE,
      LATE)
    register("in-scope-prefixes",
      1,
      classOf[InScopePrefixes],
      BuiltInAtomicType.STRING,
      STAR,
      0).arg(0, NodeKindTest.ELEMENT, ONE | INS, null)
    register("index-of",
      2,
      classOf[IndexOf],
      BuiltInAtomicType.INTEGER,
      STAR,
      DCOLL)
      .arg(0, BuiltInAtomicType.ANY_ATOMIC, STAR, EMPTY)
      .arg(1, BuiltInAtomicType.ANY_ATOMIC, ONE, null)
    register("index-of",
      3,
      classOf[CollatingFunctionFree],
      BuiltInAtomicType.INTEGER,
      STAR,
      BASE)
      .arg(0, BuiltInAtomicType.ANY_ATOMIC, STAR, EMPTY)
      .arg(1, BuiltInAtomicType.ANY_ATOMIC, ONE, null)
      .arg(2, BuiltInAtomicType.STRING, ONE, null)
    register("insert-before",
      3,
      classOf[InsertBefore],
      Type.ITEM_TYPE,
      STAR,
      0)
      .arg(0, Type.ITEM_TYPE, STAR | TRA, null)
      .arg(1, BuiltInAtomicType.INTEGER, ONE, null)
      .arg(2, Type.ITEM_TYPE, STAR | TRA, null)
    register("iri-to-uri",
      1,
      classOf[IriToUri],
      BuiltInAtomicType.STRING,
      ONE,
      0).arg(0, BuiltInAtomicType.STRING, OPT, StringValue.EMPTY_STRING)
    register("lang",
      1,
      classOf[Lang],
      BuiltInAtomicType.BOOLEAN,
      ONE,
      CITEM | LATE).arg(0, BuiltInAtomicType.STRING, OPT, null)
    register("lang", 2, classOf[Lang], BuiltInAtomicType.BOOLEAN, ONE, 0)
      .arg(0, BuiltInAtomicType.STRING, OPT, null)
      .arg(1, Type.NODE_TYPE, ONE | INS, null)
    register("last",
      0,
      classOf[PositionAndLast.Last],
      BuiltInAtomicType.INTEGER,
      ONE,
      LAST | LATE)
    register("local-name",
      0,
      classOf[ContextItemAccessorFunction],
      BuiltInAtomicType.STRING,
      ONE,
      CITEM | LATE)
    register("local-name",
      1,
      classOf[LocalName_1],
      BuiltInAtomicType.STRING,
      ONE,
      0).arg(0, Type.NODE_TYPE, OPT | INS, StringValue.EMPTY_STRING)
    register("local-name-from-QName",
      1,
      classOf[AccessorFn.LocalNameFromQName],
      BuiltInAtomicType.NCNAME,
      OPT,
      0).arg(0, BuiltInAtomicType.QNAME, OPT, EMPTY)
    register("lower-case",
      1,
      classOf[LowerCase],
      BuiltInAtomicType.STRING,
      ONE,
      0).arg(0, BuiltInAtomicType.STRING, OPT, StringValue.EMPTY_STRING)
    register("matches",
      2,
      classOf[RegexFunctionSansFlags],
      BuiltInAtomicType.BOOLEAN,
      ONE,
      0)
      .arg(0, BuiltInAtomicType.STRING, OPT, null)
      .arg(1, BuiltInAtomicType.STRING, ONE, null)
    register("matches", 3, classOf[Matches], BuiltInAtomicType.BOOLEAN, ONE, 0)
      .arg(0, BuiltInAtomicType.STRING, OPT, null)
      .arg(1, BuiltInAtomicType.STRING, ONE, null)
      .arg(2, BuiltInAtomicType.STRING, ONE, null)
    register(
      "max",
      1,
      classOf[Minimax.Max],
      BuiltInAtomicType.ANY_ATOMIC,
      OPT,
      DCOLL | UO | CARD0).arg(0, BuiltInAtomicType.ANY_ATOMIC, STAR, EMPTY)
    register("max",
      2,
      classOf[CollatingFunctionFree],
      BuiltInAtomicType.ANY_ATOMIC,
      OPT,
      BASE | UO | CARD0)
      .arg(0, BuiltInAtomicType.ANY_ATOMIC, STAR, EMPTY)
      .arg(1, BuiltInAtomicType.STRING, ONE, null)
    register(
      "min",
      1,
      classOf[Minimax.Min],
      BuiltInAtomicType.ANY_ATOMIC,
      OPT,
      DCOLL | UO | CARD0).arg(0, BuiltInAtomicType.ANY_ATOMIC, STAR, EMPTY)
    register("min",
      2,
      classOf[CollatingFunctionFree],
      BuiltInAtomicType.ANY_ATOMIC,
      OPT,
      BASE | UO | CARD0)
      .arg(0, BuiltInAtomicType.ANY_ATOMIC, STAR, EMPTY)
      .arg(1, BuiltInAtomicType.STRING, ONE, null)
    register("minutes-from-dateTime",
      1,
      classOf[AccessorFn.MinutesFromDateTime],
      BuiltInAtomicType.INTEGER,
      OPT,
      CARD0).arg(0, BuiltInAtomicType.DATE_TIME, OPT, EMPTY)
    register("minutes-from-duration",
      1,
      classOf[AccessorFn.MinutesFromDuration],
      BuiltInAtomicType.INTEGER,
      OPT,
      CARD0).arg(0, BuiltInAtomicType.DURATION, OPT, EMPTY)
    register("minutes-from-time",
      1,
      classOf[AccessorFn.MinutesFromTime],
      BuiltInAtomicType.INTEGER,
      OPT,
      CARD0).arg(0, BuiltInAtomicType.TIME, OPT, EMPTY)
    register("month-from-date",
      1,
      classOf[AccessorFn.MonthFromDate],
      BuiltInAtomicType.INTEGER,
      OPT,
      CARD0).arg(0, BuiltInAtomicType.DATE, OPT, EMPTY)
    register("month-from-dateTime",
      1,
      classOf[AccessorFn.MonthFromDateTime],
      BuiltInAtomicType.INTEGER,
      OPT,
      CARD0).arg(0, BuiltInAtomicType.DATE_TIME, OPT, EMPTY)
    register("months-from-duration",
      1,
      classOf[AccessorFn.MonthsFromDuration],
      BuiltInAtomicType.INTEGER,
      OPT,
      CARD0).arg(0, BuiltInAtomicType.DURATION, OPT, EMPTY)
    register("name",
      0,
      classOf[ContextItemAccessorFunction],
      BuiltInAtomicType.STRING,
      ONE,
      CITEM | LATE)
    register("name", 1, classOf[Name_1], BuiltInAtomicType.STRING, ONE, 0).arg(
      0,
      Type.NODE_TYPE,
      OPT | INS,
      StringValue.EMPTY_STRING)
    register("namespace-uri",
      0,
      classOf[ContextItemAccessorFunction],
      BuiltInAtomicType.ANY_URI,
      ONE,
      CITEM | LATE)
    register("namespace-uri",
      1,
      classOf[NamespaceUri_1],
      BuiltInAtomicType.ANY_URI,
      ONE,
      0).arg(0, Type.NODE_TYPE, OPT | INS, StringValue.EMPTY_STRING)
    register("namespace-uri-for-prefix",
      2,
      classOf[NamespaceForPrefix],
      BuiltInAtomicType.ANY_URI,
      OPT,
      0)
      .arg(0, BuiltInAtomicType.STRING, OPT, null)
      .arg(1, NodeKindTest.ELEMENT, ONE | INS, null)
    register("namespace-uri-from-QName",
      1,
      classOf[AccessorFn.NamespaceUriFromQName],
      BuiltInAtomicType.ANY_URI,
      OPT,
      CARD0).arg(0, BuiltInAtomicType.QNAME, OPT, EMPTY)
    register("nilled", 1, classOf[Nilled_1], BuiltInAtomicType.BOOLEAN, OPT, 0)
      .arg(0, Type.NODE_TYPE, OPT | INS, EMPTY)
    register("node-name",
      1,
      classOf[NodeName_1],
      BuiltInAtomicType.QNAME,
      OPT,
      0).arg(0, Type.NODE_TYPE, OPT | INS, EMPTY)
    register("not", 1, classOf[NotFn], BuiltInAtomicType.BOOLEAN, ONE, 0).arg(
      0,
      Type.ITEM_TYPE,
      STAR | INS,
      BooleanValue.TRUE)
    register("normalize-space",
      0,
      classOf[ContextItemAccessorFunction.StringAccessor],
      BuiltInAtomicType.STRING,
      ONE,
      CITEM | LATE)
    register("normalize-space",
      1,
      classOf[NormalizeSpace_1],
      BuiltInAtomicType.STRING,
      ONE,
      0).arg(0, BuiltInAtomicType.STRING, OPT, null)
    register("normalize-unicode",
      1,
      classOf[NormalizeUnicode],
      BuiltInAtomicType.STRING,
      ONE,
      0).arg(0, BuiltInAtomicType.STRING, OPT, StringValue.EMPTY_STRING)
    register("normalize-unicode",
      2,
      classOf[NormalizeUnicode],
      BuiltInAtomicType.STRING,
      ONE,
      0)
      .arg(0, BuiltInAtomicType.STRING, OPT, StringValue.EMPTY_STRING)
      .arg(1, BuiltInAtomicType.STRING, ONE, null)
    register("number",
      0,
      classOf[ContextItemAccessorFunction.Number_0],
      BuiltInAtomicType.DOUBLE,
      ONE,
      CITEM | LATE)
    register("number", 1, classOf[Number_1], BuiltInAtomicType.DOUBLE, ONE, 0)
      .arg(0, BuiltInAtomicType.ANY_ATOMIC, OPT, DoubleValue.NaN)
    register("one-or-more",
      1,
      classOf[TreatFn.OneOrMore],
      Type.ITEM_TYPE,
      PLUS,
      AS_ARG0 | FILTER).arg(0, Type.ITEM_TYPE, STAR | TRA, null)
    register("position",
      0,
      classOf[PositionAndLast.Position],
      BuiltInAtomicType.INTEGER,
      ONE,
      POSN | LATE)
    register("prefix-from-QName",
      1,
      classOf[AccessorFn.PrefixFromQName],
      BuiltInAtomicType.NCNAME,
      OPT,
      0).arg(0, BuiltInAtomicType.QNAME, OPT, EMPTY)
    register("QName", 2, classOf[QNameFn], BuiltInAtomicType.QNAME, ONE, 0)
      .arg(0, BuiltInAtomicType.STRING, OPT, null)
      .arg(1, BuiltInAtomicType.STRING, ONE, null)
    register("remove",
      2,
      classOf[Remove],
      Type.ITEM_TYPE,
      STAR,
      AS_ARG0 | FILTER)
      .arg(0, Type.ITEM_TYPE, STAR | TRA, EMPTY)
      .arg(1, BuiltInAtomicType.INTEGER, ONE, null)
    register("replace",
      3,
      classOf[RegexFunctionSansFlags],
      BuiltInAtomicType.STRING,
      ONE,
      0)
      .arg(0, BuiltInAtomicType.STRING, OPT, StringValue.EMPTY_STRING)
      .arg(1, BuiltInAtomicType.STRING, ONE, null)
      .arg(2, BuiltInAtomicType.STRING, ONE, null)
    register("replace", 4, classOf[Replace], BuiltInAtomicType.STRING, ONE, 0)
      .arg(0, BuiltInAtomicType.STRING, OPT, StringValue.EMPTY_STRING)
      .arg(1, BuiltInAtomicType.STRING, ONE, null)
      .arg(2, BuiltInAtomicType.STRING, ONE, null)
      .arg(3, BuiltInAtomicType.STRING, ONE, null)
    register("resolve-QName",
      2,
      classOf[ResolveQName],
      BuiltInAtomicType.QNAME,
      OPT,
      CARD0)
      .arg(0, BuiltInAtomicType.STRING, OPT, EMPTY)
      .arg(1, NodeKindTest.ELEMENT, ONE | INS, null)
    register("resolve-uri",
      1,
      classOf[ResolveURI],
      BuiltInAtomicType.ANY_URI,
      OPT,
      CARD0 | BASE).arg(0, BuiltInAtomicType.STRING, OPT, null)
    register("resolve-uri",
      2,
      classOf[ResolveURI],
      BuiltInAtomicType.ANY_URI,
      OPT,
      CARD0)
      .arg(0, BuiltInAtomicType.STRING, OPT, null)
      .arg(1, BuiltInAtomicType.STRING, ONE, null)
    register("reverse",
      1,
      classOf[Reverse],
      Type.ITEM_TYPE,
      STAR,
      AS_ARG0 | FILTER).arg(0, Type.ITEM_TYPE, STAR | NAV, EMPTY)
    register("root",
      0,
      classOf[ContextItemAccessorFunction],
      Type.NODE_TYPE,
      ONE,
      CITEM | LATE)
    register("root", 1, classOf[Root_1], Type.NODE_TYPE, OPT, CARD0).arg(
      0,
      Type.NODE_TYPE,
      OPT | NAV,
      EMPTY)
    register("round",
      1,
      classOf[Round],
      NumericType.getInstance,
      OPT,
      AS_PRIM_ARG0).arg(0, NumericType.getInstance, OPT, EMPTY)
    register("round-half-to-even",
      1,
      classOf[RoundHalfToEven],
      NumericType.getInstance,
      OPT,
      AS_PRIM_ARG0).arg(0, NumericType.getInstance, OPT, EMPTY)
    register("round-half-to-even",
      2,
      classOf[RoundHalfToEven],
      NumericType.getInstance,
      OPT,
      AS_PRIM_ARG0)
      .arg(0, NumericType.getInstance, OPT, EMPTY)
      .arg(1, BuiltInAtomicType.INTEGER, ONE, null)
    register("seconds-from-dateTime",
      1,
      classOf[AccessorFn.SecondsFromDateTime],
      BuiltInAtomicType.DECIMAL,
      OPT,
      CARD0).arg(0, BuiltInAtomicType.DATE_TIME, OPT, EMPTY)
    register("seconds-from-duration",
      1,
      classOf[AccessorFn.SecondsFromDuration],
      BuiltInAtomicType.DECIMAL,
      OPT,
      CARD0).arg(0, BuiltInAtomicType.DURATION, OPT, EMPTY)
    register("seconds-from-time",
      1,
      classOf[AccessorFn.SecondsFromTime],
      BuiltInAtomicType.DECIMAL,
      OPT,
      CARD0).arg(0, BuiltInAtomicType.TIME, OPT, EMPTY)
    register("starts-with",
      2,
      classOf[StartsWith],
      BuiltInAtomicType.BOOLEAN,
      ONE,
      DCOLL)
      .arg(0, BuiltInAtomicType.STRING, OPT, null)
      .arg(1, BuiltInAtomicType.STRING, OPT, BooleanValue.TRUE)
    register("starts-with",
      3,
      classOf[CollatingFunctionFree],
      BuiltInAtomicType.BOOLEAN,
      ONE,
      BASE)
      .arg(0, BuiltInAtomicType.STRING, OPT, null)
      .arg(1, BuiltInAtomicType.STRING, OPT, BooleanValue.TRUE)
      .arg(2, BuiltInAtomicType.STRING, ONE, null)
    register("static-base-uri",
      0,
      classOf[StaticBaseUri],
      BuiltInAtomicType.ANY_URI,
      OPT,
      BASE | LATE)
    register("string",
      0,
      classOf[ContextItemAccessorFunction],
      BuiltInAtomicType.STRING,
      ONE,
      CITEM | LATE)
    register("string", 1, classOf[String_1], BuiltInAtomicType.STRING, ONE, 0)
      .arg(0, Type.ITEM_TYPE, OPT | ABS, StringValue.EMPTY_STRING)
    register("string-length",
      0,
      classOf[ContextItemAccessorFunction.StringAccessor],
      BuiltInAtomicType.INTEGER,
      ONE,
      CITEM | LATE)
    register("string-length",
      1,
      classOf[StringLength_1],
      BuiltInAtomicType.INTEGER,
      ONE,
      0).arg(0, BuiltInAtomicType.STRING, OPT, null)
    register("string-join",
      2,
      classOf[StringJoin],
      BuiltInAtomicType.STRING,
      ONE,
      0)
      .arg(0, BuiltInAtomicType.ANY_ATOMIC, STAR, StringValue.EMPTY_STRING)
      .arg(1, BuiltInAtomicType.STRING, ONE, null)
    register("string-to-codepoints",
      1,
      classOf[StringToCodepoints],
      BuiltInAtomicType.INTEGER,
      STAR,
      0).arg(0, BuiltInAtomicType.STRING, OPT, EMPTY)
    register("subsequence",
      2,
      classOf[Subsequence_2],
      Type.ITEM_TYPE,
      STAR,
      AS_ARG0 | FILTER)
      .arg(0, Type.ITEM_TYPE, STAR | TRA, EMPTY)
      .arg(1, NumericType.getInstance, ONE, null)
    register("subsequence",
      3,
      classOf[Subsequence_3],
      Type.ITEM_TYPE,
      STAR,
      AS_ARG0 | FILTER)
      .arg(0, Type.ITEM_TYPE, STAR | TRA, EMPTY)
      .arg(1, NumericType.getInstance, ONE, null)
      .arg(2, NumericType.getInstance, ONE, null)
    register("substring",
      2,
      classOf[Substring],
      BuiltInAtomicType.STRING,
      ONE,
      0)
      .arg(0, BuiltInAtomicType.STRING, OPT, StringValue.EMPTY_STRING)
      .arg(1, NumericType.getInstance, ONE, null)
    register("substring",
      3,
      classOf[Substring],
      BuiltInAtomicType.STRING,
      ONE,
      0)
      .arg(0, BuiltInAtomicType.STRING, OPT, StringValue.EMPTY_STRING)
      .arg(1, NumericType.getInstance, ONE, null)
      .arg(2, NumericType.getInstance, ONE, null)
    register("substring-after",
      2,
      classOf[SubstringAfter],
      BuiltInAtomicType.STRING,
      ONE,
      DCOLL)
      .arg(0, BuiltInAtomicType.STRING, OPT, null)
      .arg(1, BuiltInAtomicType.STRING, OPT, null)
    register("substring-after",
      3,
      classOf[CollatingFunctionFree],
      BuiltInAtomicType.STRING,
      ONE,
      BASE)
      .arg(0, BuiltInAtomicType.STRING, OPT, null)
      .arg(1, BuiltInAtomicType.STRING, OPT, null)
      .arg(2, BuiltInAtomicType.STRING, ONE, null)
    register("substring-before",
      2,
      classOf[SubstringBefore],
      BuiltInAtomicType.STRING,
      ONE,
      DCOLL)
      .arg(0, BuiltInAtomicType.STRING, OPT, null)
      .arg(1, BuiltInAtomicType.STRING, OPT, StringValue.EMPTY_STRING)
    register("substring-before",
      3,
      classOf[CollatingFunctionFree],
      BuiltInAtomicType.STRING,
      ONE,
      BASE)
      .arg(0, BuiltInAtomicType.STRING, OPT, null)
      .arg(1, BuiltInAtomicType.STRING, OPT, StringValue.EMPTY_STRING)
      .arg(2, BuiltInAtomicType.STRING, ONE, null)
    register("sum", 1, classOf[Sum], BuiltInAtomicType.ANY_ATOMIC, ONE, UO)
      .arg(0, BuiltInAtomicType.ANY_ATOMIC, STAR, null)
    register("sum", 2, classOf[Sum], BuiltInAtomicType.ANY_ATOMIC, OPT, UO)
      .arg(0, BuiltInAtomicType.ANY_ATOMIC, STAR, null)
      .arg(1, BuiltInAtomicType.ANY_ATOMIC, OPT, null)
    register("timezone-from-date",
      1,
      classOf[AccessorFn.TimezoneFromDate],
      BuiltInAtomicType.DAY_TIME_DURATION,
      OPT,
      0).arg(0, BuiltInAtomicType.DATE, OPT, EMPTY)
    register("timezone-from-dateTime",
      1,
      classOf[AccessorFn.TimezoneFromDateTime],
      BuiltInAtomicType.DAY_TIME_DURATION,
      OPT,
      0).arg(0, BuiltInAtomicType.DATE_TIME, OPT, EMPTY)
    register("timezone-from-time",
      1,
      classOf[AccessorFn.TimezoneFromTime],
      BuiltInAtomicType.DAY_TIME_DURATION,
      OPT,
      0).arg(0, BuiltInAtomicType.TIME, OPT, EMPTY)
    register("tokenize",
      2,
      classOf[RegexFunctionSansFlags],
      BuiltInAtomicType.STRING,
      STAR,
      0)
      .arg(0, BuiltInAtomicType.STRING, OPT, EMPTY)
      .arg(1, BuiltInAtomicType.STRING, ONE, null)
    register("tokenize",
      3,
      classOf[Tokenize_3],
      BuiltInAtomicType.STRING,
      STAR,
      0)
      .arg(0, BuiltInAtomicType.STRING, OPT, EMPTY)
      .arg(1, BuiltInAtomicType.STRING, ONE, null)
      .arg(2, BuiltInAtomicType.STRING, ONE, null)
    register("trace", 2, classOf[Trace], Type.ITEM_TYPE, STAR, AS_ARG0 | LATE)
      .arg(0, Type.ITEM_TYPE, STAR | TRA, null)
      .arg(1, BuiltInAtomicType.STRING, ONE, null)
    register("translate",
      3,
      classOf[Translate],
      BuiltInAtomicType.STRING,
      ONE,
      0)
      .arg(0, BuiltInAtomicType.STRING, OPT, StringValue.EMPTY_STRING)
      .arg(1, BuiltInAtomicType.STRING, ONE, null)
      .arg(2, BuiltInAtomicType.STRING, ONE, null)
    register("true",
      0,
      classOf[ConstantFunction.True],
      BuiltInAtomicType.BOOLEAN,
      ONE,
      0)
    register("unordered",
      1,
      classOf[Unordered],
      Type.ITEM_TYPE,
      STAR,
      AS_ARG0 | FILTER | UO).arg(0, Type.ITEM_TYPE, STAR | TRA, EMPTY)
    register("upper-case",
      1,
      classOf[UpperCase],
      BuiltInAtomicType.STRING,
      ONE,
      0).arg(0, BuiltInAtomicType.STRING, OPT, StringValue.EMPTY_STRING)
    register("year-from-date",
      1,
      classOf[AccessorFn.YearFromDate],
      BuiltInAtomicType.INTEGER,
      OPT,
      CARD0).arg(0, BuiltInAtomicType.DATE, OPT, EMPTY)
    register("year-from-dateTime",
      1,
      classOf[AccessorFn.YearFromDateTime],
      BuiltInAtomicType.INTEGER,
      OPT,
      CARD0).arg(0, BuiltInAtomicType.DATE_TIME, OPT, EMPTY)
    register("years-from-duration",
      1,
      classOf[AccessorFn.YearsFromDuration],
      BuiltInAtomicType.INTEGER,
      OPT,
      CARD0).arg(0, BuiltInAtomicType.DURATION, OPT, EMPTY)
    register("zero-or-one",
      1,
      classOf[TreatFn.ZeroOrOne],
      Type.ITEM_TYPE,
      OPT,
      AS_ARG0 | FILTER).arg(0, Type.ITEM_TYPE, STAR | TRA, null)
  }

}
