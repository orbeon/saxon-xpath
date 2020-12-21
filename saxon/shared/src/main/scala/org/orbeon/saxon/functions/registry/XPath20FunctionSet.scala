package org.orbeon.saxon.functions.registry

import org.orbeon.saxon.functions.{Error, _}
import org.orbeon.saxon.functions.registry.BuiltInFunctionSet._
import org.orbeon.saxon.model.{BuiltInAtomicType, NumericType, Type}
import org.orbeon.saxon.pattern.NodeKindTest
import org.orbeon.saxon.value.{BooleanValue, DoubleValue, Int64Value, StringValue}

object XPath20FunctionSet {
  val getInstance: XPath20FunctionSet = new XPath20FunctionSet()
}

class XPath20FunctionSet private() extends BuiltInFunctionSet {

  init()

  private def init(): Unit = {
    register("abs",
      1,
      () => new Abs,
      NumericType.getInstance,
      OPT,
      AS_PRIM_ARG0).arg(0, NumericType.getInstance, OPT, EMPTY)
    register("adjust-date-to-timezone",
      1,
      () => new Adjust_1,
      BuiltInAtomicType.DATE,
      OPT,
      LATE | CARD0).arg(0, BuiltInAtomicType.DATE, OPT, EMPTY)
    register("adjust-date-to-timezone",
      2,
      () => new Adjust_2,
      BuiltInAtomicType.DATE,
      OPT,
      CARD0)
      .arg(0, BuiltInAtomicType.DATE, OPT, EMPTY)
      .arg(1, BuiltInAtomicType.DAY_TIME_DURATION, OPT, null)
    register("adjust-dateTime-to-timezone",
      1,
      () => new Adjust_1,
      BuiltInAtomicType.DATE_TIME,
      OPT,
      LATE | CARD0).arg(0, BuiltInAtomicType.DATE_TIME, OPT, EMPTY)
    register("adjust-dateTime-to-timezone",
      2,
      () => new Adjust_2,
      BuiltInAtomicType.DATE_TIME,
      OPT,
      CARD0)
      .arg(0, BuiltInAtomicType.DATE_TIME, OPT, EMPTY)
      .arg(1, BuiltInAtomicType.DAY_TIME_DURATION, OPT, null)
    register("adjust-time-to-timezone",
      1,
      () => new Adjust_1,
      BuiltInAtomicType.TIME,
      OPT,
      LATE | CARD0).arg(0, BuiltInAtomicType.TIME, OPT, EMPTY)
    register("adjust-time-to-timezone",
      2,
      () => new Adjust_2,
      BuiltInAtomicType.TIME,
      OPT,
      CARD0)
      .arg(0, BuiltInAtomicType.TIME, OPT, EMPTY)
      .arg(1, BuiltInAtomicType.DAY_TIME_DURATION, OPT, null)
    register("avg", 1, () => new Average, BuiltInAtomicType.ANY_ATOMIC, OPT, UO)
      .arg(0, BuiltInAtomicType.ANY_ATOMIC, STAR, EMPTY)
    register("base-uri",
      0,
      () => new ContextItemAccessorFunction,
      BuiltInAtomicType.ANY_URI,
      OPT,
      CITEM | BASE | LATE)
    register("base-uri",
      1,
      () => new BaseUri_1,
      BuiltInAtomicType.ANY_URI,
      OPT,
      BASE).arg(0, Type.NODE_TYPE, OPT | INS, EMPTY)
    register("boolean",
      1,
      () => new BooleanFn,
      BuiltInAtomicType.BOOLEAN,
      ONE,
      0).arg(0, Type.ITEM_TYPE, STAR | INS, null)
    register("ceiling",
      1,
      () => new Ceiling,
      NumericType.getInstance,
      OPT,
      AS_PRIM_ARG0).arg(0, NumericType.getInstance, OPT, EMPTY)
    register("codepoint-equal",
      2,
      () => new CodepointEqual,
      BuiltInAtomicType.BOOLEAN,
      OPT,
      0)
      .arg(0, BuiltInAtomicType.STRING, OPT, EMPTY)
      .arg(1, BuiltInAtomicType.STRING, OPT, EMPTY)
    register("codepoints-to-string",
      1,
      () => new CodepointsToString,
      BuiltInAtomicType.STRING,
      ONE,
      0).arg(0, BuiltInAtomicType.INTEGER, STAR, null)
    register("collection",
      0,
      () => new CollectionFn,
      Type.ITEM_TYPE,
      STAR,
      BASE | LATE)
    register("collection",
      1,
      () => new CollectionFn,
      Type.ITEM_TYPE,
      STAR,
      BASE | LATE).arg(0, BuiltInAtomicType.STRING, OPT, null)
    register("compare",
      2,
      () => new Compare,
      BuiltInAtomicType.INTEGER,
      OPT,
      DCOLL)
      .arg(0, BuiltInAtomicType.STRING, OPT, EMPTY)
      .arg(1, BuiltInAtomicType.STRING, OPT, EMPTY)
    register("compare",
      3,
      () => new CollatingFunctionFree,
      BuiltInAtomicType.INTEGER,
      OPT,
      BASE)
      .arg(0, BuiltInAtomicType.STRING, OPT, EMPTY)
      .arg(1, BuiltInAtomicType.STRING, OPT, EMPTY)
      .arg(2, BuiltInAtomicType.STRING, ONE, null)
    register("concat", -1, () => new Concat, BuiltInAtomicType.STRING, ONE, 0)
      .arg(0, BuiltInAtomicType.ANY_ATOMIC, OPT, null)
    register("contains",
      2,
      () => new Contains,
      BuiltInAtomicType.BOOLEAN,
      ONE,
      DCOLL)
      .arg(0, BuiltInAtomicType.STRING, OPT, null)
      .arg(1, BuiltInAtomicType.STRING, OPT, BooleanValue.TRUE)
    register("contains",
      3,
      () => new CollatingFunctionFree,
      BuiltInAtomicType.BOOLEAN,
      ONE,
      BASE)
      .arg(0, BuiltInAtomicType.STRING, OPT, null)
      .arg(1, BuiltInAtomicType.STRING, OPT, BooleanValue.TRUE)
      .arg(2, BuiltInAtomicType.STRING, ONE, null)
    register("count", 1, () => new Count, BuiltInAtomicType.INTEGER, ONE, UO)
      .arg(0, Type.ITEM_TYPE, STAR | INS, Int64Value.ZERO)
    register("current-date",
      0,
      () => new DynamicContextAccessor.CurrentDate,
      BuiltInAtomicType.DATE,
      ONE,
      LATE)
    register("current-dateTime",
      0,
      () => new DynamicContextAccessor.CurrentDateTime,
      BuiltInAtomicType.DATE_TIME,
      ONE,
      LATE)
    register("current-time",
      0,
      () => new DynamicContextAccessor.CurrentTime,
      BuiltInAtomicType.TIME,
      ONE,
      LATE)
    register("data", 1, () => new Data_1, BuiltInAtomicType.ANY_ATOMIC, STAR, 0)
      .arg(0, Type.ITEM_TYPE, STAR | ABS, EMPTY)
    register("dateTime",
      2,
      () => new DateTimeConstructor,
      BuiltInAtomicType.DATE_TIME,
      OPT,
      0)
      .arg(0, BuiltInAtomicType.DATE, OPT, EMPTY)
      .arg(1, BuiltInAtomicType.TIME, OPT, EMPTY)
    register("day-from-date",
      1,
      () => new AccessorFn.DayFromDate,
      BuiltInAtomicType.INTEGER,
      OPT,
      CARD0).arg(0, BuiltInAtomicType.DATE, OPT, EMPTY)
    register("day-from-dateTime",
      1,
      () => new AccessorFn.DayFromDateTime,
      BuiltInAtomicType.INTEGER,
      OPT,
      CARD0).arg(0, BuiltInAtomicType.DATE_TIME, OPT, EMPTY)
    register("days-from-duration",
      1,
      () => new AccessorFn.DaysFromDuration,
      BuiltInAtomicType.INTEGER,
      OPT,
      CARD0).arg(0, BuiltInAtomicType.DURATION, OPT, EMPTY)
    register("deep-equal",
      2,
      () => new DeepEqual,
      BuiltInAtomicType.BOOLEAN,
      ONE,
      DCOLL)
      .arg(0, Type.ITEM_TYPE, STAR | ABS, null)
      .arg(1, Type.ITEM_TYPE, STAR | ABS, null)
    register("deep-equal",
      3,
      () => new CollatingFunctionFree,
      BuiltInAtomicType.BOOLEAN,
      ONE,
      BASE)
      .arg(0, Type.ITEM_TYPE, STAR, null)
      .arg(1, Type.ITEM_TYPE, STAR, null)
      .arg(2, BuiltInAtomicType.STRING, ONE, null)
    register("default-collation",
      0,
      () => new StaticContextAccessor.DefaultCollation,
      BuiltInAtomicType.STRING,
      ONE,
      DCOLL)
    register("distinct-values",
      1,
      () => new DistinctValues,
      BuiltInAtomicType.ANY_ATOMIC,
      STAR,
      DCOLL | UO).arg(0, BuiltInAtomicType.ANY_ATOMIC, STAR, EMPTY)
    register("distinct-values",
      2,
      () => new CollatingFunctionFree,
      BuiltInAtomicType.ANY_ATOMIC,
      STAR,
      BASE | UO)
      .arg(0, BuiltInAtomicType.ANY_ATOMIC, STAR, EMPTY)
      .arg(1, BuiltInAtomicType.STRING, ONE, null)
    register("doc", 1, () => new Doc, NodeKindTest.DOCUMENT, OPT, BASE | LATE)
      .arg(0, BuiltInAtomicType.STRING, OPT, EMPTY)
    register(
      "doc-available",
      1,
      () => new DocAvailable,
      BuiltInAtomicType.BOOLEAN,
      ONE,
      BASE | LATE).arg(0, BuiltInAtomicType.STRING, OPT, BooleanValue.FALSE)
    register("document-uri",
      1,
      () => new DocumentUri_1,
      BuiltInAtomicType.ANY_URI,
      OPT,
      LATE).arg(0, Type.NODE_TYPE, OPT | INS, EMPTY)
    register("element-with-id",
      1,
      () => new SuperId.ElementWithId,
      NodeKindTest.ELEMENT,
      STAR,
      CDOC | LATE | UO).arg(0, BuiltInAtomicType.STRING, STAR, EMPTY)
    register("element-with-id",
      2,
      () => new SuperId.ElementWithId,
      NodeKindTest.ELEMENT,
      STAR,
      UO)
      .arg(0, BuiltInAtomicType.STRING, STAR, EMPTY)
      .arg(1, Type.NODE_TYPE, ONE, null)
    register("empty", 1, () => new Empty, BuiltInAtomicType.BOOLEAN, ONE, UO)
      .arg(0, Type.ITEM_TYPE, STAR | INS, BooleanValue.TRUE)
    register("encode-for-uri",
      1,
      () => new EncodeForUri,
      BuiltInAtomicType.STRING,
      ONE,
      0).arg(0, BuiltInAtomicType.STRING, OPT, StringValue.EMPTY_STRING)
    register("ends-with",
      2,
      () => new EndsWith,
      BuiltInAtomicType.BOOLEAN,
      ONE,
      DCOLL)
      .arg(0, BuiltInAtomicType.STRING, OPT, null)
      .arg(1, BuiltInAtomicType.STRING, OPT, BooleanValue.TRUE)
    register("ends-with",
      3,
      () => new CollatingFunctionFree,
      BuiltInAtomicType.BOOLEAN,
      ONE,
      BASE)
      .arg(0, BuiltInAtomicType.STRING, OPT, null)
      .arg(1, BuiltInAtomicType.STRING, OPT, BooleanValue.TRUE)
      .arg(2, BuiltInAtomicType.STRING, ONE, null)
    register("escape-html-uri",
      1,
      () => new EscapeHtmlUri,
      BuiltInAtomicType.STRING,
      ONE,
      0).arg(0, BuiltInAtomicType.STRING, OPT, StringValue.EMPTY_STRING)
    register("error", 0, () => new Error, Type.ITEM_TYPE, OPT, LATE)
    register("error", 1, () => new Error, Type.ITEM_TYPE, OPT, LATE).arg(
      0,
      BuiltInAtomicType.QNAME,
      OPT,
      null)
    register("error", 2, () => new Error, Type.ITEM_TYPE, OPT, LATE)
      .arg(0, BuiltInAtomicType.QNAME, OPT, null)
      .arg(1, BuiltInAtomicType.STRING, ONE, null)
    register("error", 3, () => new Error, Type.ITEM_TYPE, OPT, LATE)
      .arg(0, BuiltInAtomicType.QNAME, OPT, null)
      .arg(1, BuiltInAtomicType.STRING, ONE, null)
      .arg(2, Type.ITEM_TYPE, STAR, null)
    register("exactly-one",
      1,
      () => new TreatFn.ExactlyOne,
      Type.ITEM_TYPE,
      ONE,
      AS_ARG0 | FILTER).arg(0, Type.ITEM_TYPE, STAR | TRA, null)
    register("exists", 1, () => new Exists, BuiltInAtomicType.BOOLEAN, ONE, UO)
      .arg(0, Type.ITEM_TYPE, STAR | INS, BooleanValue.FALSE)
    register("false",
      0,
      () => new ConstantFunction.False,
      BuiltInAtomicType.BOOLEAN,
      ONE,
      0)
    register("floor",
      1,
      () => new Floor,
      NumericType.getInstance,
      OPT,
      AS_PRIM_ARG0).arg(0, NumericType.getInstance, OPT, EMPTY)
    register("hours-from-dateTime",
      1,
      () => new AccessorFn.HoursFromDateTime,
      BuiltInAtomicType.INTEGER,
      OPT,
      CARD0).arg(0, BuiltInAtomicType.DATE_TIME, OPT, EMPTY)
    register("hours-from-duration",
      1,
      () => new AccessorFn.HoursFromDuration,
      BuiltInAtomicType.INTEGER,
      OPT,
      CARD0).arg(0, BuiltInAtomicType.DURATION, OPT, EMPTY)
    register("hours-from-time",
      1,
      () => new AccessorFn.HoursFromTime,
      BuiltInAtomicType.INTEGER,
      OPT,
      CARD0).arg(0, BuiltInAtomicType.TIME, OPT, EMPTY)
    register("id",
      1,
      () => new SuperId.Id,
      NodeKindTest.ELEMENT,
      STAR,
      CDOC | LATE | UO).arg(0, BuiltInAtomicType.STRING, STAR, EMPTY)
    register("id",
      2,
      () => new SuperId.Id,
      NodeKindTest.ELEMENT,
      STAR,
      LATE | UO)
      .arg(0, BuiltInAtomicType.STRING, STAR, EMPTY)
      .arg(1, Type.NODE_TYPE, ONE | NAV, null)
    register("idref", 1, () => new Idref, Type.NODE_TYPE, STAR, CDOC | LATE)
      .arg(0, BuiltInAtomicType.STRING, STAR, EMPTY)
    register("idref", 2, () => new Idref, Type.NODE_TYPE, STAR, LATE)
      .arg(0, BuiltInAtomicType.STRING, STAR, EMPTY)
      .arg(1, Type.NODE_TYPE, ONE | NAV, null)
    register("implicit-timezone",
      0,
      () => new DynamicContextAccessor.ImplicitTimezone,
      BuiltInAtomicType.DAY_TIME_DURATION,
      ONE,
      LATE)
    register("in-scope-prefixes",
      1,
      () => new InScopePrefixes,
      BuiltInAtomicType.STRING,
      STAR,
      0).arg(0, NodeKindTest.ELEMENT, ONE | INS, null)
    register("index-of",
      2,
      () => new IndexOf,
      BuiltInAtomicType.INTEGER,
      STAR,
      DCOLL)
      .arg(0, BuiltInAtomicType.ANY_ATOMIC, STAR, EMPTY)
      .arg(1, BuiltInAtomicType.ANY_ATOMIC, ONE, null)
    register("index-of",
      3,
      () => new CollatingFunctionFree,
      BuiltInAtomicType.INTEGER,
      STAR,
      BASE)
      .arg(0, BuiltInAtomicType.ANY_ATOMIC, STAR, EMPTY)
      .arg(1, BuiltInAtomicType.ANY_ATOMIC, ONE, null)
      .arg(2, BuiltInAtomicType.STRING, ONE, null)
    register("insert-before",
      3,
      () => new InsertBefore,
      Type.ITEM_TYPE,
      STAR,
      0)
      .arg(0, Type.ITEM_TYPE, STAR | TRA, null)
      .arg(1, BuiltInAtomicType.INTEGER, ONE, null)
      .arg(2, Type.ITEM_TYPE, STAR | TRA, null)
    register("iri-to-uri",
      1,
      () => new IriToUri,
      BuiltInAtomicType.STRING,
      ONE,
      0).arg(0, BuiltInAtomicType.STRING, OPT, StringValue.EMPTY_STRING)
    register("lang",
      1,
      () => new Lang,
      BuiltInAtomicType.BOOLEAN,
      ONE,
      CITEM | LATE).arg(0, BuiltInAtomicType.STRING, OPT, null)
    register("lang", 2, () => new Lang, BuiltInAtomicType.BOOLEAN, ONE, 0)
      .arg(0, BuiltInAtomicType.STRING, OPT, null)
      .arg(1, Type.NODE_TYPE, ONE | INS, null)
    register("last",
      0,
      () => new PositionAndLast.Last,
      BuiltInAtomicType.INTEGER,
      ONE,
      LAST | LATE)
    register("local-name",
      0,
      () => new ContextItemAccessorFunction,
      BuiltInAtomicType.STRING,
      ONE,
      CITEM | LATE)
    register("local-name",
      1,
      () => new LocalName_1,
      BuiltInAtomicType.STRING,
      ONE,
      0).arg(0, Type.NODE_TYPE, OPT | INS, StringValue.EMPTY_STRING)
    register("local-name-from-QName",
      1,
      () => new AccessorFn.LocalNameFromQName,
      BuiltInAtomicType.NCNAME,
      OPT,
      0).arg(0, BuiltInAtomicType.QNAME, OPT, EMPTY)
    register("lower-case",
      1,
      () => new LowerCase,
      BuiltInAtomicType.STRING,
      ONE,
      0).arg(0, BuiltInAtomicType.STRING, OPT, StringValue.EMPTY_STRING)
    register("matches",
      2,
      () => new RegexFunctionSansFlags,
      BuiltInAtomicType.BOOLEAN,
      ONE,
      0)
      .arg(0, BuiltInAtomicType.STRING, OPT, null)
      .arg(1, BuiltInAtomicType.STRING, ONE, null)
    register("matches", 3, () => new Matches, BuiltInAtomicType.BOOLEAN, ONE, 0)
      .arg(0, BuiltInAtomicType.STRING, OPT, null)
      .arg(1, BuiltInAtomicType.STRING, ONE, null)
      .arg(2, BuiltInAtomicType.STRING, ONE, null)
    register(
      "max",
      1,
      () => new Minimax.Max,
      BuiltInAtomicType.ANY_ATOMIC,
      OPT,
      DCOLL | UO | CARD0).arg(0, BuiltInAtomicType.ANY_ATOMIC, STAR, EMPTY)
    register("max",
      2,
      () => new CollatingFunctionFree,
      BuiltInAtomicType.ANY_ATOMIC,
      OPT,
      BASE | UO | CARD0)
      .arg(0, BuiltInAtomicType.ANY_ATOMIC, STAR, EMPTY)
      .arg(1, BuiltInAtomicType.STRING, ONE, null)
    register(
      "min",
      1,
      () => new Minimax.Min,
      BuiltInAtomicType.ANY_ATOMIC,
      OPT,
      DCOLL | UO | CARD0).arg(0, BuiltInAtomicType.ANY_ATOMIC, STAR, EMPTY)
    register("min",
      2,
      () => new CollatingFunctionFree,
      BuiltInAtomicType.ANY_ATOMIC,
      OPT,
      BASE | UO | CARD0)
      .arg(0, BuiltInAtomicType.ANY_ATOMIC, STAR, EMPTY)
      .arg(1, BuiltInAtomicType.STRING, ONE, null)
    register("minutes-from-dateTime",
      1,
      () => new AccessorFn.MinutesFromDateTime,
      BuiltInAtomicType.INTEGER,
      OPT,
      CARD0).arg(0, BuiltInAtomicType.DATE_TIME, OPT, EMPTY)
    register("minutes-from-duration",
      1,
      () => new AccessorFn.MinutesFromDuration,
      BuiltInAtomicType.INTEGER,
      OPT,
      CARD0).arg(0, BuiltInAtomicType.DURATION, OPT, EMPTY)
    register("minutes-from-time",
      1,
      () => new AccessorFn.MinutesFromTime,
      BuiltInAtomicType.INTEGER,
      OPT,
      CARD0).arg(0, BuiltInAtomicType.TIME, OPT, EMPTY)
    register("month-from-date",
      1,
      () => new AccessorFn.MonthFromDate,
      BuiltInAtomicType.INTEGER,
      OPT,
      CARD0).arg(0, BuiltInAtomicType.DATE, OPT, EMPTY)
    register("month-from-dateTime",
      1,
      () => new AccessorFn.MonthFromDateTime,
      BuiltInAtomicType.INTEGER,
      OPT,
      CARD0).arg(0, BuiltInAtomicType.DATE_TIME, OPT, EMPTY)
    register("months-from-duration",
      1,
      () => new AccessorFn.MonthsFromDuration,
      BuiltInAtomicType.INTEGER,
      OPT,
      CARD0).arg(0, BuiltInAtomicType.DURATION, OPT, EMPTY)
    register("name",
      0,
      () => new ContextItemAccessorFunction,
      BuiltInAtomicType.STRING,
      ONE,
      CITEM | LATE)
    register("name", 1, () => new Name_1, BuiltInAtomicType.STRING, ONE, 0).arg(
      0,
      Type.NODE_TYPE,
      OPT | INS,
      StringValue.EMPTY_STRING)
    register("namespace-uri",
      0,
      () => new ContextItemAccessorFunction,
      BuiltInAtomicType.ANY_URI,
      ONE,
      CITEM | LATE)
    register("namespace-uri",
      1,
      () => new NamespaceUri_1,
      BuiltInAtomicType.ANY_URI,
      ONE,
      0).arg(0, Type.NODE_TYPE, OPT | INS, StringValue.EMPTY_STRING)
    register("namespace-uri-for-prefix",
      2,
      () => new NamespaceForPrefix,
      BuiltInAtomicType.ANY_URI,
      OPT,
      0)
      .arg(0, BuiltInAtomicType.STRING, OPT, null)
      .arg(1, NodeKindTest.ELEMENT, ONE | INS, null)
    register("namespace-uri-from-QName",
      1,
      () => new AccessorFn.NamespaceUriFromQName,
      BuiltInAtomicType.ANY_URI,
      OPT,
      CARD0).arg(0, BuiltInAtomicType.QNAME, OPT, EMPTY)
    register("nilled", 1, () => new Nilled_1, BuiltInAtomicType.BOOLEAN, OPT, 0)
      .arg(0, Type.NODE_TYPE, OPT | INS, EMPTY)
    register("node-name",
      1,
      () => new NodeName_1,
      BuiltInAtomicType.QNAME,
      OPT,
      0).arg(0, Type.NODE_TYPE, OPT | INS, EMPTY)
    register("not", 1, () => new NotFn, BuiltInAtomicType.BOOLEAN, ONE, 0).arg(
      0,
      Type.ITEM_TYPE,
      STAR | INS,
      BooleanValue.TRUE)
    register("normalize-space",
      0,
      () => new ContextItemAccessorFunction.StringAccessor,
      BuiltInAtomicType.STRING,
      ONE,
      CITEM | LATE)
    register("normalize-space",
      1,
      () => new NormalizeSpace_1,
      BuiltInAtomicType.STRING,
      ONE,
      0).arg(0, BuiltInAtomicType.STRING, OPT, null)
    register("normalize-unicode",
      1,
      () => new NormalizeUnicode,
      BuiltInAtomicType.STRING,
      ONE,
      0).arg(0, BuiltInAtomicType.STRING, OPT, StringValue.EMPTY_STRING)
    register("normalize-unicode",
      2,
      () => new NormalizeUnicode,
      BuiltInAtomicType.STRING,
      ONE,
      0)
      .arg(0, BuiltInAtomicType.STRING, OPT, StringValue.EMPTY_STRING)
      .arg(1, BuiltInAtomicType.STRING, ONE, null)
    register("number",
      0,
      () => new ContextItemAccessorFunction.Number_0,
      BuiltInAtomicType.DOUBLE,
      ONE,
      CITEM | LATE)
    register("number", 1, () => new Number_1, BuiltInAtomicType.DOUBLE, ONE, 0)
      .arg(0, BuiltInAtomicType.ANY_ATOMIC, OPT, DoubleValue.NaN)
    register("one-or-more",
      1,
      () => new TreatFn.OneOrMore,
      Type.ITEM_TYPE,
      PLUS,
      AS_ARG0 | FILTER).arg(0, Type.ITEM_TYPE, STAR | TRA, null)
    register("position",
      0,
      () => new PositionAndLast.Position,
      BuiltInAtomicType.INTEGER,
      ONE,
      POSN | LATE)
    register("prefix-from-QName",
      1,
      () => new AccessorFn.PrefixFromQName,
      BuiltInAtomicType.NCNAME,
      OPT,
      0).arg(0, BuiltInAtomicType.QNAME, OPT, EMPTY)
    register("QName", 2, () => new QNameFn, BuiltInAtomicType.QNAME, ONE, 0)
      .arg(0, BuiltInAtomicType.STRING, OPT, null)
      .arg(1, BuiltInAtomicType.STRING, ONE, null)
    register("remove",
      2,
      () => new Remove,
      Type.ITEM_TYPE,
      STAR,
      AS_ARG0 | FILTER)
      .arg(0, Type.ITEM_TYPE, STAR | TRA, EMPTY)
      .arg(1, BuiltInAtomicType.INTEGER, ONE, null)
    register("replace",
      3,
      () => new RegexFunctionSansFlags,
      BuiltInAtomicType.STRING,
      ONE,
      0)
      .arg(0, BuiltInAtomicType.STRING, OPT, StringValue.EMPTY_STRING)
      .arg(1, BuiltInAtomicType.STRING, ONE, null)
      .arg(2, BuiltInAtomicType.STRING, ONE, null)
    register("replace", 4, () => new Replace, BuiltInAtomicType.STRING, ONE, 0)
      .arg(0, BuiltInAtomicType.STRING, OPT, StringValue.EMPTY_STRING)
      .arg(1, BuiltInAtomicType.STRING, ONE, null)
      .arg(2, BuiltInAtomicType.STRING, ONE, null)
      .arg(3, BuiltInAtomicType.STRING, ONE, null)
    register("resolve-QName",
      2,
      () => new ResolveQName,
      BuiltInAtomicType.QNAME,
      OPT,
      CARD0)
      .arg(0, BuiltInAtomicType.STRING, OPT, EMPTY)
      .arg(1, NodeKindTest.ELEMENT, ONE | INS, null)
    register("resolve-uri",
      1,
      () => new ResolveURI,
      BuiltInAtomicType.ANY_URI,
      OPT,
      CARD0 | BASE).arg(0, BuiltInAtomicType.STRING, OPT, null)
    register("resolve-uri",
      2,
      () => new ResolveURI,
      BuiltInAtomicType.ANY_URI,
      OPT,
      CARD0)
      .arg(0, BuiltInAtomicType.STRING, OPT, null)
      .arg(1, BuiltInAtomicType.STRING, ONE, null)
    register("reverse",
      1,
      () => new Reverse,
      Type.ITEM_TYPE,
      STAR,
      AS_ARG0 | FILTER).arg(0, Type.ITEM_TYPE, STAR | NAV, EMPTY)
    register("root",
      0,
      () => new ContextItemAccessorFunction,
      Type.NODE_TYPE,
      ONE,
      CITEM | LATE)
    register("root", 1, () => new Root_1, Type.NODE_TYPE, OPT, CARD0).arg(
      0,
      Type.NODE_TYPE,
      OPT | NAV,
      EMPTY)
    register("round",
      1,
      () => new Round,
      NumericType.getInstance,
      OPT,
      AS_PRIM_ARG0).arg(0, NumericType.getInstance, OPT, EMPTY)
    register("round-half-to-even",
      1,
      () => new RoundHalfToEven,
      NumericType.getInstance,
      OPT,
      AS_PRIM_ARG0).arg(0, NumericType.getInstance, OPT, EMPTY)
    register("round-half-to-even",
      2,
      () => new RoundHalfToEven,
      NumericType.getInstance,
      OPT,
      AS_PRIM_ARG0)
      .arg(0, NumericType.getInstance, OPT, EMPTY)
      .arg(1, BuiltInAtomicType.INTEGER, ONE, null)
    register("seconds-from-dateTime",
      1,
      () => new AccessorFn.SecondsFromDateTime,
      BuiltInAtomicType.DECIMAL,
      OPT,
      CARD0).arg(0, BuiltInAtomicType.DATE_TIME, OPT, EMPTY)
    register("seconds-from-duration",
      1,
      () => new AccessorFn.SecondsFromDuration,
      BuiltInAtomicType.DECIMAL,
      OPT,
      CARD0).arg(0, BuiltInAtomicType.DURATION, OPT, EMPTY)
    register("seconds-from-time",
      1,
      () => new AccessorFn.SecondsFromTime,
      BuiltInAtomicType.DECIMAL,
      OPT,
      CARD0).arg(0, BuiltInAtomicType.TIME, OPT, EMPTY)
    register("starts-with",
      2,
      () => new StartsWith,
      BuiltInAtomicType.BOOLEAN,
      ONE,
      DCOLL)
      .arg(0, BuiltInAtomicType.STRING, OPT, null)
      .arg(1, BuiltInAtomicType.STRING, OPT, BooleanValue.TRUE)
    register("starts-with",
      3,
      () => new CollatingFunctionFree,
      BuiltInAtomicType.BOOLEAN,
      ONE,
      BASE)
      .arg(0, BuiltInAtomicType.STRING, OPT, null)
      .arg(1, BuiltInAtomicType.STRING, OPT, BooleanValue.TRUE)
      .arg(2, BuiltInAtomicType.STRING, ONE, null)
    register("static-base-uri",
      0,
      () => new StaticBaseUri,
      BuiltInAtomicType.ANY_URI,
      OPT,
      BASE | LATE)
    register("string",
      0,
      () => new ContextItemAccessorFunction,
      BuiltInAtomicType.STRING,
      ONE,
      CITEM | LATE)
    register("string", 1, () => new String_1, BuiltInAtomicType.STRING, ONE, 0)
      .arg(0, Type.ITEM_TYPE, OPT | ABS, StringValue.EMPTY_STRING)
    register("string-length",
      0,
      () => new ContextItemAccessorFunction.StringAccessor,
      BuiltInAtomicType.INTEGER,
      ONE,
      CITEM | LATE)
    register("string-length",
      1,
      () => new StringLength_1,
      BuiltInAtomicType.INTEGER,
      ONE,
      0).arg(0, BuiltInAtomicType.STRING, OPT, null)
    register("string-join",
      2,
      () => new StringJoin,
      BuiltInAtomicType.STRING,
      ONE,
      0)
      .arg(0, BuiltInAtomicType.ANY_ATOMIC, STAR, StringValue.EMPTY_STRING)
      .arg(1, BuiltInAtomicType.STRING, ONE, null)
    register("string-to-codepoints",
      1,
      () => new StringToCodepoints,
      BuiltInAtomicType.INTEGER,
      STAR,
      0).arg(0, BuiltInAtomicType.STRING, OPT, EMPTY)
    register("subsequence",
      2,
      () => new Subsequence_2,
      Type.ITEM_TYPE,
      STAR,
      AS_ARG0 | FILTER)
      .arg(0, Type.ITEM_TYPE, STAR | TRA, EMPTY)
      .arg(1, NumericType.getInstance, ONE, null)
    register("subsequence",
      3,
      () => new Subsequence_3,
      Type.ITEM_TYPE,
      STAR,
      AS_ARG0 | FILTER)
      .arg(0, Type.ITEM_TYPE, STAR | TRA, EMPTY)
      .arg(1, NumericType.getInstance, ONE, null)
      .arg(2, NumericType.getInstance, ONE, null)
    register("substring",
      2,
      () => new Substring,
      BuiltInAtomicType.STRING,
      ONE,
      0)
      .arg(0, BuiltInAtomicType.STRING, OPT, StringValue.EMPTY_STRING)
      .arg(1, NumericType.getInstance, ONE, null)
    register("substring",
      3,
      () => new Substring,
      BuiltInAtomicType.STRING,
      ONE,
      0)
      .arg(0, BuiltInAtomicType.STRING, OPT, StringValue.EMPTY_STRING)
      .arg(1, NumericType.getInstance, ONE, null)
      .arg(2, NumericType.getInstance, ONE, null)
    register("substring-after",
      2,
      () => new SubstringAfter,
      BuiltInAtomicType.STRING,
      ONE,
      DCOLL)
      .arg(0, BuiltInAtomicType.STRING, OPT, null)
      .arg(1, BuiltInAtomicType.STRING, OPT, null)
    register("substring-after",
      3,
      () => new CollatingFunctionFree,
      BuiltInAtomicType.STRING,
      ONE,
      BASE)
      .arg(0, BuiltInAtomicType.STRING, OPT, null)
      .arg(1, BuiltInAtomicType.STRING, OPT, null)
      .arg(2, BuiltInAtomicType.STRING, ONE, null)
    register("substring-before",
      2,
      () => new SubstringBefore,
      BuiltInAtomicType.STRING,
      ONE,
      DCOLL)
      .arg(0, BuiltInAtomicType.STRING, OPT, null)
      .arg(1, BuiltInAtomicType.STRING, OPT, StringValue.EMPTY_STRING)
    register("substring-before",
      3,
      () => new CollatingFunctionFree,
      BuiltInAtomicType.STRING,
      ONE,
      BASE)
      .arg(0, BuiltInAtomicType.STRING, OPT, null)
      .arg(1, BuiltInAtomicType.STRING, OPT, StringValue.EMPTY_STRING)
      .arg(2, BuiltInAtomicType.STRING, ONE, null)
    register("sum", 1, () => new Sum, BuiltInAtomicType.ANY_ATOMIC, ONE, UO)
      .arg(0, BuiltInAtomicType.ANY_ATOMIC, STAR, null)
    register("sum", 2, () => new Sum, BuiltInAtomicType.ANY_ATOMIC, OPT, UO)
      .arg(0, BuiltInAtomicType.ANY_ATOMIC, STAR, null)
      .arg(1, BuiltInAtomicType.ANY_ATOMIC, OPT, null)
    register("timezone-from-date",
      1,
      () => new AccessorFn.TimezoneFromDate,
      BuiltInAtomicType.DAY_TIME_DURATION,
      OPT,
      0).arg(0, BuiltInAtomicType.DATE, OPT, EMPTY)
    register("timezone-from-dateTime",
      1,
      () => new AccessorFn.TimezoneFromDateTime,
      BuiltInAtomicType.DAY_TIME_DURATION,
      OPT,
      0).arg(0, BuiltInAtomicType.DATE_TIME, OPT, EMPTY)
    register("timezone-from-time",
      1,
      () => new AccessorFn.TimezoneFromTime,
      BuiltInAtomicType.DAY_TIME_DURATION,
      OPT,
      0).arg(0, BuiltInAtomicType.TIME, OPT, EMPTY)
    register("tokenize",
      2,
      () => new RegexFunctionSansFlags,
      BuiltInAtomicType.STRING,
      STAR,
      0)
      .arg(0, BuiltInAtomicType.STRING, OPT, EMPTY)
      .arg(1, BuiltInAtomicType.STRING, ONE, null)
    register("tokenize",
      3,
      () => new Tokenize_3,
      BuiltInAtomicType.STRING,
      STAR,
      0)
      .arg(0, BuiltInAtomicType.STRING, OPT, EMPTY)
      .arg(1, BuiltInAtomicType.STRING, ONE, null)
      .arg(2, BuiltInAtomicType.STRING, ONE, null)
    register("trace", 2, () => new Trace, Type.ITEM_TYPE, STAR, AS_ARG0 | LATE)
      .arg(0, Type.ITEM_TYPE, STAR | TRA, null)
      .arg(1, BuiltInAtomicType.STRING, ONE, null)
    register("translate",
      3,
      () => new Translate,
      BuiltInAtomicType.STRING,
      ONE,
      0)
      .arg(0, BuiltInAtomicType.STRING, OPT, StringValue.EMPTY_STRING)
      .arg(1, BuiltInAtomicType.STRING, ONE, null)
      .arg(2, BuiltInAtomicType.STRING, ONE, null)
    register("true",
      0,
      () => new ConstantFunction.True,
      BuiltInAtomicType.BOOLEAN,
      ONE,
      0)
    register("unordered",
      1,
      () => new Unordered,
      Type.ITEM_TYPE,
      STAR,
      AS_ARG0 | FILTER | UO).arg(0, Type.ITEM_TYPE, STAR | TRA, EMPTY)
    register("upper-case",
      1,
      () => new UpperCase,
      BuiltInAtomicType.STRING,
      ONE,
      0).arg(0, BuiltInAtomicType.STRING, OPT, StringValue.EMPTY_STRING)
    register("year-from-date",
      1,
      () => new AccessorFn.YearFromDate,
      BuiltInAtomicType.INTEGER,
      OPT,
      CARD0).arg(0, BuiltInAtomicType.DATE, OPT, EMPTY)
    register("year-from-dateTime",
      1,
      () => new AccessorFn.YearFromDateTime,
      BuiltInAtomicType.INTEGER,
      OPT,
      CARD0).arg(0, BuiltInAtomicType.DATE_TIME, OPT, EMPTY)
    register("years-from-duration",
      1,
      () => new AccessorFn.YearsFromDuration,
      BuiltInAtomicType.INTEGER,
      OPT,
      CARD0).arg(0, BuiltInAtomicType.DURATION, OPT, EMPTY)
    register("zero-or-one",
      1,
      () => new TreatFn.ZeroOrOne,
      Type.ITEM_TYPE,
      OPT,
      AS_ARG0 | FILTER).arg(0, Type.ITEM_TYPE, STAR | TRA, null)
  }
}
