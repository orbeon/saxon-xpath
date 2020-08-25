package net.sf.saxon.functions

import net.sf.saxon.expr._
import net.sf.saxon.expr.parser.ContextItemStaticInfo
import net.sf.saxon.expr.parser.ExpressionVisitor
import net.sf.saxon.model.BuiltInAtomicType
import net.sf.saxon.model.TypeHierarchy
import net.sf.saxon.om.{Item, One, Sequence, ZeroOrOne}
import net.sf.saxon.regex.EmptyString
import net.sf.saxon.regex.UnicodeString
import net.sf.saxon.trans.XPathException
import net.sf.saxon.value.Int64Value
import net.sf.saxon.value.NumericValue
import net.sf.saxon.value.StringValue
import Substring._

object Substring {

  def substring(sv: StringValue, start: NumericValue): UnicodeString = {
    val s: UnicodeString = sv.getUnicodeString
    val slength: Int = s.uLength()
    var lstart: Long = 0L
    if (start.isInstanceOf[Int64Value]) {
      lstart = start.asInstanceOf[Int64Value].longValue()
      if (lstart > slength) {
        EmptyString.THE_INSTANCE
      } else if (lstart <= 0) {
        lstart = 1
      }
    } else {
      if (start.isNaN) {
        EmptyString.THE_INSTANCE
      } else if (start.signum() <= 0) {
        return s
      } else if (start.compareTo(slength) > 0) {
        EmptyString.THE_INSTANCE
      } else {
        lstart = Math.round(start.getDoubleValue)
      }
    }
    if (lstart > s.uLength()) {
      EmptyString.THE_INSTANCE
    }
    s.uSubstring(lstart.toInt - 1, s.uLength())
  }

  def substring(sv: StringValue,
                start: NumericValue,
                len: NumericValue): UnicodeString = {
    val slength: Int = sv.getStringLengthUpperBound
    var lstart: Long = 0L
    if (start.isInstanceOf[Int64Value]) {
      lstart = start.asInstanceOf[Int64Value].longValue()
      if (lstart > slength) {
        EmptyString.THE_INSTANCE
      }
    } else {
      if (start.isNaN) {
        EmptyString.THE_INSTANCE
      } else if (start.compareTo(slength) > 0) {
        EmptyString.THE_INSTANCE
      } else {
        val dstart: Double = start.getDoubleValue
        lstart =
          if (java.lang.Double.isInfinite(dstart)) -java.lang.Integer.MAX_VALUE
          else Math.round(dstart)
      }
    }
    var llen: Long = 0L
    if (len.isInstanceOf[Int64Value]) {
      llen = len.asInstanceOf[Int64Value].longValue()
      if (llen <= 0) {
        EmptyString.THE_INSTANCE
      }
    } else {
      if (len.isNaN) {
        EmptyString.THE_INSTANCE
      }
      if (len.signum() <= 0) {
        EmptyString.THE_INSTANCE
      }
      val dlen: Double = len.getDoubleValue
      llen =
        if (java.lang.Double.isInfinite(dlen)) java.lang.Integer.MAX_VALUE
        else Math.round(len.getDoubleValue)
    }
    val lend: Long = lstart + llen
    if (lend < lstart) {
      EmptyString.THE_INSTANCE
    }
    val us: UnicodeString = sv.getUnicodeString
    val clength: Int = us.uLength()
    var a1: Int = lstart.toInt - 1
    if (a1 >= clength) {
      EmptyString.THE_INSTANCE
    }
    val a2: Int = Math.min(clength, lend.toInt - 1)
    if (a1 < 0) {
      if (a2 < 0) {
        EmptyString.THE_INSTANCE
      } else {
        a1 = 0
      }
    }
    us.uSubstring(a1, a2)
  }

}

class Substring extends SystemFunction with Callable {

  override def typeCheckCaller(caller: FunctionCall,
                               visitor: ExpressionVisitor,
                               contextInfo: ContextItemStaticInfo): Expression = {
    val e2: Expression = super.typeCheckCaller(caller, visitor, contextInfo)
    if (e2 != caller) {
      return e2
    }
    val th: TypeHierarchy = visitor.getConfiguration.getTypeHierarchy
    if (caller.getArg(1).isCallOn(classOf[Number_1])) {
      val a1: Expression =
        caller.getArg(1).asInstanceOf[StaticFunctionCall].getArg(0)
      if (th.isSubType(a1.getItemType, BuiltInAtomicType.INTEGER)) {
        caller.setArg(1, a1)
      }
    }
    if (getArity > 2 && caller.getArg(2).isCallOn(classOf[Number_1])) {
      val a2: Expression =
        caller.getArg(2).asInstanceOf[StaticFunctionCall].getArg(0)
      if (th.isSubType(a2.getItemType, BuiltInAtomicType.INTEGER)) {
        caller.setArg(2, a2)
      }
    }
    caller
  }

  def call(context: XPathContext, arguments: Array[Sequence]): ZeroOrOne[_ <:Item] = {
    val arg0: StringValue = arguments(0).head().asInstanceOf[StringValue]
    if (arg0 == null) {
      One.string("")
    }
    val arg1: NumericValue = arguments(1).head().asInstanceOf[NumericValue]
    if (arguments.length == 2) {
      new ZeroOrOne(StringValue.makeStringValue(substring(arg0, arg1)))
    } else {
      val arg2: NumericValue = arguments(2).head().asInstanceOf[NumericValue]
      new ZeroOrOne(StringValue.makeStringValue(substring(arg0, arg1, arg2)))
    }
  }

  override def getCompilerName(): String = "SubstringCompiler"

}
