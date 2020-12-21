package org.orbeon.saxon.expr.sort

import org.orbeon.saxon.expr.XPathContext
import org.orbeon.saxon.lib.StringCollator
import org.orbeon.saxon.model.{BuiltInAtomicType, Type}
import org.orbeon.saxon.om.StandardNames
import org.orbeon.saxon.trans.XPathException
import org.orbeon.saxon.value.{AtomicValue, CalendarValue, StringValue}

import scala.beans.BeanProperty


object GenericAtomicComparer {

  def makeAtomicComparer(type0: BuiltInAtomicType,
                         type1: BuiltInAtomicType,
                         collator: StringCollator,
                         context: XPathContext): AtomicComparer = {
    val fp0 = type0.getFingerprint
    val fp1 = type1.getFingerprint
    if (fp0 == fp1) {
      fp0 match {
        case StandardNames.XS_DATE_TIME         |
             StandardNames.XS_DATE              |
             StandardNames.XS_TIME              |
             StandardNames.XS_G_DAY             |
             StandardNames.XS_G_MONTH           |
             StandardNames.XS_G_YEAR            |
             StandardNames.XS_G_MONTH_DAY       |
             StandardNames.XS_G_YEAR_MONTH =>
          return new CalendarValueComparer(context)
        case StandardNames.XS_BOOLEAN           |
             StandardNames.XS_DAY_TIME_DURATION |
             StandardNames.XS_YEAR_MONTH_DURATION =>
          return ComparableAtomicValueComparer.getInstance
        case StandardNames.XS_BASE64_BINARY     |
             StandardNames.XS_HEX_BINARY =>
          return ComparableAtomicValueComparer.getInstance
        case StandardNames.XS_QNAME             |
             StandardNames.XS_NOTATION =>
          return EqualityComparer.getInstance
        case _ =>
      }
    }
    if (type0.isPrimitiveNumeric && type1.isPrimitiveNumeric)
      ComparableAtomicValueComparer.getInstance
    else if ((fp0 == StandardNames.XS_STRING || fp0 == StandardNames.XS_UNTYPED_ATOMIC || fp0 == StandardNames.XS_ANY_URI) &&
        (fp1 == StandardNames.XS_STRING || fp1 == StandardNames.XS_UNTYPED_ATOMIC || fp1 == StandardNames.XS_ANY_URI)) {
      if (collator.isInstanceOf[CodepointCollator])
        CodepointCollatingComparer.getInstance
      else
        new CollatingAtomicComparer(collator)
    } else
      new GenericAtomicComparer(collator, context)
  }

}

class GenericAtomicComparer(@BeanProperty var collator: StringCollator,
                            @BeanProperty var context: XPathContext)
  extends AtomicComparer {

  if (collator == null)
    this.collator = CodepointCollator.getInstance

  def provideContext(context: XPathContext): GenericAtomicComparer =
    new GenericAtomicComparer(collator, context)

  def getStringCollator: StringCollator = collator

  def compareAtomicValues(a: AtomicValue, b: AtomicValue): Int = {

    if (a == null)
      return if (b == null) 0 else -1
    else if (b == null)
      return +1

    if (a.isInstanceOf[StringValue] && b.isInstanceOf[StringValue]) {
      if (collator.isInstanceOf[CodepointCollator])
        CodepointCollator.compareCS(a.getStringValueCS, b.getStringValueCS)
      else
        collator.compareStrings(a.getStringValue, b.getStringValue)
    } else {
      val implicitTimezone = context.getImplicitTimezone
      val ac = a.getXPathComparable(ordered = true, collator, implicitTimezone).asInstanceOf[Comparable[AtomicValue]]
      val bc = b.getXPathComparable(ordered = true, collator, implicitTimezone).asAtomic()
      if (ac == null || bc == null) {
        val e = new XPathException(
          "Objects are not comparable (" + Type.displayTypeName(a) +
            ", " +
            Type.displayTypeName(b) +
            ')',
          "XPTY0004")
        throw new ComparisonException(e)
      } else {
        ac.compareTo(bc)
      }
    }
  }

  def comparesEqual(a: AtomicValue, b: AtomicValue): Boolean =
    a match {
      case _: StringValue if b.isInstanceOf[StringValue] =>
        collator.comparesEqual(a.getStringValue, b.getStringValue)
      case calendarValue: CalendarValue if b.isInstanceOf[CalendarValue] =>
        calendarValue.compareTo(b.asInstanceOf[CalendarValue], context.getImplicitTimezone) == 0
      case _ =>
        val implicitTimezone = context.getImplicitTimezone
        val ac: AnyRef = a.getXPathComparable(ordered = false, collator, implicitTimezone)
        val bc: AnyRef = b.getXPathComparable(ordered = false, collator, implicitTimezone)
        ac == bc
    }

  def save(): String = "GAC|" + collator.getCollationURI

  override def hashCode: Int = collator.hashCode

  override def equals(obj: Any): Boolean = obj match {
    case obj: GenericAtomicComparer => collator == obj.collator
    case _ => false
  }
}
