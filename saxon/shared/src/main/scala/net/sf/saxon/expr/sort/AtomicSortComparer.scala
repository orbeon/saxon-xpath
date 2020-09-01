////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
package net.sf.saxon.expr.sort

import net.sf.saxon.expr.XPathContext
import net.sf.saxon.lib.StringCollator
import net.sf.saxon.model.Type
import net.sf.saxon.om.StandardNames
import net.sf.saxon.trans.NoDynamicContextException
import net.sf.saxon.trans.XPathException
import net.sf.saxon.value.{AtomicValue, HexBinaryValue, NumericValue, QNameValue, StringValue, UntypedAtomicValue}
import AtomicSortComparer._

import scala.beans.{BeanProperty, BooleanBeanProperty}

object AtomicSortComparer {

  def makeSortComparer(collator: StringCollator,
                       itemType: Int,
                       context: XPathContext): AtomicComparer =
    itemType match {
      case StandardNames.XS_STRING | StandardNames.XS_UNTYPED_ATOMIC |
          StandardNames.XS_ANY_URI =>
        if (collator.isInstanceOf[CodepointCollator]) {
          CodepointCollatingComparer.getInstance
        } else {
          new CollatingAtomicComparer(collator)
        }
      case StandardNames.XS_INTEGER | StandardNames.XS_DECIMAL =>
        DecimalSortComparer.getDecimalSortComparerInstance
      case StandardNames.XS_DOUBLE | StandardNames.XS_FLOAT |
          StandardNames.XS_NUMERIC =>
        DoubleSortComparer.getInstance
      case StandardNames.XS_DATE_TIME | StandardNames.XS_DATE |
          StandardNames.XS_TIME =>
        new CalendarValueComparer(context)
      case _ => // use the general-purpose comparer that handles all types
        new AtomicSortComparer(collator, itemType, context)

    }

  var COLLATION_KEY_NaN: AtomicMatchKey = new AtomicMatchKey() {
    def asAtomic(): AtomicValue =
      new QNameValue("saxon", "http://saxon.sf.net/collation-key", "NaN")
  }

}

class AtomicSortComparer  (
    @BeanProperty var collator: StringCollator,
    @BeanProperty var itemType: Int,
    @transient private var context: XPathContext)
    extends AtomicComparer {

  if (collator == null) {
    this.collator = CodepointCollator.getInstance
  }

  def provideContext(context: XPathContext): AtomicComparer =
    new AtomicSortComparer(collator, itemType, context)

  def getStringCollator: StringCollator = collator

  def compareAtomicValues(a: AtomicValue, b: AtomicValue): Int = {
    if (a == null) {
      if (b == null) {
        return 0
      } else {
        return -1
      }
    } else if (b == null) {
      return +1
    }
    if (a.isNaN) {
      if (b.isNaN) 0 else -1
    } else if (b.isNaN) {
      +1
    } else if (a.isInstanceOf[StringValue] && b.isInstanceOf[StringValue]) {
      if (collator.isInstanceOf[CodepointCollator]) {
        CodepointCollator.compareCS(a.getStringValueCS, b.getStringValueCS)
      } else {
        collator.compareStrings(a.getStringValue, b.getStringValue)
      }
    } else {
      val implicitTimezone: Int = context.getImplicitTimezone
      val ac: Comparable[AtomicValue] = a
        .getXPathComparable(ordered = true, collator, implicitTimezone)
        .asInstanceOf[Comparable[AtomicValue]]
      val bc: AtomicValue= b.getXPathComparable(ordered = true, collator, implicitTimezone).asAtomic()
      if (ac == null || bc == null) {
        compareNonComparables(a, b)
      } else {
        try ac.compareTo(bc)
        catch {
          case e: ClassCastException => {
            var message: String = "Cannot compare " + a.getPrimitiveType.getDisplayName +
                " with " +
                b.getPrimitiveType.getDisplayName
            if (a.isInstanceOf[UntypedAtomicValue] || b
                  .isInstanceOf[UntypedAtomicValue]) {
              message += ". Further information: see http://saxonica.plan.io/issues/3450"
            }
            throw new ClassCastException(message)
          }

        }
      }
    }
  }
// System.err.println("Comparing " + a.getClass() + "(" + a + ") with " + b.getClass() + "(" + b + ") using " + collator);
// Delete the following five lines to fix bug 3450
//if (a instanceof UntypedAtomicValue) {
//    return ((UntypedAtomicValue) a).compareTo(b, collator, context);
//} else if (b instanceof UntypedAtomicValue) {
//    return -((UntypedAtomicValue) b).compareTo(a, collator, context);
//} else
// End of fix for 3450
// System.err.println("Comparing " + a.getClass() + "(" + a + ") with " + b.getClass() + "(" + b + ") using " + collator);
// Delete the following five lines to fix bug 3450
//if (a instanceof UntypedAtomicValue) {
//    return ((UntypedAtomicValue) a).compareTo(b, collator, context);
//} else if (b instanceof UntypedAtomicValue) {
//    return -((UntypedAtomicValue) b).compareTo(a, collator, context);
//} else
// End of fix for 3450

   def compareNonComparables(a: AtomicValue, b: AtomicValue): Int = {
    val err = new XPathException(
      "Values are not comparable (" + Type.displayTypeName(a) +
        ", " +
        Type.displayTypeName(b) +
        ')',
      "XPTY0004")
    throw new ComparisonException(err)
  }

  def comparesEqual(a: AtomicValue, b: AtomicValue): Boolean =
    compareAtomicValues(a, b) == 0

  /**
    * Create a string representation of this AtomicComparer that can be saved in a compiled
    * package and used to reconstitute the AtomicComparer when the package is reloaded
    *
    * @return a string representation of the AtomicComparer
    */
  def save(): String = "AtSC|" + itemType + "|" + getCollator.getCollationURI

}

// Copyright (c) 2018-2020 Saxonica Limited
// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is "Incompatible With Secondary Licenses", as defined by the Mozilla Public License, v. 2.0.
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
/**
  * An AtomicComparer used for comparing atomic values of arbitrary item types. It encapsulates
  * a collator that is used when the values to be compared are strings. It also supports
  * a separate method for testing equality of items, which can be used for data types that
  * are not ordered.
  * <p>The AtomicSortComparer is identical to the GenericAtomicComparer except for its handling
  * of NaN: it treats NaN values as lower than any other value, and as equal to each other.</p>
  *
  */
