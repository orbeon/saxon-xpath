////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
package org.orbeon.saxon.model

import org.orbeon.saxon.pattern.NodeKindTest

import scala.beans.BeanProperty

object PrimitiveUType extends Enumeration {

  val DOCUMENT: PrimitiveUType = new PrimitiveUType(0)

  val ELEMENT: PrimitiveUType = new PrimitiveUType(1)

  val ATTRIBUTE: PrimitiveUType = new PrimitiveUType(2)

  val TEXT: PrimitiveUType = new PrimitiveUType(3)

  val COMMENT: PrimitiveUType = new PrimitiveUType(4)

  val PI: PrimitiveUType = new PrimitiveUType(5)

  val NAMESPACE: PrimitiveUType = new PrimitiveUType(6)

  val FUNCTION: PrimitiveUType = new PrimitiveUType(7)

  val STRING: PrimitiveUType = new PrimitiveUType(8)

  val BOOLEAN: PrimitiveUType = new PrimitiveUType(9)

  val DECIMAL: PrimitiveUType = new PrimitiveUType(10)

  val FLOAT: PrimitiveUType = new PrimitiveUType(11)

  val DOUBLE: PrimitiveUType = new PrimitiveUType(12)

  val DURATION: PrimitiveUType = new PrimitiveUType(13)

  val DATE_TIME: PrimitiveUType = new PrimitiveUType(14)

  val TIME: PrimitiveUType = new PrimitiveUType(15)

  val DATE: PrimitiveUType = new PrimitiveUType(16)

  val G_YEAR_MONTH: PrimitiveUType = new PrimitiveUType(17)

  val G_YEAR: PrimitiveUType = new PrimitiveUType(18)

  val G_MONTH_DAY: PrimitiveUType = new PrimitiveUType(19)

  val G_DAY: PrimitiveUType = new PrimitiveUType(20)

  val G_MONTH: PrimitiveUType = new PrimitiveUType(21)

  val HEX_BINARY: PrimitiveUType = new PrimitiveUType(22)

  val BASE64_BINARY: PrimitiveUType = new PrimitiveUType(23)

  val ANY_URI: PrimitiveUType = new PrimitiveUType(24)

  val QNAME: PrimitiveUType = new PrimitiveUType(25)

  val NOTATION: PrimitiveUType = new PrimitiveUType(26)

  val UNTYPED_ATOMIC: PrimitiveUType = new PrimitiveUType(27)

  val EXTENSION: PrimitiveUType = new PrimitiveUType(30)

  class PrimitiveUType (@BeanProperty val bit: Int) extends Val {

    def toUType: UType = new UType(1 << bit)

    override def toString: String = this match {
      case DOCUMENT => "document"
      case ELEMENT => "element"
      case ATTRIBUTE => "attribute"
      case TEXT => "text"
      case COMMENT => "comment"
      case PI => "processing-instruction"
      case NAMESPACE => "namespace"
      case FUNCTION => "function"
      case STRING => "string"
      case BOOLEAN => "boolean"
      case DECIMAL => "decimal"
      case FLOAT => "float"
      case DOUBLE => "double"
      case DURATION => "duration"
      case DATE_TIME => "dateTime"
      case TIME => "time"
      case DATE => "date"
      case G_YEAR_MONTH => "gYearMonth"
      case G_YEAR => "gYear"
      case G_MONTH_DAY => "gMonthDay"
      case G_DAY => "gDay"
      case G_MONTH => "gMoonth"
      case HEX_BINARY => "hexBinary"
      case BASE64_BINARY => "base64Binary"
      case ANY_URI => "anyURI"
      case QNAME => "QName"
      case NOTATION => "NOTATION"
      case UNTYPED_ATOMIC => "untypedAtomic"
      case EXTENSION => "external object"
      case _ => "???"

    }

    def toItemType: ItemType = this match {
      case DOCUMENT => NodeKindTest.DOCUMENT
      case ELEMENT => NodeKindTest.ELEMENT
      case ATTRIBUTE => NodeKindTest.ATTRIBUTE
      case TEXT => NodeKindTest.TEXT
      case COMMENT => NodeKindTest.COMMENT
      case PI => NodeKindTest.PROCESSING_INSTRUCTION
      case NAMESPACE => NodeKindTest.NAMESPACE
      case FUNCTION => AnyFunctionType
      case STRING => BuiltInAtomicType.STRING
      case BOOLEAN => BuiltInAtomicType.BOOLEAN
      case DECIMAL => BuiltInAtomicType.DECIMAL
      case FLOAT => BuiltInAtomicType.FLOAT
      case DOUBLE => BuiltInAtomicType.DOUBLE
      case DURATION => BuiltInAtomicType.DURATION
      case DATE_TIME => BuiltInAtomicType.DATE_TIME
      case TIME => BuiltInAtomicType.TIME
      case DATE => BuiltInAtomicType.DATE
      case G_YEAR_MONTH => BuiltInAtomicType.G_YEAR_MONTH
      case G_YEAR => BuiltInAtomicType.G_YEAR
      case G_MONTH_DAY => BuiltInAtomicType.G_MONTH_DAY
      case G_DAY => BuiltInAtomicType.G_DAY
      case G_MONTH => BuiltInAtomicType.G_MONTH
      case HEX_BINARY => BuiltInAtomicType.HEX_BINARY
      case BASE64_BINARY => BuiltInAtomicType.BASE64_BINARY
      case ANY_URI => BuiltInAtomicType.ANY_URI
      case QNAME => BuiltInAtomicType.QNAME
      case NOTATION => BuiltInAtomicType.NOTATION
      case UNTYPED_ATOMIC => BuiltInAtomicType.UNTYPED_ATOMIC
      case EXTENSION => //return JavaExternalObjectType.EXTERNAL_OBJECT_TYPE;
        AnyItemType
      case _ => throw new IllegalArgumentException()

    }

  }

  def forBit(bit: Int): PrimitiveUType = {
    var value : Option[Value] = values.find(_.id == bit)
    value.get
  }

  implicit def convertValue(v: Value): PrimitiveUType =
    v.asInstanceOf[PrimitiveUType]

}

// Copyright (c) 2013-2020 Saxonica Limited
// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is "Incompatible With Secondary Licenses", as defined by the Mozilla Public License, v. 2.0.
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
