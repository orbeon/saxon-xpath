package org.orbeon.saxon.ma.map

import org.orbeon.saxon.expr.XPathContext

import org.orbeon.saxon.functions.SystemFunction

import org.orbeon.saxon.lib.ConversionRules

import org.orbeon.saxon.model._

import org.orbeon.saxon.om.Sequence

import org.orbeon.saxon.trans.XPathException

import org.orbeon.saxon.value.AtomicValue

import org.orbeon.saxon.value.BooleanValue

import org.orbeon.saxon.value.UntypedAtomicValue

//import scala.collection.compat._
import scala.jdk.CollectionConverters._

class MapUntypedContains extends SystemFunction {

  def call(context: XPathContext, arguments: Array[Sequence]): Sequence = {
    val rules: ConversionRules = context.getConfiguration.getConversionRules
    val map: MapItem = arguments(0).head.asInstanceOf[MapItem]
    val key: AtomicValue = arguments(1).head.asInstanceOf[AtomicValue]
    if (key.isInstanceOf[UntypedAtomicValue]) {
      for (prim <- map.getKeyUType.decompose.asScala) {
        val t: BuiltInAtomicType =
          prim.toItemType.asInstanceOf[BuiltInAtomicType]
        var converter: StringConverter = t.getStringConverter(rules)
        var av: ConversionResult = converter.convert(key)
        if (av.isInstanceOf[ValidationFailure]) {
          if (prim == PrimitiveUType.DECIMAL) {
            converter = BuiltInAtomicType.DOUBLE.getStringConverter(rules)
            av = converter.convert(key)
            if (av.isInstanceOf[AtomicValue]) {
              if (map.get(av.asAtomic) != null) {
                return BooleanValue.TRUE
              }
            }
          }
        } else if (map.get(av.asAtomic) != null) {
          return BooleanValue.TRUE
        }
      }
      return BooleanValue.FALSE
    } else if (key.isNaN) {
     return BooleanValue.FALSE
    }
    val result: Boolean = map.get(key) != null
    BooleanValue.get(result)
  }

}
