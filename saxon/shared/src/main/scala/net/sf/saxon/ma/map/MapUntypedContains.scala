package net.sf.saxon.ma.map

import net.sf.saxon.expr.XPathContext

import net.sf.saxon.functions.SystemFunction

import net.sf.saxon.lib.ConversionRules

import net.sf.saxon.model._

import net.sf.saxon.om.Sequence

import net.sf.saxon.trans.XPathException

import net.sf.saxon.value.AtomicValue

import net.sf.saxon.value.BooleanValue

import net.sf.saxon.value.UntypedAtomicValue

import scala.jdk.CollectionConverters._

class MapUntypedContains extends SystemFunction {

  def call(context: XPathContext, arguments: Array[Sequence]): Sequence = {
    val rules: ConversionRules = context.getConfiguration.getConversionRules
    val map: MapItem = arguments(0).head.asInstanceOf[MapItem]
    val key: AtomicValue = arguments(1).head.asInstanceOf[AtomicValue]
    if (key.isInstanceOf[UntypedAtomicValue]) {
      for (prim <- map.getKeyUType.decompose().asScala) {
        val t: BuiltInAtomicType =
          prim.toItemType.asInstanceOf[BuiltInAtomicType]
        var converter: StringConverter = t.getStringConverter(rules)
        var av: ConversionResult = converter.convert(key)
        if (av.isInstanceOf[ValidationFailure]) {
          if (prim == PrimitiveUType.DECIMAL) {
            converter = BuiltInAtomicType.DOUBLE.getStringConverter(rules)
            av = converter.convert(key)
            if (av.isInstanceOf[AtomicValue]) {
              if (map.get(av.asAtomic()) != null) {
                BooleanValue.TRUE
              }
            }
          }
        } else if (map.get(av.asAtomic()) != null) {
          BooleanValue.TRUE
        }
      }
      BooleanValue.FALSE
    } else if (key.isNaN) {
      BooleanValue.FALSE
    }
    val result: Boolean = map.get(key) != null
    BooleanValue.get(result)
  }

}
