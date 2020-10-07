package org.orbeon.saxon.s9api

import org.orbeon.saxon.lib.ConversionRules

import org.orbeon.saxon.model.TypeHierarchy

import java.util.Objects

import scala.beans.{BeanProperty, BooleanBeanProperty}

class ConstructedItemType(var underlyingType: org.orbeon.saxon.model.ItemType,
                          @BeanProperty var processor: Processor)
  extends ItemType {

  Objects.requireNonNull(processor)

  Objects.requireNonNull(underlyingType)

  override def getConversionRules(): ConversionRules =
    processor.getUnderlyingConfiguration.getConversionRules

  def matches(item: XdmItem): Boolean = {
    val th: TypeHierarchy =
      processor.getUnderlyingConfiguration.getTypeHierarchy
    underlyingType.matches(item.getUnderlyingValue, th)
  }

  def subsumes(other: ItemType): Boolean = {
    val th: TypeHierarchy =
      processor.getUnderlyingConfiguration.getTypeHierarchy
    th.isSubType(other.getUnderlyingItemType, underlyingType)
  }

  def getUnderlyingItemType(): org.orbeon.saxon.model.ItemType = underlyingType

}
