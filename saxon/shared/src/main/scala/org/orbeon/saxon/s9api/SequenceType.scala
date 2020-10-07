package org.orbeon.saxon.s9api

import org.orbeon.saxon.s9api.OccurrenceIndicator.OccurrenceIndicator

import scala.beans.BeanProperty


object SequenceType {

  val ANY: SequenceType =
    new SequenceType(ItemType.ANY_ITEM, OccurrenceIndicator.ZERO_OR_MORE)

  val EMPTY: SequenceType =
    new SequenceType(ItemType.ERROR, OccurrenceIndicator.ZERO)

  def makeSequenceType(
                        itemType: ItemType,
                        occurrenceIndicator: OccurrenceIndicator): SequenceType =
    new SequenceType(itemType, occurrenceIndicator)

  def fromUnderlyingSequenceType(
                                  processor: Processor,
                                  st: org.orbeon.saxon.value.SequenceType): SequenceType = {
    val factory: ItemTypeFactory = new ItemTypeFactory(processor)
    val it: ItemType = factory.exposeItemType(st.getPrimaryType)
    val oc: OccurrenceIndicator =
      OccurrenceIndicator.getOccurrenceIndicator(st.getCardinality)
    makeSequenceType(it, oc)
  }

}

class SequenceType private (
                             @BeanProperty var itemType: ItemType,
                             @BeanProperty var occurrenceIndicator: OccurrenceIndicator) {

  override def equals(other: Any): Boolean =
    other.isInstanceOf[SequenceType] &&
      other
        .asInstanceOf[SequenceType]
        .getOccurrenceIndicator == getOccurrenceIndicator &&
      other.asInstanceOf[SequenceType].getItemType == getItemType

  override def hashCode: Int =
    getItemType.hashCode ^ (getOccurrenceIndicator.hashCode << 17)

  def getUnderlyingSequenceType: org.orbeon.saxon.value.SequenceType =
    org.orbeon.saxon.value.SequenceType.makeSequenceType(
      itemType.getUnderlyingItemType,
      occurrenceIndicator.getCardinality)

}
