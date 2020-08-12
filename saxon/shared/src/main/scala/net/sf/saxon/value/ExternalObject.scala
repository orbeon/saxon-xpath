////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
package net.sf.saxon.value

import net.sf.saxon.model.ItemType

import net.sf.saxon.model.TypeHierarchy

import net.sf.saxon.om.Item




trait ExternalObject[T] extends Item {

  def getObject(): T

  def getItemType(th: TypeHierarchy): ItemType

}
