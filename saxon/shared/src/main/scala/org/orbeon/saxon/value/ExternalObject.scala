package org.orbeon.saxon.value

import org.orbeon.saxon.model.{ItemType, TypeHierarchy}
import org.orbeon.saxon.om.Item


trait ExternalObject[T] extends Item {
  def getObject: T
  def getItemType(th: TypeHierarchy): ItemType
}
