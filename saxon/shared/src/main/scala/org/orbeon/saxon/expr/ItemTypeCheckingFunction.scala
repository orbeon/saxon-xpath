package org.orbeon.saxon.expr

import org.orbeon.saxon.expr.parser.RoleDiagnostic
import org.orbeon.saxon.model.{BuiltInAtomicType, ItemType, UType}
import org.orbeon.saxon.om.Item
import org.orbeon.saxon.s9api.Location
import org.orbeon.saxon.trans.XPathException
import org.orbeon.saxon.utils.Configuration


class ItemTypeCheckingFunction extends ItemMappingFunction {

  private var requiredItemType: ItemType = _
  private var role: RoleDiagnostic = _
  private var failingExpression: Expression = _
  private var config: Configuration = _
  private var location: Location = _

  def this(requiredItemType: ItemType, role: RoleDiagnostic, failingExpression: Expression, config: Configuration) = {
    this()
    this.requiredItemType = requiredItemType
    this.role = role
    this.failingExpression = failingExpression
    this.location = failingExpression.getLocation
    this.config = config
  }

  def this(requiredItemType: ItemType,
           role: RoleDiagnostic,
           locator: Location,
           config: Configuration) = {
    this()
    this.requiredItemType = requiredItemType
    this.role = role
    this.location = locator
    this.config = config
  }

  def mapItem(item: Item): Item = {
    testConformance(item, config)
    item
  }

  private def testConformance(item: Item, config: Configuration): Unit = {
    val th = config.getTypeHierarchy
    if (requiredItemType.matches(item, th)) {
      // OK, no action
    } else if (requiredItemType.getUType.subsumes(UType.STRING) && BuiltInAtomicType.ANY_URI.matches(item, th)) {
      // OK, no action
    } else {
      val message   = role.composeErrorMessage(requiredItemType, item, th)
      val errorCode = role.getErrorCode
      if ("XPDY0050" == errorCode) {
        val te = new XPathException(message, errorCode)
        te.setFailingExpression(failingExpression)
        te.setLocator(location)
        te.setIsTypeError(false)
        throw te
      } else {
        val te = new XPathException(message, errorCode)
        te.setFailingExpression(failingExpression)
        te.setLocator(location)
        te.setIsTypeError(true)
        throw te
      }
    }
  }
}
