package net.sf.saxon.expr

import net.sf.saxon.utils.Configuration

import net.sf.saxon.expr.parser.RoleDiagnostic

import net.sf.saxon.model.BuiltInAtomicType

import net.sf.saxon.model.ItemType

import net.sf.saxon.model.TypeHierarchy

import net.sf.saxon.model.UType

import net.sf.saxon.om.Item

import net.sf.saxon.s9api.Location

import net.sf.saxon.trans.XPathException

class ItemTypeCheckingFunction extends ItemMappingFunction {

  private var requiredItemType: ItemType = _
  private var role: RoleDiagnostic = _
  private var failingExpression: Expression = _
  private var config: Configuration = _
  private var location: Location = _

  def this(requiredItemType: ItemType, role: RoleDiagnostic, failingExpression: Expression, config: Configuration) {
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
    val th: TypeHierarchy = config.getTypeHierarchy
    if (requiredItemType.matches(item, th)) {} else if (requiredItemType.getUType
      .subsumes(
        UType.STRING) && BuiltInAtomicType.ANY_URI
      .matches(item, th)) {} else {
      val message: String =
        role.composeErrorMessage(requiredItemType, item, th)
      val errorCode: String = role.getErrorCode
      if ("XPDY0050" == errorCode) {
        val te: XPathException = new XPathException(message, errorCode)
        te.setFailingExpression(failingExpression)
        te.setLocator(location)
        te.setIsTypeError(false)
        throw te
      } else {
        val te: XPathException = new XPathException(message, errorCode)
        te.setFailingExpression(failingExpression)
        te.setLocator(location)
        te.setIsTypeError(true)
        throw te
      }
    }
  }

}
