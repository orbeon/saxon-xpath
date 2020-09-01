////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// Copyright (c) 2018-2020 Saxonica Limited
// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is "Incompatible With Secondary Licenses", as defined by the Mozilla Public License, v. 2.0.
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
/**
  * A StaticContext contains the information needed while an expression or pattern
  * is being parsed. The information is also sometimes needed at run-time.
  */

package net.sf.saxon.expr

import net.sf.saxon.expr.parser.{OptimizerOptions, RetainedStaticContext}
import net.sf.saxon.functions.FunctionLibrary
import net.sf.saxon.model.ItemType
import net.sf.saxon.om.{NamespaceResolver, StructuredQName}
import net.sf.saxon.s9api.{Location, UnprefixedElementMatchingPolicy}
import net.sf.saxon.s9api.UnprefixedElementMatchingPolicy.UnprefixedElementMatchingPolicy
import net.sf.saxon.trans.DecimalFormatManager
import net.sf.saxon.utils.Configuration

import scala.collection.Set


trait StaticContext {

  def getConfiguration: Configuration
  def getPackageData: PackageData
  def makeEarlyEvaluationContext(): XPathContext
  def makeRetainedStaticContext(): RetainedStaticContext
  def getContainingLocation: Location
  def issueWarning(message: String, locator: Location): Unit
  def getSystemId: String
  def getStaticBaseURI: String
  def bindVariable(qName: StructuredQName): Expression
  def getFunctionLibrary: FunctionLibrary
  def getDefaultCollationName: String
  def getDefaultElementNamespace: String
  def getUnprefixedElementMatchingPolicy: UnprefixedElementMatchingPolicy = UnprefixedElementMatchingPolicy.DEFAULT_NAMESPACE
  def getDefaultFunctionNamespace: String
  def isInBackwardsCompatibleMode: Boolean
  def isImportedSchema(namespace: String): Boolean
  def getImportedSchemaNamespaces: Set[String]
  def getNamespaceResolver: NamespaceResolver
  def getRequiredContextItemType: ItemType
  def getDecimalFormatManager: DecimalFormatManager
  def getXPathVersion: Int
  /*def getKeyManager(): KeyManager*/ // KeyManager class not found
  def resolveTypeAlias(typeName: StructuredQName): ItemType
  def getOptimizerOptions: OptimizerOptions = getConfiguration.getOptimizerOptions
}
