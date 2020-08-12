package net.sf.saxon.expr

import net.sf.saxon.utils.Configuration
import net.sf.saxon.expr.accum.AccumulatorRegistry
import net.sf.saxon.expr.instruct.GlobalVariable
import net.sf.saxon.expr.instruct.SlotManager
import net.sf.saxon.s9api.HostLanguage
import net.sf.saxon.trans.DecimalFormatManager
import net.sf.saxon.trans.TypeAliasManager
import java.util.ArrayList
import java.util.List

import net.sf.saxon.s9api.HostLanguage.HostLanguage

import scala.beans.{BeanProperty, BooleanBeanProperty}

class PackageData(var config: Configuration) {

  @BeanProperty
  var hostLanguage: HostLanguage = _

  var isSchemaAware: Boolean = _

  private var decimalFormatManager: DecimalFormatManager = null

  //  var keyManager: KeyManager = null // not found this class

  @BeanProperty
  var accumulatorRegistry: AccumulatorRegistry = null

  private var globalVariables: List[GlobalVariable] = new ArrayList()

  @BeanProperty
  var globalSlotManager: SlotManager = config.makeSlotManager

  @BeanProperty
  var localLicenseId: Int = -1

  @BeanProperty
  var targetEdition: String = config.getEditionCode

  @BooleanBeanProperty
  var relocatable: Boolean = _

  private var typeAliasManager: TypeAliasManager = _

  if (config == null) {
    throw new NullPointerException()
  }

  def getConfiguration(): Configuration = config

  def setConfiguration(configuration: Configuration): Unit = {
    this.config = configuration
  }

  def isXSLT(): Boolean = hostLanguage == HostLanguage.XSLT

  def setSchemaAware(schemaAware: Boolean): Unit = {
    isSchemaAware = schemaAware
  }

  def setDecimalFormatManager(manager: DecimalFormatManager): Unit = {
    decimalFormatManager = manager
  }

  def getDecimalFormatManager = {
    if (decimalFormatManager == null) decimalFormatManager = new DecimalFormatManager(hostLanguage, 31)
    decimalFormatManager
  }

  // not found keyManager class

  /*def getKeyManager(): KeyManager = {
    if (keyManager == null) {
      keyManager = new KeyManager(getConfiguration, this)
    }
    keyManager
  }*/ // not found  this class

 /* def setKeyManager(manager: KeyManager): Unit = {
    keyManager = manager
  }*/

  def addGlobalVariable(variable: GlobalVariable): Unit = {
    globalVariables.add(variable)
  }

  def getGlobalVariableList(): List[GlobalVariable] = globalVariables

  def setTypeAliasManager(manager: TypeAliasManager): Unit = {
    this.typeAliasManager = manager
  }

  def obtainTypeAliasManager(): TypeAliasManager = {
    if (typeAliasManager == null) {
      typeAliasManager = config.makeTypeAliasManager
    }
    typeAliasManager
  }

}
