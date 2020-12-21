package org.orbeon.saxon.expr
import java.{util => ju}

import org.orbeon.saxon.expr.accum.AccumulatorRegistry
import org.orbeon.saxon.expr.instruct.{GlobalVariable, SlotManager}
import org.orbeon.saxon.s9api.HostLanguage
import org.orbeon.saxon.s9api.HostLanguage.HostLanguage
import org.orbeon.saxon.trans.{DecimalFormatManager, TypeAliasManager}
import org.orbeon.saxon.utils.Configuration

import scala.beans.{BeanProperty, BooleanBeanProperty}

class PackageData(var config: Configuration) {

  @BeanProperty
  var hostLanguage: HostLanguage = _
  var isSchemaAware: Boolean = _
  private var decimalFormatManager: DecimalFormatManager = null
  @BeanProperty
  var accumulatorRegistry: AccumulatorRegistry = null
  private var globalVariables: ju.List[GlobalVariable] = new ju.ArrayList()
  @BeanProperty
  var globalSlotManager: SlotManager = config.makeSlotManager
  @BeanProperty
  var localLicenseId: Int = -1
  @BeanProperty
  var targetEdition: String = config.getEditionCode
  @BooleanBeanProperty
  var relocatable: Boolean = _
  private var typeAliasManager: TypeAliasManager = _

  if (config == null)
    throw new NullPointerException()

  def getConfiguration: Configuration = config

  def setConfiguration(configuration: Configuration): Unit =
    this.config = configuration

  def isXSLT: Boolean = hostLanguage == HostLanguage.XSLT

  def setSchemaAware(schemaAware: Boolean): Unit =
    isSchemaAware = schemaAware

  def setDecimalFormatManager(manager: DecimalFormatManager): Unit =
    decimalFormatManager = manager

  def getDecimalFormatManager: DecimalFormatManager = {
    if (decimalFormatManager == null)
      decimalFormatManager = new DecimalFormatManager(hostLanguage, 31)
    decimalFormatManager
  }

  def addGlobalVariable(variable: GlobalVariable): Unit =
    globalVariables.add(variable)

  def getGlobalVariableList: ju.List[GlobalVariable] = globalVariables

  def setTypeAliasManager(manager: TypeAliasManager): Unit =
    this.typeAliasManager = manager

  def obtainTypeAliasManager: TypeAliasManager = {
    if (typeAliasManager == null)
      typeAliasManager = config.makeTypeAliasManager
    typeAliasManager
  }
}
