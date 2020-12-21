package org.orbeon.saxon.query

import org.orbeon.saxon.expr.Expression

import org.orbeon.saxon.expr.StaticContext

import org.orbeon.saxon.expr.parser.RetainedStaticContext

import org.orbeon.saxon.functions.FunctionLibrary

import org.orbeon.saxon.om.Function

import org.orbeon.saxon.om.StructuredQName

import org.orbeon.saxon.trans.SymbolicName

import org.orbeon.saxon.trans.XPathException

import java.util.HashSet

import java.util.List

//import scala.collection.compat._
import scala.jdk.CollectionConverters._

class ImportedFunctionLibrary(
                               @transient private var importingModule: QueryModule,
                               private var baseLibrary: XQueryFunctionLibrary)
  extends FunctionLibrary
    with XQueryFunctionBinder {

  private var namespaces: HashSet[String] = new HashSet(5)

  def addImportedNamespace(namespace: String): Unit = {
    namespaces.add(namespace)
  }

  def bind(symbolicName: SymbolicName.F,
           staticArgs: Array[Expression],
           env: StaticContext,
           reasons: List[String]): Expression = {
    val functionName: StructuredQName = symbolicName.getComponentName
    val uri: String = functionName.getURI
    val rsc: RetainedStaticContext = new RetainedStaticContext(env)
    for (arg <- staticArgs if arg.getLocalRetainedStaticContext == null) {
      arg.setRetainedStaticContext(rsc)
    }
    if (namespaces.contains(uri)) {
      baseLibrary.bind(symbolicName, staticArgs, env, reasons)
    } else {
      null
    }
  }

  def getDeclaration(functionName: StructuredQName,
                     staticArgs: Int): XQueryFunction = {
    val uri: String = functionName.getURI
    if (namespaces.contains(uri)) {
      baseLibrary.getDeclaration(functionName, staticArgs)
    } else {
      null
    }
  }

  def copy(): FunctionLibrary = {
    val lib: ImportedFunctionLibrary =
      new ImportedFunctionLibrary(importingModule, baseLibrary)
    for (ns <- namespaces.asScala) {
      lib.addImportedNamespace(ns)
    }
    lib
  }

  def setImportingModule(importingModule: QueryModule): Unit = {
    this.importingModule = importingModule
  }

  def getFunctionItem(functionName: SymbolicName.F,
                      staticContext: StaticContext): Function =
    if (namespaces.contains(functionName.getComponentName.getURI)) {
      baseLibrary.getFunctionItem(functionName, staticContext)
    } else {
      null
    }

  def isAvailable(functionName: SymbolicName.F): Boolean =
    namespaces.contains(functionName.getComponentName.getURI) &&
      baseLibrary.isAvailable(functionName)

}
