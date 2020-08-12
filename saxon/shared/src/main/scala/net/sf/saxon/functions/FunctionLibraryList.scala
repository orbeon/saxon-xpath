package net.sf.saxon.functions

import net.sf.saxon.expr.Expression

import net.sf.saxon.expr.StaticContext

import net.sf.saxon.lib.Feature

import net.sf.saxon.lib.Logger

import net.sf.saxon.om.Function

import net.sf.saxon.om.StructuredQName

import net.sf.saxon.query.XQueryFunction

import net.sf.saxon.query.XQueryFunctionBinder

import net.sf.saxon.trans.SymbolicName

import scala.jdk.CollectionConverters._

import java.util.ArrayList

import java.util.List

class FunctionLibraryList extends FunctionLibrary with XQueryFunctionBinder {

  var libraryList: List[FunctionLibrary] = new ArrayList(8)

  def addFunctionLibrary(lib: FunctionLibrary): Int = {
    libraryList.add(lib)
    libraryList.size - 1
  }

  def get(n: Int): FunctionLibrary = libraryList.get(n)

  def getFunctionItem(functionName: SymbolicName.F,
                      staticContext: StaticContext): Function = {
    for (lib <- libraryList.asScala) {
      val fi: Function = lib.getFunctionItem(functionName, staticContext)
      if (fi != null) {
        fi
      }
    }
    null
  }

  def isAvailable(functionName: SymbolicName.F): Boolean =
    libraryList.asScala
      .find(_.isAvailable(functionName))
      .map(_ => true)
      .getOrElse(false)

  def bind(functionName: SymbolicName.F,
           staticArgs: Array[Expression],
           env: StaticContext,
           reasons: List[String]): Expression = {
    val debug: Boolean =
      env.getConfiguration.getBooleanProperty(Feature.TRACE_EXTERNAL_FUNCTIONS)
    val err: Logger = env.getConfiguration.getLogger
    if (debug) {
      err.info(
        "Looking for function " + functionName.getComponentName.getEQName +
          "#" +
          functionName.getArity)
    }
    for (lib <- libraryList.asScala) {
      if (debug) {
        err.info("Trying " + lib.getClass.getName)
      }
      val func: Expression = lib.bind(functionName, staticArgs, env, reasons)
      if (func != null) {
        func
      }
    }
    if (debug) {
      err.info(
        "Function " + functionName.getComponentName.getEQName +
          " not found!")
    }
    null
  }

  def getDeclaration(functionName: StructuredQName,
                     staticArgs: Int): XQueryFunction = {
    for (lib <- libraryList.asScala if lib.isInstanceOf[XQueryFunctionBinder]) {
      val func: XQueryFunction = lib
        .asInstanceOf[XQueryFunctionBinder]
        .getDeclaration(functionName, staticArgs)
      if (func != null) {
        func
      }
    }
    null
  }

  def getLibraryList(): List[FunctionLibrary] = libraryList

  def copy(): FunctionLibrary = {
    val fll: FunctionLibraryList = new FunctionLibraryList()
    fll.libraryList = new ArrayList(libraryList.size)
    for (i <- 0 until libraryList.size) {
      fll.libraryList.add(libraryList.get(i).copy())
    }
    fll
  }

}
