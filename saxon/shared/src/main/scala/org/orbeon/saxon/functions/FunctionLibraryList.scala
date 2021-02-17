package org.orbeon.saxon.functions

import org.orbeon.saxon.expr.{Expression, StaticContext}
import org.orbeon.saxon.om.{Function, StructuredQName}
import org.orbeon.saxon.query.{XQueryFunction, XQueryFunctionBinder}
import org.orbeon.saxon.trans.SymbolicName

import java.util.{ArrayList, List}

import scala.jdk.CollectionConverters._


class FunctionLibraryList extends FunctionLibrary with XQueryFunctionBinder {

  var libraryList: List[FunctionLibrary] = new ArrayList(8)

  def addFunctionLibrary(lib: FunctionLibrary): Int = {
    libraryList.add(lib)
    libraryList.size - 1
  }

  def get(n: Int): FunctionLibrary = libraryList.get(n)

  def getFunctionItem(functionName: SymbolicName.F, staticContext: StaticContext): Function =
    libraryList.iterator.asScala.map(_.getFunctionItem(functionName, staticContext)).find(_ ne null).orNull

  def isAvailable(functionName: SymbolicName.F): Boolean =
    libraryList.asScala.exists(_.isAvailable(functionName))

  def bind(
    functionName : SymbolicName.F,
    staticArgs   : Array[Expression],
    env          : StaticContext,
    reasons      : List[String]
  ): Expression = {
//    val debug = env.getConfiguration.getBooleanProperty(Feature.TRACE_EXTERNAL_FUNCTIONS)
//    val err = env.getConfiguration.getLogger
    libraryList.iterator.asScala.map(_.bind(functionName, staticArgs, env, reasons)).find(_ ne null).orNull
  }

  def getDeclaration(
    functionName : StructuredQName,
    staticArgs   : Int
  ): XQueryFunction = {
    for (lib <- libraryList.asScala if lib.isInstanceOf[XQueryFunctionBinder]) {
      val func = lib.asInstanceOf[XQueryFunctionBinder].getDeclaration(functionName, staticArgs)
      if (func != null)
        return func
    }
    null
  }

  def getLibraryList: List[FunctionLibrary] = libraryList

  def copy(): FunctionLibrary = {
    val fll = new FunctionLibraryList
    fll.libraryList = new ArrayList(libraryList.size)
    for (i <- 0 until libraryList.size)
      fll.libraryList.add(libraryList.get(i).copy())
    fll
  }
}
