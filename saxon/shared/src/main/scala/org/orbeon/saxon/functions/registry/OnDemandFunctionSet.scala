//////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
//// Copyright (c) 2018-2020 Saxonica Limited
//// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
//// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
//// This Source Code Form is "Incompatible With Secondary Licenses", as defined by the Mozilla Public License, v. 2.0.
//////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
///**
//  * The <tt>OnDemandFunctionSet</tt> represents a function library where the implementation classes
//  * are loaded dynamically on demand. The idea is that no failure should occur if implementations
//  * are not available unless the functions are actually required. The class contains the name of
//  * a real FunctionLibrary containing the function implementations; that FunctionLibrary is dynamically
//  * loaded if an attempt is made to bind a function name in the namespace registered with this
//  * class.
//  * <p>This mechanism is currently used for the SQL function library, because the implementations
//  * of these functions are shipped in a separate JAR file (and this is not available on .NET).
//  * If the target function library cannot be loaded, the relevant functions will be reported as
//  * being not available.</p>
//  */
//
//package org.orbeon.saxon.functions.registry
//
//import java.util.List
//
//import org.orbeon.saxon.expr.{Expression, StaticContext}
//import org.orbeon.saxon.functions.FunctionLibrary
//import org.orbeon.saxon.om.Function
//import org.orbeon.saxon.trans.{SymbolicName, XPathException}
//import org.orbeon.saxon.utils.Configuration
//
//
//class OnDemandFunctionSet(private var config: Configuration,
//                          private var namespace: String,
//                          private var libraryClass: String)
//    extends FunctionLibrary {
//
//  private var library: FunctionLibrary = _
//
//  private def load(functionName: SymbolicName.F,
//                   reasons: List[String]): Boolean =
//    if (functionName.getComponentName.hasURI(namespace)) {
//      if (library == null) {
//        try {
//          val lib: AnyRef =
//            config.getDynamicLoader.getInstance(libraryClass, null).asInstanceOf[AnyRef]
//          if (lib.isInstanceOf[FunctionLibrary]) {
//            library = lib.asInstanceOf[FunctionLibrary]
//          } else {
//            if (reasons != null) {
//              reasons.add(
//                "Class " + libraryClass + " was loaded but it is not a FunctionLibrary")
//            }
//            return false
//          }
//        } catch {
//          case e: XPathException => {
//            if (reasons != null) {
//              reasons.add(
//                "Failed to load class " + libraryClass + ": " + e.getMessage)
//            }
//            return false
//          }
//
//        }
//      }
//      library.setConfiguration(config)
//      true
//    } else {
//      false
//    }
//
//  override def isAvailable(functionName: SymbolicName.F): Boolean = {
//    val _match = load(functionName, null)
//    _match && library.isAvailable(functionName)
//  }
//
//  override def bind(functionName: SymbolicName.F,
//                    staticArgs: Array[Expression],
//                    env: StaticContext,
//                    reasons: List[String]): Expression = {
//    val `match`: Boolean = load(functionName, reasons)
//    if (`match`) {
//      library.bind(functionName, staticArgs, env, reasons)
//    } else {
//      null
//    }
//  }
//
//  override def copy(): FunctionLibrary = this
//
//  override def getFunctionItem(functionName: SymbolicName.F,
//                               staticContext: StaticContext): Function = {
//    val `match`: Boolean = load(functionName, null)
//    if (`match`) {
//      library.getFunctionItem(functionName, staticContext)
//    } else {
//      null
//    }
//  }
//
//}
