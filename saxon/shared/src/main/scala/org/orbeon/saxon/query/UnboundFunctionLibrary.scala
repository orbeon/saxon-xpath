package org.orbeon.saxon.query

import org.orbeon.saxon.expr.Expression
import org.orbeon.saxon.expr.StaticContext
import org.orbeon.saxon.expr.UserFunctionCall
import org.orbeon.saxon.expr.UserFunctionResolvable
import org.orbeon.saxon.expr.parser.XPathParser
import org.orbeon.saxon.functions.CallableFunction
import org.orbeon.saxon.functions.FunctionLibrary
import org.orbeon.saxon.lib.Feature
import org.orbeon.saxon.model.AnyFunctionType
import org.orbeon.saxon.om.Function
import org.orbeon.saxon.om.StructuredQName
import org.orbeon.saxon.trans.SymbolicName
import org.orbeon.saxon.trans.XPathException
import java.util.ArrayList
import java.util.List

import org.orbeon.saxon.utils.Configuration

//import scala.collection.compat._
import scala.jdk.CollectionConverters._

class UnboundFunctionLibrary extends FunctionLibrary {

  private var unboundFunctionReferences: List[UserFunctionResolvable] =
    new ArrayList(20)

  private var correspondingStaticContext: List[StaticContext] = new ArrayList(
    20)

  private var correspondingReasons: List[List[String]] = new ArrayList()

  private var resolving: Boolean = false

  def bind(functionName: SymbolicName.F,
           arguments: Array[Expression],
           env: StaticContext,
           reasons: List[String]): Expression = {
    if (resolving) {
      return null
    }
    if (!reasons.isEmpty &&
      reasons.get(0).startsWith("Cannot call the private XQuery function")) {
      return null
    }
    val ufc: UserFunctionCall = new UserFunctionCall()
    ufc.setFunctionName(functionName.getComponentName)
    ufc.setArguments(arguments)
    unboundFunctionReferences.add(ufc)
    correspondingStaticContext.add(env)
    correspondingReasons.add(reasons)
    ufc
  }

  def getFunctionItem(functionName: SymbolicName.F,
                      staticContext: StaticContext): Function = {
    if (resolving) {
      return null
    }
    val uc: XQueryFunctionLibrary.UnresolvedCallable =
      new XQueryFunctionLibrary.UnresolvedCallable(functionName)
    unboundFunctionReferences.add(uc)
    correspondingStaticContext.add(null)
    correspondingReasons.add(new ArrayList())
    val fi: CallableFunction =
      new CallableFunction(functionName, uc, AnyFunctionType)
    fi
  }

  def isAvailable(functionName: SymbolicName.F): Boolean = false

  def bindUnboundFunctionReferences(lib: XQueryFunctionBinder,
                                    config: Configuration): Unit = {
    resolving = true
    for (i <- 0 until unboundFunctionReferences.size) {
      val ref: UserFunctionResolvable = unboundFunctionReferences.get(i)
      if (ref.isInstanceOf[UserFunctionCall]) {
        val ufc: UserFunctionCall = ref.asInstanceOf[UserFunctionCall]
        val importingModule: QueryModule =
          correspondingStaticContext.get(i).asInstanceOf[QueryModule]
        if (importingModule == null) {
          //continue
        }
        correspondingStaticContext.set(i, null)
        val q: StructuredQName = ufc.getFunctionName
        val arity: Int = ufc.getArity
        val fd: XQueryFunction = lib.getDeclaration(q, arity)
        if (fd != null) {
          fd.registerReference(ufc)
          ufc.setStaticType(fd.getResultType)
        } else {
          val sb: StringBuilder = new StringBuilder(
            "Cannot find a " + arity + "-argument function named " +
              q.getEQName +
              "()")
          val reasons: List[String] = correspondingReasons.get(i)
          for (reason <- reasons.asScala) {
            sb.append(". ").append(reason)
          }
          if (reasons.isEmpty) {
            val supplementary: String =
              XPathParser.getMissingFunctionExplanation(q, config)
            if (supplementary != null) {
              sb.append(". ").append(supplementary)
            }
          }
          val err =
            new XPathException(sb.toString, "XPST0017", ufc.getLocation)
          err.setIsStaticError(true)
          throw err
        }
      } else if (ref.isInstanceOf[XQueryFunctionLibrary.UnresolvedCallable]) {
        val uc: XQueryFunctionLibrary.UnresolvedCallable =
          ref.asInstanceOf[XQueryFunctionLibrary.UnresolvedCallable]
        val q: StructuredQName = uc.getFunctionName
        val arity: Int = uc.getArity
        val fd: XQueryFunction = lib.getDeclaration(q, arity)
        if (fd != null) {
          fd.registerReference(uc)
        } else {
          var msg: String = "Cannot find a " + arity + "-argument function named " +
            q.getEQName +
            "()"
          if (!config.getBooleanProperty(Feature.ALLOW_EXTERNAL_FUNCTIONS)) {
            msg += ". Note: external function calls have been disabled"
          }
          val err = new XPathException(msg)
          err.setErrorCode("XPST0017")
          err.setIsStaticError(true)
          throw err
        }
      }
    }
  }

  def copy(): FunctionLibrary = {
    val qfl: UnboundFunctionLibrary = new UnboundFunctionLibrary()
    qfl.unboundFunctionReferences = new ArrayList(unboundFunctionReferences)
    qfl.correspondingStaticContext = new ArrayList(correspondingStaticContext)
    qfl.resolving = resolving
    qfl
  }

}
