package org.orbeon.saxon.functions.registry

import java.util.{HashMap, List}

import org.orbeon.saxon.expr.{Expression, OperandUsage, StaticContext, StaticProperty}
import org.orbeon.saxon.expr.parser.RetainedStaticContext
import org.orbeon.saxon.functions.{FunctionLibrary, OptionsParameter, SystemFunction}
import org.orbeon.saxon.functions.registry.BuiltInFunctionSet._
import org.orbeon.saxon.lib.NamespaceConstant
import org.orbeon.saxon.model.{ItemType, PlainType}
import org.orbeon.saxon.om.{Function, Sequence, StructuredQName}
import org.orbeon.saxon.trans.{SymbolicName, XPathException}
import org.orbeon.saxon.value.{AtomicValue, EmptySequence, SequenceType}


object BuiltInFunctionSet {

  val EMPTY                    : Sequence = EmptySequence.getInstance
  val ONE                      : Int      = StaticProperty.ALLOWS_ONE
  val OPT                      : Int      = StaticProperty.ALLOWS_ZERO_OR_ONE
  val STAR                     : Int      = StaticProperty.ALLOWS_ZERO_OR_MORE
  val PLUS                     : Int      = StaticProperty.ALLOWS_ONE_OR_MORE
  val AS_ARG0                  : Int      = 1
  val AS_PRIM_ARG0             : Int      = 2
  val CITEM                    : Int      = 4
  val BASE                     : Int      = 8
  val NS                       : Int      = 16
  val DCOLL                    : Int      = 32
  val DLANG                    : Int      = 64
  val FILTER                   : Int      = 256
  val LATE                     : Int      = 512
  val UO                       : Int      = 1024
  val POSN                     : Int      = 1024 * 2
  val LAST                     : Int      = 1024 * 4
  val SIDE                     : Int      = 1024 * 8
  val CDOC                     : Int      = 1024 * 16
  val CARD0                    : Int      = 1024 * 32
  val NEW                      : Int      = 1024 * 64
  val DEPENDS_ON_STATIC_CONTEXT: Int      = BASE | NS | DCOLL
  val FOCUS                    : Int      = CITEM | POSN | LAST | CDOC
  val INS                      : Int      = 1 << 24
  val ABS                      : Int      = 1 << 25
  val TRA                      : Int      = 1 << 26
  val NAV                      : Int      = 1 << 27

  private def pluralArguments(num: Int): String =
    if (num == 0)
      "zero arguments"
    else if (num == 1)
      "one argument"
    else
      s"$num arguments"

  class Entry {

    var name         : StructuredQName                  = _
    var make         : () => SystemFunction             = _
    var arity        : Int                              = _
    var itemType     : ItemType                         = _
    var cardinality  : Int                              = _
    var usage        : Array[OperandUsage.OperandUsage] = _
    var argumentTypes: Array[SequenceType]              = _
    var resultIfEmpty: Array[Sequence]                  = Array()
    var properties   : Int                              = _
    var optionDetails: OptionsParameter                 = _

    def arg(a: Int, `type`: ItemType, options: Int, resultIfEmpty: Sequence): Entry = {
      val cardinality = options & StaticProperty.CARDINALITY_MASK
      var usage       = OperandUsage.NAVIGATION
      if ((options & ABS) != 0)
        usage = OperandUsage.ABSORPTION
      else if ((options & TRA) != 0)
        usage = OperandUsage.TRANSMISSION
      else if ((options & INS) != 0)
        usage = OperandUsage.INSPECTION
      else if (`type`.isInstanceOf[PlainType])
        usage = OperandUsage.ABSORPTION
      try {
        this.argumentTypes(a) = SequenceType.makeSequenceType(`type`, cardinality)
        this.resultIfEmpty(a) = resultIfEmpty
        this.usage(a) = usage
      } catch {
        case _: ArrayIndexOutOfBoundsException =>
          System.err.println("Internal Saxon error: Can't set argument " + a + " of " + name)
      }
      this
    }

    def optionDetails(details: OptionsParameter): Entry = {
      this.optionDetails = details
      this
    }
  }

}

abstract class BuiltInFunctionSet extends FunctionLibrary {

  private val functionTable: HashMap[String, Entry] = new HashMap(200)

  def importFunctionSet(importee: BuiltInFunctionSet): Unit = {
    if (importee.getNamespace != getNamespace)
      throw new IllegalArgumentException(importee.getNamespace)
    functionTable.putAll(importee.functionTable)
  }

  def getFunctionDetails(name: String, arity: Int): Entry = {
    if (arity == -1) {
      for (i <- 0.until(20)) {
        val found = getFunctionDetails(name, i)
        if (found != null)
          return found
      }
      return null
    }
    var key   = name + "#" + arity
    var entry = functionTable.get(key)
    if (entry ne null)
      return entry

    if (name == "concat" && arity >= 2 && getNamespace == NamespaceConstant.FN) {
      key = "concat#-1"
      entry = functionTable.get(key)
      return entry
    }
    null
  }

  override def bind(
    symbolicName: SymbolicName.F,
    staticArgs  : Array[Expression],
    env         : StaticContext,
    reasons     : List[String]
  ): Expression = {
    val functionName = symbolicName.getComponentName
    val arity        = symbolicName.getArity
    val localName    = functionName.getLocalPart

    if (functionName.hasURI(getNamespace) && getFunctionDetails(localName, arity) != null) {
      val rsc = new RetainedStaticContext(env)
      try {
        val fn = makeFunction(localName, arity)
        fn.setRetainedStaticContext(rsc)
        val f = fn.makeFunctionCall(staticArgs.toIndexedSeq: _*)
        f.setRetainedStaticContext(rsc)
        f
      } catch {
        case e: XPathException =>
          reasons.add(e.getMessage)
          null
      }
    } else {
      null
    }
  }

  def makeFunction(name: String, arity: Int): SystemFunction = {
    val entry = getFunctionDetails(name, arity)
    if (entry == null) {
      val diagName =
        if (getNamespace == NamespaceConstant.FN)
          "System function " + name
        else
          "Function Q{" + getNamespace + "}" + name
      if (getFunctionDetails(name, -1) == null) {
        val err = new XPathException(diagName + "() does not exist or is not available in this environment")
        err.setErrorCode("XPST0017")
        err.setIsStaticError(true)
        throw err
      } else {
        val err = new XPathException(diagName + "() cannot be called with " + pluralArguments(arity))
        err.setErrorCode("XPST0017")
        err.setIsStaticError(true)
        throw err
      }
    }

    // ORBEON: No reflection.
    //    val functionClass: Class[_] = entry.implementationClass
    //    val f =
    //      try functionClass.newInstance().asInstanceOf[SystemFunction]
    //      catch {
    //        case err: Exception =>
    //          err.printStackTrace()
    //          throw new AssertionError("Failed to instantiate system function " + name + " - " + err.getMessage)
    //      }
    val f = entry.make()
    f.setDetails(entry)
    f.setArity(arity)
    f
  }

  override def isAvailable(symbolicName: SymbolicName.F): Boolean = {
    val qn = symbolicName.getComponentName
    qn.hasURI(getNamespace) &&
      getFunctionDetails(qn.getLocalPart, symbolicName.getArity) != null
  }

  override def copy(): FunctionLibrary = this

  override def getFunctionItem(symbolicName: SymbolicName.F, staticContext: StaticContext): Function = {

    val functionName = symbolicName.getComponentName
    val arity        = symbolicName.getArity

    if (functionName.hasURI(getNamespace) && getFunctionDetails(functionName.getLocalPart, arity) != null) {
      val rsc = staticContext.makeRetainedStaticContext()
      val fn  = makeFunction(functionName.getLocalPart, arity)
      fn.setRetainedStaticContext(rsc)
      fn
    } else {
      null
    }
  }

  def register(
    name       : String,
    arity      : Int,
    make       : () => SystemFunction,
    itemType   : ItemType,
    cardinality: Int,
    properties : Int
  ): Entry = {
    val e = new Entry
    e.name = new StructuredQName(getConventionalPrefix, getNamespace, name)
    e.arity = arity
    e.make = make
    e.itemType = itemType
    e.cardinality = cardinality
    e.properties = properties
    if (e.arity == -1) {
      e.argumentTypes = Array.ofDim[SequenceType](1)
      e.resultIfEmpty = Array.ofDim[AtomicValue](1).asInstanceOf[Array[Sequence]]
      e.usage         = Array.ofDim[OperandUsage.OperandUsage](1)
    } else {
      e.argumentTypes = Array.ofDim[SequenceType](arity)
      e.resultIfEmpty = Array.ofDim[Sequence](arity)
      e.usage         = Array.ofDim[OperandUsage.OperandUsage](arity)
    }
    functionTable.put(name + "#" + arity, e)
    e
  }

  def registerReducedArityVariants(
    key: String,
    min: Int,
    max: Int
  ): Unit = {
    val master = functionTable.get(key)
    var arity  = min
    while (arity <= max) {
      val e = new Entry
      e.name          = master.name
      e.arity         = arity
      e.make          = master.make
      e.itemType      = master.itemType
      e.cardinality   = master.cardinality
      e.properties    = master.properties
      e.argumentTypes = Array.ofDim[SequenceType](arity)
      e.resultIfEmpty = Array.ofDim[Sequence](arity)
      e.usage         = Array.ofDim[OperandUsage.OperandUsage](arity)
      for (i <- 0 until arity) {
        e.argumentTypes(i) = master.argumentTypes(i)
        e.resultIfEmpty(i) = master.resultIfEmpty(i)
        e.usage(i) = master.usage(i)
      }
      functionTable.put(e.name.getLocalPart + "#" + arity, e)
      arity += 1
    }
  }

  def getNamespace: String = NamespaceConstant.FN

  def getConventionalPrefix: String = "fn"
}
