package net.sf.saxon.functions.registry

import net.sf.saxon.expr.Expression

import net.sf.saxon.expr.OperandUsage

import net.sf.saxon.expr.StaticContext

import net.sf.saxon.expr.StaticProperty

import net.sf.saxon.expr.parser.RetainedStaticContext

import net.sf.saxon.functions.FunctionLibrary

import net.sf.saxon.functions.OptionsParameter

import net.sf.saxon.functions.SystemFunction

import net.sf.saxon.lib.NamespaceConstant

import net.sf.saxon.model.ItemType

import net.sf.saxon.model.PlainType

import net.sf.saxon.om.Function

import net.sf.saxon.om.Sequence

import net.sf.saxon.om.StructuredQName

import net.sf.saxon.trans.SymbolicName

import net.sf.saxon.trans.XPathException

import net.sf.saxon.value.AtomicValue

import net.sf.saxon.value.EmptySequence

import net.sf.saxon.value.SequenceType

import java.util.HashMap

import java.util.List

import BuiltInFunctionSet._

object BuiltInFunctionSet {

  var EMPTY: Sequence = EmptySequence.getInstance

  val ONE: Int = StaticProperty.ALLOWS_ONE

  val OPT: Int = StaticProperty.ALLOWS_ZERO_OR_ONE

  val STAR: Int = StaticProperty.ALLOWS_ZERO_OR_MORE

  val PLUS: Int = StaticProperty.ALLOWS_ONE_OR_MORE

  val AS_ARG0: Int = 1

  val AS_PRIM_ARG0: Int = 2

  val CITEM: Int = 4

  val BASE: Int = 8

  val NS: Int = 16

  val DCOLL: Int = 32

  val DLANG: Int = 64

  val FILTER: Int = 256

  val LATE: Int = 512

  val UO: Int = 1024

  val POSN: Int = 1024 * 2

  val LAST: Int = 1024 * 4

  val SIDE: Int = 1024 * 8

  val CDOC: Int = 1024 * 16

  val CARD0: Int = 1024 * 32

  val NEW: Int = 1024 * 64

  val DEPENDS_ON_STATIC_CONTEXT: Int = BASE | NS | DCOLL

  val FOCUS: Int = CITEM | POSN | LAST | CDOC

  val INS: Int = 1 << 24

  val ABS: Int = 1 << 25

  val TRA: Int = 1 << 26

  val NAV: Int = 1 << 27

  private def pluralArguments(num: Int): String = {
    if (num == 0) {
      return "zero arguments"
    }
    if (num == 1) {
      return "one argument"
    }
    s"$num arguments"
  }

  class Entry {

    var name: StructuredQName = _

    var implementationClass: Class[_] = _

    var arity: Int = _

    var itemType: ItemType = _

    var cardinality: Int = _

    var usage: Array[OperandUsage.OperandUsage] = _

    var argumentTypes: Array[SequenceType] = _

    var resultIfEmpty: Array[Sequence] = Array()

    var properties: Int = _

    var optionDetails: OptionsParameter = _

    def arg(a: Int, `type`: ItemType, options: Int, resultIfEmpty: Sequence): Entry = {
      val cardinality: Int = options & StaticProperty.CARDINALITY_MASK
      var usage: OperandUsage.OperandUsage = OperandUsage.NAVIGATION
      if ((options & ABS) != 0) {
        usage = OperandUsage.ABSORPTION
      } else if ((options & TRA) != 0) {
        usage = OperandUsage.TRANSMISSION
      } else if ((options & INS) != 0) {
        usage = OperandUsage.INSPECTION
      } else if (`type`.isInstanceOf[PlainType]) {
        usage = OperandUsage.ABSORPTION
      }
      try {
        this.argumentTypes(a) =
          SequenceType.makeSequenceType(`type`, cardinality)
        this.resultIfEmpty(a) = resultIfEmpty
        this.usage(a) = usage
      } catch {
        case err: ArrayIndexOutOfBoundsException =>
          System.err.println(
            "Internal Saxon error: Can't set argument " + a + " of " +
              name)

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

  private var functionTable: HashMap[String, Entry] = new HashMap(200)

  def importFunctionSet(importee: BuiltInFunctionSet): Unit = {
    if (importee.getNamespace != getNamespace) {
      throw new IllegalArgumentException(importee.getNamespace)
    }
    functionTable.putAll(importee.functionTable)
  }

  def getFunctionDetails(name: String, arity: Int): Entry = {
    if (arity == -1) {
      for (i <- 0.until(20)) {
        val found: Entry = getFunctionDetails(name, i)
        if (found != null) {
          found
        }
      }
      return null
    }
    var key: String = name + "#" + arity
    var entry: Entry = functionTable.get(key)
    if (entry != null) {
      return entry
    }
    if (name.==("concat") && arity >= 2 && getNamespace == NamespaceConstant.FN) {
      key = "concat#-1"
      entry = functionTable.get(key)
      return entry
    }
    null
  }

  override def bind(symbolicName: SymbolicName.F,
                    staticArgs: Array[Expression],
                    env: StaticContext,
                    reasons: List[String]): Expression = {
    val functionName: StructuredQName = symbolicName.getComponentName
    val arity: Int = symbolicName.getArity
    val localName: String = functionName.getLocalPart
    if (functionName.hasURI(getNamespace) && getFunctionDetails(
      localName,
      arity) != null) {
      val rsc: RetainedStaticContext = new RetainedStaticContext(env)
      try {
        val fn: SystemFunction = makeFunction(localName, arity)
        fn.setRetainedStaticContext(rsc)
        val f: Expression = fn.makeFunctionCall(staticArgs.toIndexedSeq: _*)
        f.setRetainedStaticContext(rsc)
        f
      } catch {
        case e: XPathException => {
          reasons.add(e.getMessage)
          null
        }

      }
    } else {
      null
    }
  }

  def makeFunction(name: String, arity: Int): SystemFunction = {
    val entry: Entry = getFunctionDetails(name, arity)
    if (entry == null) {
      val diagName: String =
        if (getNamespace == NamespaceConstant.FN) "System function " + name
        else "Function Q{" + getNamespace + "}" + name
      if (getFunctionDetails(name, -1) == null) {
        val err: XPathException = new XPathException(
          diagName +
            "() does not exist or is not available in this environment")
        err.setErrorCode("XPST0017")
        err.setIsStaticError(true)
        throw err
      } else {
        val err: XPathException = new XPathException(
          diagName + "() cannot be called with " + pluralArguments(arity))
        err.setErrorCode("XPST0017")
        err.setIsStaticError(true)
        throw err
      }
    }
    val functionClass: Class[_] = entry.implementationClass
    var f: SystemFunction = null
    try f = functionClass.newInstance().asInstanceOf[SystemFunction]
    catch {
      case err: Exception => {
        err.printStackTrace()
        throw new AssertionError(
          "Failed to instantiate system function " + name + " - " +
            err.getMessage)
      }

    }
    f.setDetails(entry)
    f.setArity(arity)
    f
  }

  override def isAvailable(symbolicName: SymbolicName.F): Boolean = {
    val qn: StructuredQName = symbolicName.getComponentName
    qn.hasURI(getNamespace) &&
      getFunctionDetails(qn.getLocalPart, symbolicName.getArity) !=
        null
  }

  override def copy(): FunctionLibrary = this

  override def getFunctionItem(symbolicName: SymbolicName.F,
                               staticContext: StaticContext): Function = {
    val functionName: StructuredQName = symbolicName.getComponentName
    val arity: Int = symbolicName.getArity
    if (functionName.hasURI(getNamespace) &&
      getFunctionDetails(functionName.getLocalPart, arity) !=
        null) {
      val rsc: RetainedStaticContext =
        staticContext.makeRetainedStaticContext()
      val fn: SystemFunction = makeFunction(functionName.getLocalPart, arity)
      fn.setRetainedStaticContext(rsc)
      fn
    } else {
      null
    }
  }

  def register(name: String,
               arity: Int,
               implementationClass: Class[_ <: SystemFunction],
               itemType: ItemType,
               cardinality: Int,
               properties: Int): Entry = {
    val e: Entry = new Entry()
    e.name = new StructuredQName(getConventionalPrefix, getNamespace, name)
    e.arity = arity
    e.implementationClass = implementationClass
    e.itemType = itemType
    e.cardinality = cardinality
    e.properties = properties
    if (e.arity == -1) {
      e.argumentTypes = Array.ofDim[SequenceType](1)
      e.resultIfEmpty = Array.ofDim[AtomicValue](1).asInstanceOf[Array[Sequence]]
      e.usage = Array.ofDim[OperandUsage.OperandUsage](1)
    } else {
      e.argumentTypes = Array.ofDim[SequenceType](arity)
      e.resultIfEmpty = Array.ofDim[Sequence](arity)
      e.usage = Array.ofDim[OperandUsage.OperandUsage](arity)
    }
    functionTable.put(name + "#" + arity, e)
    e
  }

  def registerReducedArityVariants(key: String,
                                   min: Int,
                                   max: Int): Unit = {
    val master: Entry = functionTable.get(key)
    var arity: Int = min
    while (arity <= max) {
      val e: Entry = new Entry()
      e.name = master.name
      e.arity = arity
      e.implementationClass = master.implementationClass
      e.itemType = master.itemType
      e.cardinality = master.cardinality
      e.properties = master.properties
      e.argumentTypes = Array.ofDim[SequenceType](arity)
      e.resultIfEmpty =
        Array.ofDim[Sequence](arity)
      e.usage = Array.ofDim[OperandUsage.OperandUsage](arity)
      for (i <- 0 until arity) {
        e.argumentTypes(i) = master.argumentTypes(i)
        e.resultIfEmpty(i) = master.resultIfEmpty(i)
        e.usage(i) = master.usage(i)
      }
      functionTable.put(e.name.getLocalPart + "#" + arity, e)
      arity += 1
    }
  }

  def getNamespace(): String = NamespaceConstant.FN

  def getConventionalPrefix(): String = "fn"

}
