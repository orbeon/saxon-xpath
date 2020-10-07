package org.orbeon.saxon.s9apir

import org.orbeon.saxon.expr.XPathContext
import org.orbeon.saxon.expr.parser.Loc
import org.orbeon.saxon.expr.parser.RoleDiagnostic
import org.orbeon.saxon.model.FunctionItemType
import org.orbeon.saxon.model.TypeHierarchy
import org.orbeon.saxon.om.Function
import org.orbeon.saxon.om.Sequence
import org.orbeon.saxon.om.StructuredQName
import org.orbeon.saxon.s9api.streams.Step
import org.orbeon.saxon.trans.XPathException
import java.util.stream.Stream

import org.orbeon.saxon.s9api.{Processor, QName, SaxonApiException, XdmItem, XdmValue}
import org.orbeon.saxon.utils.{Configuration, Controller}


object XdmFunctionItem {

  def getSystemFunction(processor: Processor,
                        name: QName,
                        arity: Int): XdmFunctionItem = {
    val config: Configuration = processor.getUnderlyingConfiguration
    val f: Function = config.getSystemFunction(name.getStructuredQName, arity)
    if (f == null) null else new XdmFunctionItem(f)
  }

}

class XdmFunctionItem extends XdmItem {

  def this(fi: Function) = {
    this()
    this.setValue(fi)
  }

  def getName: QName = {
    val fi: Function = getUnderlyingValue.asInstanceOf[Function]
    val sq: StructuredQName = fi.getFunctionName
    if (sq == null) null else new QName(sq)
  }

  def getArity: Int = {
    val fi: Function = getUnderlyingValue.asInstanceOf[Function]
    fi.getArity
  }

  override def isAtomicValue(): Boolean = false

  def asFunction(processor: Processor): java.util.function.Function[_ >: XdmValue, _ <: XdmValue] =
    if (getArity == 1) { (arg) =>
      XdmFunctionItem.this.call(processor, arg)
    } else {
      throw new IllegalStateException("Function arity must be one")
    }

  def asStep(processor: Processor): Step[XdmItem] =
    if (getArity == 1) {
      new Step[XdmItem]() {
        def apply(arg: XdmItem): Stream[_ <: XdmItem] =
          XdmFunctionItem.this.call(processor, arg).stream()
      }
    } else {
      throw new IllegalStateException("Function arity must be one")
    }

  def call(processor: Processor, arguments: XdmValue*): XdmValue = {
    if (arguments.length != getArity) {
      throw new SaxonApiException(
        "Supplied " + arguments.length + " arguments, required " +
          getArity)
    }
    val fi: Function = getUnderlyingValue.asInstanceOf[Function]
    val `type`: FunctionItemType = fi.getFunctionItemType
    val argVals: Array[Sequence] = Array.ofDim[Sequence](arguments.length)
    val th: TypeHierarchy =
      processor.getUnderlyingConfiguration.getTypeHierarchy
    for (i <- 0 until arguments.length) {
      val required: org.orbeon.saxon.value.SequenceType =
        `type`.getArgumentTypes(i)
      var `val`: Sequence = arguments(i).getUnderlyingValue
      if (!required.matches(`val`, th)) {
        val role: RoleDiagnostic =
          new RoleDiagnostic(RoleDiagnostic.FUNCTION, "", i)
        `val` =
          th.applyFunctionConversionRules(`val`, required, role, Loc.NONE)
      }
      argVals(i) = `val`
    }
    val config: Configuration = processor.getUnderlyingConfiguration
    val controller: Controller = new Controller(config)
    var context: XPathContext = controller.newXPathContext
    context = fi.makeNewContext(context, controller)
    var result: Sequence = fi.call(context, argVals)
    if (!fi.isTrustedResultType) {
      result = result.materialize()
      val required: org.orbeon.saxon.value.SequenceType = `type`.getResultType
      if (!required.matches(result, th)) {
        val role: RoleDiagnostic =
          new RoleDiagnostic(RoleDiagnostic.FUNCTION_RESULT, "", 0)
        result = th.applyFunctionConversionRules(result.materialize(),
          required,
          role,
          Loc.NONE)
      }
    }
    val se: Sequence = result.iterate().materialize()
    XdmValue.wrap(se)
  }

}