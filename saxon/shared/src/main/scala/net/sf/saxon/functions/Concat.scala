package net.sf.saxon.functions

import java.util.Arrays

import net.sf.saxon.event.Outputter
import net.sf.saxon.expr._
import net.sf.saxon.expr.oper.OperandArray
import net.sf.saxon.expr.parser.{ContextItemStaticInfo, ExpressionVisitor}
import net.sf.saxon.model.{BuiltInAtomicType, FunctionItemType, SpecificFunctionType}
import net.sf.saxon.om.{Item, Sequence}
import net.sf.saxon.tree.util.{CharSequenceConsumer, FastStringBuffer}
import net.sf.saxon.value.{SequenceType, StringValue}

import scala.jdk.CollectionConverters._

class Concat extends SystemFunction with PushableFunction {

   override def resultIfEmpty(arg: Int): Sequence = null

  override def getOperandRoles(): Array[OperandRole] = {
    val roles: Array[OperandRole] = Array.ofDim[OperandRole](getArity)
    val operandRole: OperandRole = new OperandRole(0, OperandUsage.ABSORPTION)
    for (i <- 0 until getArity) {
      roles(i) = operandRole
    }
    roles
  }

  override def getFunctionItemType: FunctionItemType = {
    val argTypes: Array[SequenceType] = Array.ofDim[SequenceType](getArity)
    Arrays.fill(argTypes.asInstanceOf[Array[AnyRef]], SequenceType.OPTIONAL_ATOMIC.asInstanceOf[AnyRef])
    new SpecificFunctionType(argTypes, SequenceType.SINGLE_STRING)
  }

  override def makeOptimizedFunctionCall(
                                 visitor: ExpressionVisitor,
                                 contextInfo: ContextItemStaticInfo,
                                 arguments: Expression*): Expression = {
    if (OperandArray.every(arguments.toArray,
      (arg: Expression) =>
        arg.getCardinality == StaticProperty.EXACTLY_ONE && arg.getItemType == BuiltInAtomicType.BOOLEAN)) {
      visitor.getStaticContext.issueWarning(
        "Did you intend to apply string concatenation to boolean operands? " +
          "Perhaps you intended 'or' rather than '||'. " +
          "To suppress this warning, use string() on the arguments.",
        arguments(0).getLocation
      )
    }
    new SystemFunctionCall.Optimized(this, arguments.toArray) {
      override def evaluateAsString(context: XPathContext): CharSequence = {
        val buffer: FastStringBuffer = new FastStringBuffer(256)
        for (o <- operands.asScala) {
          val it: Item = o.getChildExpression.evaluateItem(context)
          if (it != null) {
            buffer.cat(it.getStringValueCS)
          }
        }
        buffer
      }

      override def evaluateItem(context: XPathContext): Item =
        new StringValue(evaluateAsString(context))
    }
  }

  private def isSingleBoolean(arg: Expression): Boolean =
    arg.getCardinality == StaticProperty.EXACTLY_ONE && arg.getItemType == BuiltInAtomicType.BOOLEAN

  def call(context: XPathContext, arguments: Array[Sequence]): StringValue = {
    val fsb: FastStringBuffer = new FastStringBuffer(FastStringBuffer.C64)
    for (arg <- arguments) {
      val item: Item = arg.head
      if (item != null) {
        fsb.cat(item.getStringValueCS)
      }
    }
    new StringValue(fsb)
  }

  override def process(destination: Outputter,
                       context: XPathContext,
                       arguments: Array[Sequence]): Unit = {
    val output: CharSequenceConsumer = destination.getStringReceiver(false)
    output.open()
    for (arg <- arguments) {
      val item: Item = arg.head
      if (item != null) {
        output.cat(item.getStringValueCS)
      }
    }
    output.close()
  }

  override def getRequiredType(arg: Int): SequenceType = getDetails().argumentTypes(0)

  override def getCompilerName(): String = "ConcatCompiler"

}
