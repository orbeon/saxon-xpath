package net.sf.saxon.expr

import net.sf.saxon.functions.AbstractFunction

import net.sf.saxon.lib.ConversionRules

import net.sf.saxon.model._

import net.sf.saxon.om._

import net.sf.saxon.trans.XPathException

import net.sf.saxon.value.AnyURIValue

import net.sf.saxon.value.AtomicValue

import net.sf.saxon.value.SequenceType

import net.sf.saxon.value.StringValue

import scala.jdk.CollectionConverters._

object UnionConstructorFunction {

  def cast(value: AtomicValue,
           targetType: UnionType,
           nsResolver: NamespaceResolver,
           rules: ConversionRules): AtomicSequence = {
    if (value == null) {
      throw new NullPointerException()
    }
    if (value
      .isInstanceOf[StringValue] && !(value.isInstanceOf[AnyURIValue])) {
      try targetType.getTypedValue(value.getStringValueCS, nsResolver, rules)
      catch {
        case e: ValidationException => {
          e.setErrorCode("FORG0001")
          throw e
        }

      }
    }
    val label: AtomicType = value.getItemType
    val memberTypes: Iterable[_ <: PlainType] =
      targetType.getPlainMemberTypes
    for (member <- memberTypes if label == member) {
      value
    }
    for (member <- memberTypes) {
      var t: AtomicType = label
      while (t != null) if (t == member) {
        value
      } else {
        t =
          if (t.getBaseType.isInstanceOf[AtomicType])
            t.getBaseType.asInstanceOf[AtomicType]
          else null
      }
    }
    for (memType <- memberTypes if memType.isInstanceOf[AtomicType]) {
      val c: Converter =
        rules.getConverter(value.getItemType, memType.asInstanceOf[AtomicType])
      if (c != null) {
        val result: ConversionResult = c.convert(value)
        if (result.isInstanceOf[AtomicValue]) {
          result.asInstanceOf[AtomicValue]
        }
      }
    }
    throw new XPathException(
      "Cannot convert the supplied value to " + targetType.getDescription,
      "FORG0001")
  }

}

class UnionConstructorFunction( var targetType: UnionType,
                                var resolver: NamespaceResolver,
                                var allowEmpty: Boolean)
  extends AbstractFunction {

   def getOperandRole: OperandRole = OperandRole.SINGLE_ATOMIC

  def isAllowEmpty: Boolean = allowEmpty

  def getTargetType: UnionType = targetType

  def getNamespaceResolver: NamespaceResolver = resolver

  def getFunctionItemType: FunctionItemType = {
    val resultType: SequenceType = targetType.getResultTypeOfCast
    val argType: SequenceType =
      if (allowEmpty) SequenceType.OPTIONAL_ATOMIC
      else SequenceType.SINGLE_ATOMIC
    new SpecificFunctionType(Array(argType), resultType)
  }

  def getFunctionName: StructuredQName = targetType.getTypeName

  def getDescription: String = getFunctionName.getDisplayName

  def getArity: Int = 1

  def cast(value: AtomicValue, context: XPathContext): AtomicSequence = {
    val rules: ConversionRules = context.getConfiguration.getConversionRules
    if (value == null) {
      throw new NullPointerException()
    }
    if (value
      .isInstanceOf[StringValue] && !(value.isInstanceOf[AnyURIValue])) {
      try targetType.getTypedValue(value.getStringValueCS, resolver, rules)
      catch {
        case e: ValidationException => {
          e.setErrorCode("FORG0001")
          throw e
        }

      }
    }
    val label: AtomicType = value.getItemType
    val memberTypes: Iterable[_ <: PlainType] =
      targetType.asInstanceOf[UnionType].getPlainMemberTypes
    if (targetType.asInstanceOf[UnionType].isPlainType) {
      for (member <- memberTypes if label == member) {
        value
      }
      for (member <- memberTypes) {
        var t: AtomicType = label
        while (t != null) if (t == member) {
          value
        } else {
          t =
            if (t.getBaseType.isInstanceOf[AtomicType])
              t.getBaseType.asInstanceOf[AtomicType]
            else null
        }
      }
    }
    for (memType <- memberTypes if memType.isInstanceOf[AtomicType]) {
      val c: Converter =
        rules.getConverter(value.getItemType, memType.asInstanceOf[AtomicType])
      if (c != null) {
        val result: ConversionResult = c.convert(value)
        if (result.isInstanceOf[AtomicValue]) {
          if (!targetType.isPlainType) {
            val vf: ValidationFailure = targetType.checkAgainstFacets(
              result.asInstanceOf[AtomicValue],
              rules)
            if (vf == null) {
              result.asInstanceOf[AtomicValue]
            }
          } else {
            result.asInstanceOf[AtomicValue]
          }
        }
      }
    }
    throw new XPathException(
      "Cannot convert the supplied value to " + targetType.getDescription,
      "FORG0001")
  }

  def call(context: XPathContext, args: Array[Sequence]): AtomicSequence = {
    val `val`: AtomicValue = args(0).head.asInstanceOf[AtomicValue]
    if (`val` == null) {
      if (allowEmpty) {
        EmptyAtomicSequence.getInstance
      } else {
        val e: XPathException = new XPathException(
          "Cast expression does not allow an empty sequence to be supplied",
          "XPTY0004")
        e.setIsTypeError(true)
        throw e
      }
    }
    cast(`val`, context)
  }

}
