package org.orbeon.saxon.expr.parser

import org.orbeon.saxon.expr._
import org.orbeon.saxon.expr.parser.TypeChecker._
import org.orbeon.saxon.lib.ConversionRules
import org.orbeon.saxon.ma.map.{MapType, TupleType}
import org.orbeon.saxon.model.Affinity._
import org.orbeon.saxon.model._
import org.orbeon.saxon.om.{Item, Sequence, SequenceIterator, StandardNames}
import org.orbeon.saxon.pattern.NodeTest
import org.orbeon.saxon.trans.XPathException
import org.orbeon.saxon.utils.Configuration
import org.orbeon.saxon.value.{AtomicValue, Cardinality, SequenceType}

object TypeChecker {

  private def makeFunctionSequenceCoercer(exp: Expression,
                                          reqItemType: FunctionItemType,
                                          visitor: ExpressionVisitor,
                                          role: RoleDiagnostic): Expression =
    reqItemType.makeFunctionSequenceCoercer(exp, role)

  def strictTypeCheck(supplied: Expression,
                      req: SequenceType,
                      role: RoleDiagnostic,
                      env: StaticContext): Expression = {
    var exp = supplied
    val th = env.getConfiguration.getTypeHierarchy
    val reqItemType = req.getPrimaryType
    val reqCard = req.getCardinality
    var suppliedItemType: ItemType = null
    var suppliedCard = -1

    var cardOK = reqCard == StaticProperty.ALLOWS_ZERO_OR_MORE
    if (! cardOK) {
      suppliedCard = exp.getCardinality
      cardOK = Cardinality.subsumes(reqCard, suppliedCard)
    }
    var itemTypeOK = req.getPrimaryType eq AnyItemType
    if (! itemTypeOK) {
      suppliedItemType = exp.getItemType
      val relation = th.relationship(reqItemType, suppliedItemType)
      itemTypeOK = relation == SAME_TYPE || relation == SUBSUMES
    }

    if (itemTypeOK && cardOK)
      return exp

    if (suppliedCard == -1) {
      suppliedCard =
        if (suppliedItemType eq ErrorType)
          StaticProperty.EMPTY
        else
          exp.getCardinality
      if (! cardOK)
        cardOK = Cardinality.subsumes(reqCard, suppliedCard)
    }

    if (cardOK && suppliedCard == StaticProperty.EMPTY)
      return exp

    if (suppliedItemType == null)
      suppliedItemType = exp.getItemType

    if (suppliedCard == StaticProperty.EMPTY && ((reqCard & StaticProperty.ALLOWS_ZERO) == 0)) {
      val err = new XPathException(
        "An empty sequence is not allowed as the " + role.getMessage,
        role.getErrorCode,
        supplied.getLocation)
      err.setIsTypeError(role.isTypeError)
      throw err
    }
    val relation = th.relationship(suppliedItemType, reqItemType)
    if (relation == DISJOINT) {
      if (Cardinality.allowsZero(suppliedCard) && Cardinality.allowsZero(reqCard)) {
        if (suppliedCard != StaticProperty.EMPTY) {
          val msg = "Required item type of " + role.getMessage + " is " +
            reqItemType +
            "; supplied value (" +
            supplied.toShortString +
            ") has item type " +
            suppliedItemType +
            ". The expression can succeed only if the supplied value is an empty sequence."
          env.issueWarning(msg, supplied.getLocation)
        }
      } else {
        val msg = role.composeErrorMessage(reqItemType, supplied, th)
        val err =
          new XPathException(msg, role.getErrorCode, supplied.getLocation)
        err.setIsTypeError(role.isTypeError)
        throw err
      }
    }
    if (! (relation == SAME_TYPE || relation == SUBSUMED_BY)) {
      val cexp = new ItemChecker(exp, reqItemType, role)
      cexp.adoptChildExpression(exp)
      exp = cexp
    }
    if (! cardOK) {
      if (exp.isInstanceOf[Literal]) {
        val err = new XPathException(
          "Required cardinality of " + role.getMessage + " is " +
            Cardinality.toString(reqCard) +
            "; supplied value has cardinality " +
            Cardinality.toString(suppliedCard),
          role.getErrorCode,
          supplied.getLocation
        )
        err.setIsTypeError(role.isTypeError)
        throw err
      } else {
        val cexp = CardinalityChecker.makeCardinalityChecker(exp, reqCard, role)
        cexp.adoptChildExpression(exp)
        exp = cexp
      }
    }
    exp
  }

  def testConformance(`val`: Sequence,
                      requiredType: SequenceType,
                      context: XPathContext): XPathException = {
    val reqItemType: ItemType = requiredType.getPrimaryType
    val iter: SequenceIterator = `val`.iterate()
    var count: Int = 0
    var item: Item = null
    while ({
      item = iter.next()
      item
    } != null) {
      { count += 1;}
      if (! reqItemType.matches(item,
        context.getConfiguration.getTypeHierarchy)) {
        val err = new XPathException(
          "Required type is " + reqItemType + "; supplied value has type " +
            UType.getUType(`val`.materialize))
        err.setIsTypeError(true)
        err.setErrorCode("XPTY0004")
        err
      }
    }
    val reqCardinality: Int = requiredType.getCardinality
    if (count == 0 && !Cardinality.allowsZero(reqCardinality)) {
      val err = new XPathException(
        "Required type does not allow empty sequence, but supplied value is empty")
      err.setIsTypeError(true)
      err.setErrorCode("XPTY0004")
      return err
    }
    if (count > 1 && !Cardinality.allowsMany(reqCardinality)) {
      val err = new XPathException(
        "Required type requires a singleton sequence; supplied value contains " +
          count +
          " items")
      err.setIsTypeError(true)
      err.setErrorCode("XPTY0004")
      return err
    }
    if (count > 0 && reqCardinality == StaticProperty.EMPTY) {
      val err = new XPathException(
        "Required type requires an empty sequence, but supplied value is non-empty")
      err.setIsTypeError(true)
      err.setErrorCode("XPTY0004")
      return err
    }
    null
  }

  def ebvError(exp: Expression, th: TypeHierarchy): XPathException = {
    if (Cardinality.allowsZero(exp.getCardinality)) {
      return null
    }
    val t: ItemType = exp.getItemType
    if (th.relationship(t, Type.NODE_TYPE) == DISJOINT &&
      th.relationship(t, BuiltInAtomicType.BOOLEAN) == DISJOINT &&
      th.relationship(t, BuiltInAtomicType.STRING) == DISJOINT &&
      th.relationship(t, BuiltInAtomicType.ANY_URI) == DISJOINT &&
      th.relationship(t, BuiltInAtomicType.UNTYPED_ATOMIC) ==
        DISJOINT &&
      th.relationship(t, NumericType.getInstance) == DISJOINT &&
      !t.isInstanceOf[JavaExternalObjectType]) {
      val err = new XPathException(
        "Effective boolean value is defined only for sequences containing " +
          "booleans, strings, numbers, URIs, or nodes")
      err.setErrorCode("FORG0006")
      err.setIsTypeError(true)
      return err
    }
    null
  }

  private def makePromoterToDouble(exp: Expression): Expression =
    makePromoter(exp,
      new Converter.PromoterToDouble(),
      BuiltInAtomicType.DOUBLE)

  private def makePromoterToFloat(exp: Expression): Expression =
    makePromoter(exp, new Converter.PromoterToFloat(), BuiltInAtomicType.FLOAT)

  private def makePromoterToString(exp: Expression): Expression =
    makePromoter(exp,
      new Converter.ToStringConverter(),
      BuiltInAtomicType.STRING)

  private def makePromoter(exp: Expression,
                           converter: Converter,
                           `type`: BuiltInAtomicType): Expression = {
    val rules: ConversionRules = exp.getConfiguration.getConversionRules
    converter.setConversionRules(rules)
    exp match {
      case literal: Literal if literal.value.isInstanceOf[AtomicValue] =>
        val result = converter.convert(literal.value.asInstanceOf[AtomicValue])
        result match {
          case atomicValue: AtomicValue =>
            val converted = Literal.makeLiteral(atomicValue, exp)
            ExpressionTool.copyLocationInfo(exp, converted)
            return converted
          case _ =>
        }
      case _ =>
    }
    val asc: AtomicSequenceConverter = new AtomicSequenceConverter(exp, `type`)
    asc.setConverter(converter)
    ExpressionTool.copyLocationInfo(exp, asc)
    asc
  }

}

class TypeChecker {

  def staticTypeCheck(supplied: Expression,
                      req: SequenceType,
                      role: RoleDiagnostic,
                      visitor: ExpressionVisitor): Expression = {
    if (supplied.implementsStaticTypeCheck()) {
      supplied.staticTypeCheck(req, backwardsCompatible = false, role, visitor)
    }
    var exp: Expression = supplied
    val env: StaticContext = visitor.getStaticContext
    val config: Configuration = env.getConfiguration
    val th = config.getTypeHierarchy
    val defaultContextInfo: ContextItemStaticInfo =
      config.getDefaultContextItemStaticInfo
    val reqItemType: ItemType = req.getPrimaryType
    val reqCard: Int = req.getCardinality
    val allowsMany: Boolean = Cardinality.allowsMany(reqCard)
    var suppliedItemType: ItemType = null
    var suppliedCard: Int = -1
    var cardOK: Boolean = reqCard == StaticProperty.ALLOWS_ZERO_OR_MORE
    if (!cardOK) {
      suppliedCard = exp.getCardinality
      cardOK = Cardinality.subsumes(reqCard, suppliedCard)
    }

    var itemTypeOK = reqItemType eq AnyItemType
    if (reqCard == StaticProperty.ALLOWS_ZERO)
      itemTypeOK = true

    if (! itemTypeOK) {
      suppliedItemType = exp.getItemType
      if (reqItemType == null || suppliedItemType == null) {
        throw new NullPointerException()
      }
      val relation = th.relationship(reqItemType, suppliedItemType)
      itemTypeOK = relation == Affinity.SAME_TYPE || relation == Affinity.SUBSUMES
    }
    if (! itemTypeOK) {
      if (reqItemType.isPlainType) {
        if (!suppliedItemType.isPlainType && !(suppliedCard == StaticProperty.EMPTY)) {
          if (!suppliedItemType.isAtomizable(th)) {
            var shortItemType: String = null
            shortItemType =
              suppliedItemType match {
                case _: TupleType => "a tuple type"
                case _: MapType => "a map type"
                case _: FunctionItemType => "a function type"
                case _: NodeTest => "an element type with element-only content"
                case _ => suppliedItemType.toString
              }
            val err = new XPathException(
              "An atomic value is required for the " + role.getMessage +
                ", but the supplied type is " +
                shortItemType +
                ", which cannot be atomized",
              "FOTY0013",
              supplied.getLocation
            )
            err.setIsTypeError(true)
            err.setFailingExpression(supplied)
            throw err
          }
          if (exp.getRetainedStaticContext == null) {
            exp.setRetainedStaticContextLocally(
              env.makeRetainedStaticContext())
          }
          var cexp: Expression = Atomizer.makeAtomizer(exp, role)
          ExpressionTool.copyLocationInfo(exp, cexp)
          exp = cexp
          cexp = exp.simplify()
          ExpressionTool.copyLocationInfo(exp, cexp)
          exp = cexp
          suppliedItemType = exp.getItemType
          suppliedCard = exp.getCardinality
          cardOK = Cardinality.subsumes(reqCard, suppliedCard)
        }
        if (suppliedItemType == BuiltInAtomicType.UNTYPED_ATOMIC &&
          !(reqItemType == BuiltInAtomicType.UNTYPED_ATOMIC || reqItemType == BuiltInAtomicType.ANY_ATOMIC)) {
          if (reqItemType.asInstanceOf[PlainType].isNamespaceSensitive) {
            val err = new XPathException(
              "An untyped atomic value cannot be converted to a QName or NOTATION as required for the " +
                role.getMessage,
              "XPTY0117",
              supplied.getLocation)
            err.setIsTypeError(true)
            throw err
          }
          val cexp: UntypedSequenceConverter =
            UntypedSequenceConverter.makeUntypedSequenceConverter(
              config,
              exp,
              reqItemType.asInstanceOf[PlainType])
          cexp.setRoleDiagnostic(role)
          ExpressionTool.copyLocationInfo(exp, cexp)
          try if (exp.isInstanceOf[Literal]) {
            exp = Literal.makeLiteral(
              cexp.iterate(visitor.makeDynamicContext()).materialize,
              exp)
            ExpressionTool.copyLocationInfo(cexp, exp)
          } else {
            exp = cexp
          } catch {
            case err: XPathException => {
              err.maybeSetLocation(exp.getLocation)
              err.setFailingExpression(supplied)
              err.setErrorCode(role.getErrorCode)
              err.setIsStaticError(true)
              throw err
            }

          }
          itemTypeOK = true
          suppliedItemType = reqItemType
        }
        if (suppliedItemType == BuiltInAtomicType.ANY_ATOMIC &&
          !(reqItemType == BuiltInAtomicType.UNTYPED_ATOMIC || reqItemType == BuiltInAtomicType.ANY_ATOMIC) &&
          !exp.hasSpecialProperty(StaticProperty.NOT_UNTYPED_ATOMIC)) {
          var conversion: Expression = null
          if (reqItemType.asInstanceOf[PlainType].isNamespaceSensitive) {
            conversion = UntypedSequenceConverter.makeUntypedSequenceRejector(
              config,
              exp,
              reqItemType.asInstanceOf[PlainType])
          } else {
            val usc: UntypedSequenceConverter =
              UntypedSequenceConverter.makeUntypedSequenceConverter(
                config,
                exp,
                reqItemType.asInstanceOf[PlainType])
            usc.setRoleDiagnostic(role)
            conversion = usc
          }
          ExpressionTool.copyLocationInfo(exp, conversion)
          try {
            if (exp.isInstanceOf[Literal]) {
              exp = Literal.makeLiteral(
                conversion.iterate(visitor.makeDynamicContext()).materialize,
                exp)
              ExpressionTool.copyLocationInfo(supplied, exp)
            } else {
              exp = conversion
            }
            suppliedItemType = exp.getItemType
          } catch {
            case err: XPathException => {
              err.maybeSetLocation(exp.getLocation)
              err.setIsStaticError(true)
              throw err
            }

          }
        }
        reqItemType match {
          case atomicType: AtomicType =>
            val rt = atomicType.getFingerprint
            if (rt == StandardNames.XS_DOUBLE &&
              th.relationship(suppliedItemType, NumericType.getInstance) != DISJOINT) {
              val cexp = makePromoterToDouble(exp)
              cexp match {
                case converter: AtomicSequenceConverter =>
                  converter.setRoleDiagnostic(role)
                case _ =>
              }
              ExpressionTool.copyLocationInfo(exp, cexp)
              exp = cexp
              try
                exp = exp.simplify().typeCheck(visitor, defaultContextInfo)
              catch {
                case err: XPathException => {
                  err.maybeSetLocation(exp.getLocation)
                  err.setIsStaticError(true)
                  throw err
                }
              }
              suppliedItemType = BuiltInAtomicType.DOUBLE
              suppliedCard = -1
            } else if (rt == StandardNames.XS_FLOAT &&
              th.relationship(suppliedItemType, NumericType.getInstance) !=
                DISJOINT &&
              !th.isSubType(suppliedItemType, BuiltInAtomicType.DOUBLE)) {
              val cexp: Expression = makePromoterToFloat(exp)
              if (cexp.isInstanceOf[AtomicSequenceConverter]) {
                cexp
                  .asInstanceOf[AtomicSequenceConverter]
                  .setRoleDiagnostic(role)
              }
              ExpressionTool.copyLocationInfo(exp, cexp)
              exp = cexp
              try exp = exp.simplify().typeCheck(visitor, defaultContextInfo)
              catch {
                case err: XPathException => {
                  err.maybeSetLocation(exp.getLocation)
                  err.setFailingExpression(supplied)
                  err.setIsStaticError(true)
                  throw err
                }

              }
              suppliedItemType = BuiltInAtomicType.FLOAT
              suppliedCard = -1
            }
            if (rt == StandardNames.XS_STRING &&
              th.isSubType(suppliedItemType, BuiltInAtomicType.ANY_URI)) {
              itemTypeOK = true
              val cexp: Expression = makePromoterToString(exp)
              cexp match {
                case converter: AtomicSequenceConverter =>
                  converter.setRoleDiagnostic(role)
                case _ =>
              }
              ExpressionTool.copyLocationInfo(exp, cexp)
              exp = cexp
              try
                exp = exp.simplify().typeCheck(visitor, defaultContextInfo)
              catch {
                case err: XPathException => {
                  err.maybeSetLocation(exp.getLocation)
                  err.setFailingExpression(supplied)
                  err.setIsStaticError(true)
                  throw err
                }

              }
              suppliedItemType = BuiltInAtomicType.STRING
              suppliedCard = -1
            }
          case _ =>
        }
      } else reqItemType match {
        case itemType: FunctionItemType if ! itemType.isArrayType && ! itemType.isMapType =>
          val r = th.relationship(suppliedItemType, th.getGenericFunctionItemType)
          if (r != DISJOINT) {
            if (! suppliedItemType.isInstanceOf[FunctionItemType]) {
              exp = new ItemChecker(exp, th.getGenericFunctionItemType, role)
              suppliedItemType = th.getGenericFunctionItemType
            }
            exp = makeFunctionSequenceCoercer(
              exp,
              itemType,
              visitor,
              role)
            itemTypeOK = true
          }
        case objectType: JavaExternalObjectType if reqCard == StaticProperty.EXACTLY_ONE =>
          if (classOf[Sequence].isAssignableFrom(objectType.getJavaClass)) {
            itemTypeOK = true
          } else supplied match {
            case call: FunctionCall =>
              if (call.adjustRequiredType(objectType)) {
                itemTypeOK = true
                cardOK = true
              }
            case _ =>
          }
        case _ =>
      }
    }
    if (itemTypeOK && cardOK) {
      return exp
    }
    if (suppliedCard == -1) {
      suppliedCard = exp.getCardinality
      if (!cardOK) {
        cardOK = Cardinality.subsumes(reqCard, suppliedCard)
      }
    }
    if (cardOK && suppliedCard == StaticProperty.EMPTY) {
      return exp
    }
    if (suppliedCard == StaticProperty.EMPTY && ((reqCard & StaticProperty.ALLOWS_ZERO) == 0)) {
      val err = new XPathException(
        "An empty sequence is not allowed as the " + role.getMessage,
        role.getErrorCode,
        supplied.getLocation)
      err.setIsTypeError(role.isTypeError)
      err.setFailingExpression(supplied)
      throw err
    }
    var relation: Affinity =
      if (itemTypeOK)
        SUBSUMED_BY
      else
        th.relationship(suppliedItemType, reqItemType)
    if (reqCard == StaticProperty.ALLOWS_ZERO) {
      relation = SAME_TYPE
    }
    if (relation == DISJOINT) {
      if (Cardinality.allowsZero(suppliedCard) && Cardinality.allowsZero(reqCard)) {
        if (suppliedCard != StaticProperty.EMPTY) {
          var msg: String = role.composeErrorMessage(reqItemType, supplied, th)
          msg += ". The expression can succeed only if the supplied value is an empty sequence."
          visitor.issueWarning(msg, supplied.getLocation)
        }
      } else {
        val msg = role.composeErrorMessage(reqItemType, supplied, th)
        val err =
          new XPathException(msg, role.getErrorCode, supplied.getLocation)
        err.setIsTypeError(role.isTypeError)
        err.setFailingExpression(supplied)
        throw err
      }
    }
    if (!(relation == SAME_TYPE || relation == SUBSUMED_BY)) {
      exp match {
        case literal: Literal =>
          if (req.matches(literal.value, th))
            return exp
          val msg = role.composeErrorMessage(reqItemType, supplied, th)
          val err = new XPathException(msg, role.getErrorCode, supplied.getLocation)
          err.setIsTypeError(role.isTypeError)
          throw err
        case _ =>
          val cexp = new ItemChecker(exp, reqItemType, role)
          ExpressionTool.copyLocationInfo(exp, cexp)
          exp = cexp
      }
    }
    if (!cardOK) {
      if (exp.isInstanceOf[Literal]) {
        val err = new XPathException(
          "Required cardinality of " + role.getMessage + " is " +
            Cardinality.toString(reqCard) +
            "; supplied value has cardinality " +
            Cardinality.toString(suppliedCard),
          role.getErrorCode,
          supplied.getLocation
        )
        err.setIsTypeError(role.isTypeError)
        throw err
      } else {
        val cexp: Expression =
          CardinalityChecker.makeCardinalityChecker(exp, reqCard, role)
        ExpressionTool.copyLocationInfo(exp, cexp)
        exp = cexp
      }
    }
    exp
  }

  def makeArithmeticExpression(lhs: Expression,
                               operator: Int,
                               rhs: Expression): Expression =
    new ArithmeticExpression(lhs, operator, rhs)

  def makeGeneralComparison(lhs: Expression,
                            operator: Int,
                            rhs: Expression): Expression =
    new GeneralComparison20(lhs, operator, rhs)

  def processValueOf(select: Expression, config: Configuration): Expression =
    select
}
