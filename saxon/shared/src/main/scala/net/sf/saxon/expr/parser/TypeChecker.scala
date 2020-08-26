package net.sf.saxon.expr.parser

import net.sf.saxon.utils.Configuration

import net.sf.saxon.expr._

import net.sf.saxon.lib.ConversionRules

import net.sf.saxon.ma.map.MapType

import net.sf.saxon.ma.map.TupleType

import net.sf.saxon.model._

import net.sf.saxon.om.Item

import net.sf.saxon.om.Sequence

import net.sf.saxon.om.SequenceIterator

import net.sf.saxon.om.StandardNames

import net.sf.saxon.pattern.NodeTest

import net.sf.saxon.trans.XPathException

import net.sf.saxon.value.AtomicValue

import net.sf.saxon.value.Cardinality

import net.sf.saxon.value.SequenceType

import net.sf.saxon.model.Affinity._

import TypeChecker._

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
    var exp: Expression = supplied
    val th: TypeHierarchy = env.getConfiguration.getTypeHierarchy
    val reqItemType: ItemType = req.getPrimaryType
    val reqCard: Int = req.getCardinality
    var suppliedItemType: ItemType = null
    var suppliedCard: Int = -1
    var cardOK: Boolean = reqCard == StaticProperty.ALLOWS_ZERO_OR_MORE
    if (!cardOK) {
      suppliedCard = exp.getCardinality
      cardOK = Cardinality.subsumes(reqCard, suppliedCard)
    }
    var itemTypeOK: Boolean = req.getPrimaryType.isInstanceOf[AnyItemType]
    if (!itemTypeOK) {
      suppliedItemType = exp.getItemType
      val relation: Affinity = th.relationship(reqItemType, suppliedItemType)
      itemTypeOK = relation == SAME_TYPE || relation == SUBSUMES
    }
    if (itemTypeOK && cardOK) {
      return exp
    }
    if (suppliedCard == -1) {
      suppliedCard =
        if (suppliedItemType.isInstanceOf[ErrorType]) StaticProperty.EMPTY
        else exp.getCardinality
      if (!cardOK) {
        cardOK = Cardinality.subsumes(reqCard, suppliedCard)
      }
    }
    if (cardOK && suppliedCard == StaticProperty.EMPTY) {
      return exp
    }
    if (suppliedItemType == null) {
      suppliedItemType = exp.getItemType
    }
    if (suppliedCard == StaticProperty.EMPTY && ((reqCard & StaticProperty.ALLOWS_ZERO) == 0)) {
      val err: XPathException = new XPathException(
        "An empty sequence is not allowed as the " + role.getMessage,
        role.getErrorCode,
        supplied.getLocation)
      err.setIsTypeError(role.isTypeError)
      throw err
    }
    val relation: Affinity = th.relationship(suppliedItemType, reqItemType)
    if (relation == DISJOINT) {
      if (Cardinality.allowsZero(suppliedCard) && Cardinality.allowsZero(
        reqCard)) {
        if (suppliedCard != StaticProperty.EMPTY) {
          val msg: String = "Required item type of " + role.getMessage + " is " +
            reqItemType +
            "; supplied value (" +
            supplied.toShortString() +
            ") has item type " +
            suppliedItemType +
            ". The expression can succeed only if the supplied value is an empty sequence."
          env.issueWarning(msg, supplied.getLocation)
        }
      } else {
        val msg: String = role.composeErrorMessage(reqItemType, supplied, th)
        val err: XPathException =
          new XPathException(msg, role.getErrorCode, supplied.getLocation)
        err.setIsTypeError(role.isTypeError)
        throw err
      }
    }
    if (!(relation == SAME_TYPE || relation == SUBSUMED_BY)) {
      val cexp: Expression = new ItemChecker(exp, reqItemType, role)
      cexp.adoptChildExpression(exp)
      exp = cexp
    }
    if (!cardOK) {
      if (exp.isInstanceOf[Literal]) {
        val err: XPathException = new XPathException(
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
    while (({
      item = iter.next()
      item
    }) != null) {
      { count += 1;}
      if (!reqItemType.matches(item,
        context.getConfiguration.getTypeHierarchy)) {
        val err: XPathException = new XPathException(
          "Required type is " + reqItemType + "; supplied value has type " +
            UType.getUType(`val`.materialize()))
        err.setIsTypeError(true)
        err.setErrorCode("XPTY0004")
        err
      }
    }
    val reqCardinality: Int = requiredType.getCardinality
    if (count == 0 && !Cardinality.allowsZero(reqCardinality)) {
      val err: XPathException = new XPathException(
        "Required type does not allow empty sequence, but supplied value is empty")
      err.setIsTypeError(true)
      err.setErrorCode("XPTY0004")
      return err
    }
    if (count > 1 && !Cardinality.allowsMany(reqCardinality)) {
      val err: XPathException = new XPathException(
        "Required type requires a singleton sequence; supplied value contains " +
          count +
          " items")
      err.setIsTypeError(true)
      err.setErrorCode("XPTY0004")
      return err
    }
    if (count > 0 && reqCardinality == StaticProperty.EMPTY) {
      val err: XPathException = new XPathException(
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
      !(t.isInstanceOf[JavaExternalObjectType])) {
      val err: XPathException = new XPathException(
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
    if (exp.isInstanceOf[Literal] &&
      exp.asInstanceOf[Literal].value.isInstanceOf[AtomicValue]) {
      val result: ConversionResult = converter.convert(
        exp.asInstanceOf[Literal].value.asInstanceOf[AtomicValue])
      if (result.isInstanceOf[AtomicValue]) {
        val converted: Literal =
          Literal.makeLiteral(result.asInstanceOf[AtomicValue], exp)
        ExpressionTool.copyLocationInfo(exp, converted)
        return converted
      }
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
      supplied.staticTypeCheck(req, false, role, visitor)
    }
    var exp: Expression = supplied
    val env: StaticContext = visitor.getStaticContext
    val config: Configuration = env.getConfiguration
    val th: TypeHierarchy = config.getTypeHierarchy
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
    var itemTypeOK: Boolean = reqItemType.isInstanceOf[AnyItemType]
    if (reqCard == StaticProperty.ALLOWS_ZERO) {
      itemTypeOK = true
    }
    if (!itemTypeOK) {
      suppliedItemType = exp.getItemType
      if (reqItemType == null || suppliedItemType == null) {
        throw new NullPointerException()
      }
      val relation: Affinity = th.relationship(reqItemType, suppliedItemType)
      itemTypeOK = relation == Affinity.SAME_TYPE || relation == Affinity.SUBSUMES
    }
    if (!itemTypeOK) {
      if (reqItemType.isPlainType) {
        if (!suppliedItemType.isPlainType && !(suppliedCard == StaticProperty.EMPTY)) {
          if (!suppliedItemType.isAtomizable(th)) {
            var shortItemType: String = null
            shortItemType =
              if (suppliedItemType.isInstanceOf[TupleType]) "a tuple type"
              else if (suppliedItemType.isInstanceOf[MapType]) "a map type"
              else if (suppliedItemType.isInstanceOf[FunctionItemType])
                "a function type"
              else if (suppliedItemType.isInstanceOf[NodeTest])
                "an element type with element-only content"
              else suppliedItemType.toString
            val err: XPathException = new XPathException(
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
            val err: XPathException = new XPathException(
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
              cexp.iterate(visitor.makeDynamicContext()).materialize(),
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
                conversion.iterate(visitor.makeDynamicContext()).materialize(),
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
        if (reqItemType.isInstanceOf[AtomicType]) {
          val rt: Int = reqItemType.asInstanceOf[AtomicType].getFingerprint
          if (rt == StandardNames.XS_DOUBLE &&
            th.relationship(suppliedItemType, NumericType.getInstance) !=
              DISJOINT) {
            val cexp: Expression = makePromoterToDouble(exp)
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
            suppliedItemType = BuiltInAtomicType.STRING
            suppliedCard = -1
          }
        }
      } else if (reqItemType.isInstanceOf[FunctionItemType] &&
        !reqItemType.asInstanceOf[FunctionItemType].isMapType &&
        !reqItemType.asInstanceOf[FunctionItemType].isArrayType) {
        val r: Affinity =
          th.relationship(suppliedItemType, th.getGenericFunctionItemType)
        if (r != DISJOINT) {
          if (!(suppliedItemType.isInstanceOf[FunctionItemType])) {
            exp = new ItemChecker(exp, th.getGenericFunctionItemType, role)
            suppliedItemType = th.getGenericFunctionItemType
          }
          exp = makeFunctionSequenceCoercer(
            exp,
            reqItemType.asInstanceOf[FunctionItemType],
            visitor,
            role)
          itemTypeOK = true
        }
      } else if (reqItemType
        .isInstanceOf[JavaExternalObjectType] && reqCard == StaticProperty.EXACTLY_ONE) {
        if (classOf[Sequence].isAssignableFrom(
          reqItemType.asInstanceOf[JavaExternalObjectType].getJavaClass)) {
          itemTypeOK = true
        } else if (supplied.isInstanceOf[FunctionCall]) {
          if (supplied
            .asInstanceOf[FunctionCall]
            .adjustRequiredType(
              reqItemType.asInstanceOf[JavaExternalObjectType])) {
            itemTypeOK = true
            cardOK = true
          }
        }
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
      val err: XPathException = new XPathException(
        "An empty sequence is not allowed as the " + role.getMessage,
        role.getErrorCode,
        supplied.getLocation)
      err.setIsTypeError(role.isTypeError)
      err.setFailingExpression(supplied)
      throw err
    }
    var relation: Affinity =
      if (itemTypeOK) SUBSUMED_BY
      else th.relationship(suppliedItemType, reqItemType)
    if (reqCard == StaticProperty.ALLOWS_ZERO) {
      relation = SAME_TYPE
    }
    if (relation == DISJOINT) {
      if (Cardinality.allowsZero(suppliedCard) && Cardinality.allowsZero(
        reqCard)) {
        if (suppliedCard != StaticProperty.EMPTY) {
          var msg: String = role.composeErrorMessage(reqItemType, supplied, th)
          msg += ". The expression can succeed only if the supplied value is an empty sequence."
          visitor.issueWarning(msg, supplied.getLocation)
        }
      } else {
        val msg: String = role.composeErrorMessage(reqItemType, supplied, th)
        val err: XPathException =
          new XPathException(msg, role.getErrorCode, supplied.getLocation)
        err.setIsTypeError(role.isTypeError)
        err.setFailingExpression(supplied)
        throw err
      }
    }
    if (!(relation == SAME_TYPE || relation == SUBSUMED_BY)) {
      if (exp.isInstanceOf[Literal]) {
        if (req.matches(exp.asInstanceOf[Literal].value, th)) {
          return exp
        }
        val msg: String = role.composeErrorMessage(reqItemType, supplied, th)
        val err: XPathException =
          new XPathException(msg, role.getErrorCode, supplied.getLocation)
        err.setIsTypeError(role.isTypeError)
        throw err
      } else {
        val cexp: Expression = new ItemChecker(exp, reqItemType, role)
        ExpressionTool.copyLocationInfo(exp, cexp)
        exp = cexp
      }
    }
    if (!cardOK) {
      if (exp.isInstanceOf[Literal]) {
        val err: XPathException = new XPathException(
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
