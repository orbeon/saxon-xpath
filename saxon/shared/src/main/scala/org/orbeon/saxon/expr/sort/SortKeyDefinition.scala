package org.orbeon.saxon.expr.sort

import org.orbeon.saxon.utils.Configuration

import org.orbeon.saxon.utils.Version

import org.orbeon.saxon.expr._

import org.orbeon.saxon.expr.parser.ContextItemStaticInfo

import org.orbeon.saxon.expr.parser.ExpressionVisitor

import org.orbeon.saxon.expr.parser.RebindingMap

import org.orbeon.saxon.lib.StringCollator

import org.orbeon.saxon.model.StringConverter

import org.orbeon.saxon.model.ValidationFailure

import org.orbeon.saxon.om.StandardNames

import org.orbeon.saxon.trace.ExpressionPresenter

import org.orbeon.saxon.trans.XPathException

import org.orbeon.saxon.value.SequenceType

import org.orbeon.saxon.value.StringValue

import org.orbeon.saxon.value.Whitespace

import java.net.URI

import java.net.URISyntaxException

import java.util.ArrayList

import java.util.List

import java.util.Objects

import java.util.Properties

import scala.beans.{BeanProperty, BooleanBeanProperty}

//import scala.collection.compat._
import scala.jdk.CollectionConverters._

class SortKeyDefinition extends PseudoExpression {

   var sortKey: Operand = _

   var order: Operand = new Operand(this,
    new StringLiteral("ascending"),
    OperandRole.SINGLE_ATOMIC)

   var dataTypeExpression: Operand = null

   var caseOrder: Operand =
    new Operand(this, new StringLiteral("#default"), OperandRole.SINGLE_ATOMIC)

   var language: Operand = new Operand(
    this,
    new StringLiteral(StringValue.EMPTY_STRING),
    OperandRole.SINGLE_ATOMIC)

   var collationName: Operand = null

   var stable: Operand = null

   var collation: StringCollator = _

   var baseURI: String = _

   var emptyLeast: Boolean = true

   var backwardsCompatible: Boolean = false

   var setContextForSortKey: Boolean = false

  @BeanProperty
  var finalComparator: AtomicComparer = null

  override def isLiftable(forStreaming: Boolean): Boolean = false

  def setSortKey(exp: Expression, setContext: Boolean): Unit = {
    var opRole: OperandRole = null
    opRole =
      if (setContext)
        new OperandRole(
          OperandRole.HAS_SPECIAL_FOCUS_RULES | OperandRole.HIGHER_ORDER,
          OperandUsage.TRANSMISSION,
          SequenceType.ANY_SEQUENCE)
      else OperandRole.ATOMIC_SEQUENCE
    sortKey = new Operand(this, exp, opRole)
    setContextForSortKey = setContext
  }

  def getSortKey: Expression = sortKey.getChildExpression

  def getSortKeyOperand: Operand = sortKey

  def isSetContextForSortKey: Boolean = setContextForSortKey

  def setOrder(exp: Expression): Unit = {
    order.setChildExpression(exp)
  }

  def getOrder: Expression = order.getChildExpression

  def setDataTypeExpression(exp: Expression): Unit = {
    if (exp == null) {
      dataTypeExpression = null
    } else {
      if (dataTypeExpression == null) {
        dataTypeExpression = new Operand(this, exp, OperandRole.SINGLE_ATOMIC)
      }
      dataTypeExpression.setChildExpression(exp)
    }
  }

  def getDataTypeExpression: Expression =
    if (dataTypeExpression == null) null
    else dataTypeExpression.getChildExpression

  def setCaseOrder(exp: Expression): Unit = {
    caseOrder.setChildExpression(exp)
  }

  def getCaseOrder: Expression = caseOrder.getChildExpression

  def setLanguage(exp: Expression): Unit = {
    language.setChildExpression(exp)
  }

  def getLanguage: Expression = language.getChildExpression

  def setCollationNameExpression(collationNameExpr: Expression): Unit = {
    if (collationNameExpr == null) {
      collationName = null
    } else {
      if (collationName == null) {
        collationName =
          new Operand(this, collationNameExpr, OperandRole.SINGLE_ATOMIC)
      }
      collationName.setChildExpression(collationNameExpr)
    }
  }

  def getCollationNameExpression: Expression =
    if (collationName == null) null else collationName.getChildExpression

  def setCollation(collation: StringCollator): Unit = {
    this.collation = collation
  }

  def getCollation: StringCollator = collation

  def setBaseURI(baseURI: String): Unit = {
    this.baseURI = baseURI
  }

  def getBaseURI: String = baseURI

  def setStable(stableExpr: Expression): Unit = {
    var stableExpression = stableExpr
    if (stableExpression == null) {
      stableExpression = new StringLiteral("yes")
    }
    if (stable == null) {
      stable = new Operand(this, stableExpression, OperandRole.SINGLE_ATOMIC)
    }
    stable.setChildExpression(stableExpression)
  }

  def getStable: Expression = stable.getChildExpression

  def setBackwardsCompatible(compatible: Boolean): Unit = {
    backwardsCompatible = compatible
  }

  def isBackwardsCompatible: Boolean = backwardsCompatible

  def setEmptyLeast(emptyLeast: Boolean): Unit = {
    this.emptyLeast = emptyLeast
  }

  def getEmptyLeast: Boolean = emptyLeast

  def isFixed: Boolean =
    order.getChildExpression.isInstanceOf[Literal] &&
      (dataTypeExpression == null ||
        dataTypeExpression.getChildExpression.isInstanceOf[Literal]) &&
      caseOrder.getChildExpression.isInstanceOf[Literal] &&
      language.getChildExpression.isInstanceOf[Literal] &&
      (stable == null || stable.getChildExpression.isInstanceOf[Literal]) &&
      (collationName == null ||
        collationName.getChildExpression.isInstanceOf[Literal])

  def copy(rm: RebindingMap): SortKeyDefinition = {
    val sk2: SortKeyDefinition = new SortKeyDefinition()
    sk2.setSortKey(copy(sortKey.getChildExpression, rm), setContext = true)
    sk2.setOrder(copy(order.getChildExpression, rm))
    sk2.setDataTypeExpression(
      if (dataTypeExpression == null) null
      else copy(dataTypeExpression.getChildExpression, rm))
    sk2.setCaseOrder(copy(caseOrder.getChildExpression, rm))
    sk2.setLanguage(copy(language.getChildExpression, rm))
    sk2.setStable(
      copy(if (stable == null) null else stable.getChildExpression, rm))
    sk2.setCollationNameExpression(
      if (collationName == null) null
      else copy(collationName.getChildExpression, rm))
    sk2.collation = collation
    sk2.emptyLeast = emptyLeast
    sk2.baseURI = baseURI
    sk2.backwardsCompatible = backwardsCompatible
    sk2.finalComparator = finalComparator
    sk2.setContextForSortKey = setContextForSortKey
    sk2
  }

  private def copy(in: Expression, rebindings: RebindingMap): Expression =
    if (in == null) null else in.copy(rebindings)

  override def typeCheck(visitor: ExpressionVisitor,
                contextItemType: ContextItemStaticInfo): SortKeyDefinition = {
    for (o <- checkedOperands.asScala if o.hasSameFocus) {
      o.typeCheck(visitor, contextItemType)
    }
    val lang: Expression = getLanguage
    if (lang.isInstanceOf[StringLiteral] &&
      !lang.asInstanceOf[StringLiteral].getStringValue.isEmpty) {
      val vf: ValidationFailure = StringConverter.StringToLanguage.INSTANCE
        .validate(lang.asInstanceOf[StringLiteral].getStringValue)
      if (vf != null) {
        throw new XPathException(
          "The lang attribute of xsl:sort must be a valid language code",
          "XTDE0030")
      }
    }
    this
  }

  override def operands: java.lang.Iterable[Operand] = {
    val list: List[Operand] = new ArrayList[Operand](8)
    list.add(sortKey)
    list.add(order)
    if (dataTypeExpression != null) {
      list.add(dataTypeExpression)
    }
    list.add(caseOrder)
    list.add(language)
    if (stable != null) {
      list.add(stable)
    }
    if (collationName != null) {
      list.add(collationName)
    }
    list
  }

  override def getImplementationMethod: Int = 0

  def makeComparator(context: XPathContext): AtomicComparer = {
    val orderX: String =
      order.getChildExpression.evaluateAsString(context).toString
    val config: Configuration = context.getConfiguration
    var atomicComparer: AtomicComparer = null
    var stringCollator: StringCollator = null
    if (collation != null) {
      stringCollator = collation
    } else if (collationName != null) {
      val cname: String =
        collationName.getChildExpression.evaluateAsString(context).toString
      var collationURI: URI = null
      collationURI = new URI(cname)
      if (!collationURI.isAbsolute) {
        if (baseURI == null) {
          throw new XPathException(
            "Collation URI is relative, and base URI is unknown")
        } else {
          val base: URI = new URI(baseURI)
          collationURI = base.resolve(collationURI)
        }
      }
      stringCollator =
        context.getConfiguration.getCollation(collationURI.toString)
      if (stringCollator == null) {
        throw new XPathException("Unknown collation " + collationURI,
          "XTDE1035")
      }
    } else {
      val caseOrderX: String =
        caseOrder.getChildExpression.evaluateAsString(context).toString
      val languageX: String =
        language.getChildExpression.evaluateAsString(context).toString
      var uri: String = "http:"
      var firstParam: Boolean = true
      val props: Properties = new Properties()
      if (!languageX.isEmpty) {
        val vf: ValidationFailure =
          StringConverter.StringToLanguage.INSTANCE.validate(languageX)
        if (vf != null) {
          throw new XPathException(
            "The lang attribute of xsl:sort must be a valid language code",
            "XTDE0030")
        }
        props.setProperty("lang", languageX)
        uri += "?lang=" + languageX
        firstParam = false
      }
      if (caseOrderX.!=("#default")) {
        props.setProperty("case-order", caseOrderX)
        uri += (if (firstParam) "?" else ";") + "case-order=" + caseOrderX
        firstParam = false
      }
      stringCollator = Version.platform.makeCollation(config, props, uri)  // need to changes in calling method parameter type
    }
    if (dataTypeExpression == null) {
      atomicComparer = AtomicSortComparer.makeSortComparer(
        stringCollator,
        sortKey.getChildExpression.getItemType.getAtomizedItemType.getPrimitiveType,
        context)
      if (!emptyLeast) {
        atomicComparer = new EmptyGreatestComparer(atomicComparer)
      }
    } else {
      val dataType: String = dataTypeExpression.getChildExpression
        .evaluateAsString(context)
        .toString
      dataType match {
        case "text" =>
          atomicComparer = AtomicSortComparer.makeSortComparer(
            stringCollator,
            StandardNames.XS_STRING,
            context)
          atomicComparer = new TextComparer(atomicComparer)
        case "number" =>
          atomicComparer =
            if (context.getConfiguration.getXsdVersion == Configuration.XSD10)
              NumericComparer.getInstance
            else NumericComparer11.getInstance
        case _ =>
          var err: XPathException = new XPathException(
            "data-type on xsl:sort must be 'text' or 'number'")
          err.setErrorCode("XTDE0030")
          throw err

      }
    }
    if (stable != null) {
      val stableVal: StringValue = stable.getChildExpression
        .evaluateItem(context)
        .asInstanceOf[StringValue]
      val s: String = Whitespace.trim(stableVal.getStringValue)
      if (s.equals("yes") || s.equals("no") || s.equals("true") || s.equals("false") ||
        s.equals("1") ||
        s.equals("0")) {} else {
        val err = new XPathException(
          "Value of 'stable' on xsl:sort must be yes|no|true|false|1|0")
        err.setErrorCode("XTDE0030")
        throw err
      }
    }
    orderX match {
      case "ascending" => atomicComparer
      case "descending" => new DescendingComparer(atomicComparer)
      case _ =>
        var err1: XPathException = new XPathException(
          "order must be 'ascending' or 'descending'")
        err1.setErrorCode("XTDE0030")
        throw err1

    }
  }

  def fix(context: XPathContext): SortKeyDefinition = {
    val newSKD: SortKeyDefinition = this.copy(new RebindingMap())
    newSKD.setLanguage(
      new StringLiteral(this.getLanguage.evaluateAsString(context)))
    newSKD.setOrder(new StringLiteral(this.getOrder.evaluateAsString(context)))
    if (collationName != null) {
      newSKD.setCollationNameExpression(
        new StringLiteral(
          this.getCollationNameExpression.evaluateAsString(context)))
    }
    newSKD.setCaseOrder(
      new StringLiteral(this.getCaseOrder.evaluateAsString(context)))
    if (dataTypeExpression != null) {
      newSKD.setDataTypeExpression(
        new StringLiteral(
          this.getDataTypeExpression.evaluateAsString(context)))
    }
    newSKD.setSortKey(new ContextItemExpression(), setContext = true)
    newSKD
  }

  override def equals(other: Any): Boolean = other match {
    case other: SortKeyDefinition => {
      val s2: SortKeyDefinition = other
      Objects.equals(getSortKey, s2.getSortKey) && Objects.equals(getOrder,
        s2.getOrder) &&
        Objects.equals(getLanguage, s2.getLanguage) &&
        Objects.equals(getDataTypeExpression, s2.getDataTypeExpression) &&
        Objects.equals(getStable, s2.getStable) &&
        Objects.equals(getCollationNameExpression, s2.getCollationNameExpression)
    }
    case _ => false

  }

  override def computeHashCode(): Int = {
    var h: Int = 0
    h ^= getOrder.hashCode
    h ^= getCaseOrder.hashCode
    h ^= getLanguage.hashCode
    if (getDataTypeExpression != null) {
      h ^= getDataTypeExpression.hashCode
    }
    if (getStable != null) {
      h ^= getStable.hashCode
    }
    if (getCollationNameExpression != null) {
      h ^= getCollationNameExpression.hashCode
    }
    h
  }

  override def export(out: ExpressionPresenter): Unit = {
    out.startElement("sortKey", this)
    if (finalComparator != null) {
      out.emitAttribute("comp", finalComparator.save())
    }
    out.setChildRole("select")
    sortKey.getChildExpression.export(out)
    out.setChildRole("order")
    order.getChildExpression.export(out)
    if (dataTypeExpression != null) {
      out.setChildRole("dataType")
      dataTypeExpression.getChildExpression.export(out)
    }
    out.setChildRole("lang")
    language.getChildExpression.export(out)
    out.setChildRole("caseOrder")
    caseOrder.getChildExpression.export(out)
    if (stable != null) {
      out.setChildRole("stable")
      stable.getChildExpression.export(out)
    }
    if (collationName != null) {
      out.setChildRole("collation")
      collationName.getChildExpression.export(out)
    }
    out.endElement()
  }

}
