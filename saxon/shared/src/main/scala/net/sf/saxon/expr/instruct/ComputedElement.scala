package net.sf.saxon.expr.instruct

import net.sf.saxon.utils.Configuration

import net.sf.saxon.utils.Controller

import net.sf.saxon.event.Outputter

import net.sf.saxon.expr._

import net.sf.saxon.expr.parser._

import net.sf.saxon.functions.SystemFunction

import net.sf.saxon.lib.NamespaceConstant

import net.sf.saxon.lib.StandardURIChecker

import net.sf.saxon.lib.Validation

import net.sf.saxon.model._

import net.sf.saxon.om._

import net.sf.saxon.pattern.ContentTypeTest

import net.sf.saxon.pattern.NodeKindTest

import net.sf.saxon.trace.ExpressionPresenter

import net.sf.saxon.trans.XPathException

import net.sf.saxon.value._

import scala.beans.{BeanProperty, BooleanBeanProperty}

import scala.jdk.CollectionConverters._

class ComputedElement(elementName: Expression,
                      namespace: Expression,
                      schemaType: SchemaType,
                      validation: Int,
                      inheritNamespaces: Boolean,
                      @BooleanBeanProperty var allowNameAsQName: Boolean)
  extends ElementCreator {

  private var nameOp: Operand =
    new Operand(this, elementName, OperandRole.SINGLE_ATOMIC)

  private var namespaceOp: Operand = _

  private var itemType: ItemType = _

  if (namespace != null) {
    namespaceOp = new Operand(this, namespace, OperandRole.SINGLE_ATOMIC)
  }

  setValidationAction(validation, schemaType)

  preservingTypes = schemaType == null && validation == Validation.PRESERVE

  this.bequeathNamespacesToChildren = inheritNamespaces

  def getNameExp(): Expression = nameOp.getChildExpression

  def getNamespaceExp(): Expression =
    if (namespaceOp == null) null else namespaceOp.getChildExpression

   def setNameExp(elementName: Expression): Unit = {
    nameOp.setChildExpression(elementName)
  }

   def setNamespaceExp(namespace: Expression): Unit = {
    if (namespaceOp == null) {
      namespaceOp = new Operand(this, namespace, OperandRole.SINGLE_ATOMIC)
    } else {
      namespaceOp.setChildExpression(namespace)
    }
  }

  override def operands(): java.lang.Iterable[Operand] =
    operandSparseList(contentOp, nameOp, namespaceOp)

  def getNamespaceResolver(): NamespaceResolver = getRetainedStaticContext

 override def simplify(): Expression = {
    setNameExp(getNameExp.simplify())
    if (getNamespaceExp != null) {
      setNamespaceExp(getNamespaceExp.simplify())
    }
    val config: Configuration = getConfiguration
    val schemaAware: Boolean = getPackageData.isSchemaAware
    preservingTypes |= !schemaAware
    val schemaType: SchemaType = getSchemaType
    if (schemaType != null) {
      itemType = new ContentTypeTest(Type.ELEMENT, schemaType, config, false)
      schemaType.analyzeContentExpression(getContentExpression, Type.ELEMENT)
    } else
      itemType =
        if (getValidationAction == Validation.STRIP || !schemaAware)
          new ContentTypeTest(Type.ELEMENT, Untyped.getInstance, config, false)
        else NodeKindTest.ELEMENT
    super.simplify()
  }

override  def typeCheck(visitor: ExpressionVisitor,
                contextInfo: ContextItemStaticInfo): Expression = {
    super.typeCheck(visitor, contextInfo)
    val config: Configuration = visitor.getConfiguration
    val th: TypeHierarchy = config.getTypeHierarchy
    val role: RoleDiagnostic =
      new RoleDiagnostic(RoleDiagnostic.INSTRUCTION, "element/name", 0)
    if (allowNameAsQName) {
      setNameExp(config
        .getTypeChecker(false)
        .staticTypeCheck(getNameExp, SequenceType.SINGLE_ATOMIC, role, visitor))
      val supplied: ItemType = getNameExp.getItemType
      if (th.relationship(supplied, BuiltInAtomicType.STRING) ==
        Affinity.DISJOINT &&
        th.relationship(supplied, BuiltInAtomicType.UNTYPED_ATOMIC) ==
          Affinity.DISJOINT &&
        th.relationship(supplied, BuiltInAtomicType.QNAME) == Affinity.DISJOINT) {
        val de: XPathException = new XPathException(
          "The name of a constructed element must be a string, QName, or untypedAtomic")
        de.setErrorCode("XPTY0004")
        de.setIsTypeError(true)
        de.setLocation(getLocation)
        throw de
      }
    } else {
      if (!th.isSubType(getNameExp.getItemType, BuiltInAtomicType.STRING)) {
        setNameExp(SystemFunction.makeCall("string",
          getRetainedStaticContext,
          getNameExp))
      }
    }
    if (Literal.isAtomic(getNameExp)) {
      try {
        val `val`: AtomicValue =
          getNameExp.asInstanceOf[Literal].value.asInstanceOf[AtomicValue]
        if (`val`.isInstanceOf[StringValue]) {
          val parts: Array[String] =
            NameChecker.checkQNameParts(`val`.getStringValueCS)
          if (getNamespaceExp == null) {
            val prefix: String = parts(0)
            val uri: String =
              getNamespaceResolver.getURIForPrefix(prefix, true)
            if (uri == null) {
              val se: XPathException = new XPathException(
                "Prefix " + prefix + " has not been declared")
              se.setErrorCode("XPST0081")
              se.setIsStaticError(true)
              throw se
            }
            setNamespaceExp(new StringLiteral(uri))
          }
        }
      } catch {
        case e: XPathException => {
          val code: String = e.getErrorCodeLocalPart
          if (code == null || code.==("FORG0001")) {
            e.setErrorCode(if (isXSLT) "XTDE0820" else "XQDY0074")
          } else if (code.==("XPST0081")) {
            e.setErrorCode(if (isXSLT) "XTDE0830" else "XQDY0074")
          }
          e.maybeSetLocation(getLocation)
          e.setIsStaticError(true)
          throw e
        }

      }
    }
    super.typeCheck(visitor, contextInfo)
  }

  def copy(rebindings: RebindingMap): Expression = {
    val ce: ComputedElement = new ComputedElement(
      getNameExp.copy(rebindings),
      if (getNamespaceExp == null) null else getNamespaceExp.copy(rebindings),
      getSchemaType,
      getValidationAction,
      bequeathNamespacesToChildren,
      allowNameAsQName
    )
    ExpressionTool.copyLocationInfo(this, ce)
    ce.setContentExpression(getContentExpression.copy(rebindings))
    ce
  }

  override def getItemType(): ItemType = {
    if (itemType == null) {
      super.getItemType
    }
    itemType
  }

override  def checkPermittedContents(parentType: SchemaType, whole: Boolean): Unit = {
    if (parentType.isInstanceOf[SimpleType] ||
      parentType.asInstanceOf[ComplexType].isSimpleContent) {
      var msg: String =
        "Elements are not permitted here: the containing element "
      if (parentType.isInstanceOf[SimpleType]) {
        if (parentType.isAnonymousType) {
          msg += "is defined to have a simple type"
        } else {
          msg += "is of simple type " + parentType.getDescription
        }
      } else {
        msg += "has a complex type with simple content"
      }
      val err: XPathException = new XPathException(msg)
      err.setIsTypeError(true)
      err.setLocation(getLocation)
      throw err
    }
  }

  def getElementName(context: XPathContext, copiedNode: NodeInfo): NodeName = {
    val controller: Controller = context.getController
    assert(controller != null)
    var prefix: String = null
    var localName: String = null
    var uri: String = null
    val nameValue: AtomicValue =
      getNameExp.evaluateItem(context).asInstanceOf[AtomicValue]
    if (nameValue == null) {
      val errorCode: String = if (isXSLT) "XTDE0820" else "XPTY0004"
      val err1: XPathException = new XPathException(
        "Invalid element name (empty sequence)",
        errorCode,
        getLocation)
      throw dynamicError(getLocation.toString, err1.toString, context)
    }
    if (nameValue.isInstanceOf[StringValue]) {
      var rawName: CharSequence = nameValue.getStringValueCS
      rawName = Whitespace.trimWhitespace(rawName)
      try {
        val parts: Array[String] = NameChecker.getQNameParts(rawName)
        prefix = parts(0)
        localName = parts(1)
      } catch {
        case err: QNameException => {
          var message: String = "Invalid element name. " + err.getMessage
          if (rawName.length == 0) {
            message = "Supplied element name is a zero-length string"
          }
          val errorCode: String = if (isXSLT) "XTDE0820" else "XQDY0074"
          val err1: XPathException =
            new XPathException(message, errorCode, getLocation)
          throw dynamicError(getLocation.toString, err1.toString, context)
        }

      }
    } else if (nameValue.isInstanceOf[QNameValue] && allowNameAsQName) {
      localName = nameValue.asInstanceOf[QNameValue].getLocalName
      uri = nameValue.asInstanceOf[QNameValue].getNamespaceURI
      prefix = nameValue.asInstanceOf[QNameValue].getPrefix
      if (prefix.==("xmlns")) {
        val err: XPathException = new XPathException(
          "Computed element name has prefix xmlns",
          "XQDY0096",
          getLocation)
        throw dynamicError(getLocation.toString, err.toString, context)
      }
    } else {
      val errorCode: String = if (isXSLT) "XTDE0820" else "XPTY0004"
      val err: XPathException = new XPathException(
        "Computed element name has incorrect type",
        errorCode,
        getLocation)
      err.setIsTypeError(true)
      throw dynamicError(getLocation.toString, err.toString, context)
    }
    if (getNamespaceExp == null && uri == null) {
      uri = getRetainedStaticContext.getURIForPrefix(prefix, true).toString
      if (uri == null) {
        val errorCode: String =
          if (isXSLT) "XTDE0830"
          else if (prefix.==("xmlns")) "XQDY0096"
          else "XQDY0074"
        val err: XPathException = new XPathException(
          "Undeclared prefix in element name: " + prefix,
          errorCode,
          getLocation)
        throw dynamicError(getLocation.toString, err.toString, context)
      }
    } else {
      if (uri == null) {
        if (getNamespaceExp.isInstanceOf[StringLiteral]) {
          uri = getNamespaceExp.asInstanceOf[StringLiteral].getStringValue
        } else {
          uri = getNamespaceExp.evaluateAsString(context).toString
          if (!StandardURIChecker.getInstance.isValidURI(uri)) {
            val de: XPathException = new XPathException(
              "The value of the namespace attribute must be a valid URI",
              "XTDE0835",
              getLocation)
            throw dynamicError(getLocation.toString, de.toString, context)
          }
        }
      }
      if (uri.isEmpty) {
        prefix = ""
      }
      if (prefix.==("xmlns")) {
        prefix = "x-xmlns"
      }
    }
    if (uri == NamespaceConstant.XMLNS) {
      val errorCode: String = if (isXSLT) "XTDE0835" else "XQDY0096"
      val err: XPathException = new XPathException(
        "Cannot create element in namespace " + uri,
        errorCode,
        getLocation)
      throw dynamicError(getLocation.toString, err.toString, context)
    }
    if (uri == NamespaceConstant.XML != prefix.==("xml")) {
      var message: String = null
      message =
        if (prefix.==("xml"))
          "When the prefix is 'xml', the namespace URI must be " +
            NamespaceConstant.XML
        else
          "When the namespace URI is " + NamespaceConstant.XML +
            ", the prefix must be 'xml'"
      val errorCode: String = if (isXSLT) "XTDE0835" else "XQDY0096"
      val err: XPathException =
        new XPathException(message, errorCode, getLocation)
      throw dynamicError(getLocation.toString, err.toString, context)
    }
    new FingerprintedQName(prefix, uri, localName)
  }

  def getNewBaseURI(context: XPathContext, copiedNode: NodeInfo): String =
    getStaticBaseURIString

  def outputNamespaceNodes(out: Outputter,
                           nodeName: NodeName,
                           copiedNode: NodeInfo): Unit = {}

  override def getInstructionNameCode(): Int = StandardNames.XSL_ELEMENT

  def export(out: ExpressionPresenter): Unit = {
    out.startElement("compElem", this)
    var flags: String = getInheritanceFlags
    if (isLocal) {
      flags += "l"
    }
    if (!flags.isEmpty) {
      out.emitAttribute("flags", flags)
    }
    exportValidationAndType(out)
    out.setChildRole("name")
    getNameExp.export(out)
    if (getNamespaceExp != null) {
      out.setChildRole("namespace")
      getNamespaceExp.export(out)
    }
    out.setChildRole("content")
    getContentExpression.export(out)
    out.endElement()
  }

}
