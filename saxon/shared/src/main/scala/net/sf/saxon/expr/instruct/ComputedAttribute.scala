////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// Copyright (c) 2018-2020 Saxonica Limited
// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is "Incompatible With Secondary Licenses", as defined by the Mozilla Public License, v. 2.0.
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
package net.sf.saxon.expr.instruct

import net.sf.saxon.utils.Configuration
import net.sf.saxon.event.ReceiverOption
import net.sf.saxon.expr._
import net.sf.saxon.expr.parser._
import net.sf.saxon.functions.SystemFunction
import net.sf.saxon.lib.NamespaceConstant
import net.sf.saxon.lib.StandardURIChecker
import net.sf.saxon.lib.Validation
import net.sf.saxon.model._
import net.sf.saxon.om._
import net.sf.saxon.pattern.NodeKindTest
import net.sf.saxon.trace.ExpressionPresenter
import net.sf.saxon.trans.XPathException
import net.sf.saxon.tree.util.Orphan
import net.sf.saxon.value._

/**
 * An instruction derived from an xsl:attribute element in stylesheet, or from
 * an attribute constructor in XQuery, in cases where the attribute name is not known
 * statically
 */
final class ComputedAttribute(val attributeName: Expression, val namespace: Expression,
                              val nsContext: NamespaceResolver,  val validAction:
                              Int,  val schemaTyp: SimpleType, var allowNameAsQName: Boolean) extends AttributeCreator {

  nameOp = new Operand(this, attributeName, OperandRole.SINGLE_ATOMIC)
  if (namespace != null) namespaceOp = new Operand(this, namespace, OperandRole.SINGLE_ATOMIC)
  setSchemaType(schemaType)
  setValidationAction(validAction)
  setOptions(ReceiverOption.NONE)
  private var nameOp: Operand = null
  private var namespaceOp: Operand = null

  /**
   * Indicate that two attributes with the same name are not acceptable.
   * (This option is set in XQuery, but not in XSLT)
   */
  override def setRejectDuplicates() = setOptions(getOptions | ReceiverOption.REJECT_DUPLICATES)

  /**
   * Get the name of this instruction
   */
  override def getInstructionNameCode = StandardNames.XSL_ATTRIBUTE

  /**
   * Get the expression used to compute the name of the attribute
   *
   * @return the expression used to compute the name of the attribute
   */
  def getNameExp = nameOp.getChildExpression

  /**
   * Get the expression used to compute the namespace part of the name of the attribute
   *
   * @return the expression used to compute the namespace part of the name of the attribute
   */
  def getNamespaceExp = if (namespaceOp == null) null
  else namespaceOp.getChildExpression

  def setNameExp(attributeName: Expression) = nameOp.setChildExpression(attributeName)

  def setNamespace(namespace: Expression) = if (namespace != null) if (namespaceOp == null) namespaceOp = new Operand(this, namespace, OperandRole.SINGLE_ATOMIC)
  else namespaceOp.setChildExpression(namespace)

  override def operands = operandSparseList(selectOp, nameOp, namespaceOp)

  /**
   * Get the namespace resolver used to resolve any prefix in the name of the attribute
   *
   * @return the namespace resolver if one has been saved; or null otherwise
   */
  def getNamespaceResolver = getRetainedStaticContext

  /**
   * Get the static type of this expression
   *
   * @return the static type of the item returned by this expression
   */
  /*@NotNull*/ override def getItemType = NodeKindTest.ATTRIBUTE

  /**
   * Get the static cardinality of this expression
   *
   * @return the static cardinality (exactly one)
   */
  override def getCardinality = StaticProperty.ALLOWS_ZERO_OR_ONE

  /**
   * Ask whether it is allowed for the name to be evaluted as an xs:QName instance (true in XQuery)
   *
   * @return the boolean if name is allowed as a QName
   */
  def isAllowNameAsQName = allowNameAsQName

  /**
   * Get the static properties of this expression (other than its type). The result is
   * bit-signficant. These properties are used for optimizations. In general, if
   * property bit is set, it is true, but if it is unset, the value is unknown.
   *
   * @return a set of flags indicating static properties of this expression
   */
  override def computeSpecialProperties = super.computeSpecialProperties | StaticProperty.SINGLE_DOCUMENT_NODESET

  @throws[XPathException]
  override def localTypeCheck(visitor: ExpressionVisitor, contextItemType: ContextItemStaticInfo) = {
    nameOp.typeCheck(visitor, contextItemType)
    val role = new RoleDiagnostic(RoleDiagnostic.INSTRUCTION, "attribute/name", 0)
    val config = visitor.getConfiguration
    val th = config.getTypeHierarchy
    if (allowNameAsQName) { // Can only happen in XQuery
      setNameExp(config.getTypeChecker(false).staticTypeCheck(getNameExp, SequenceType.SINGLE_ATOMIC, role, visitor))
      val nameItemType = getNameExp.getItemType
      val maybeString = (th.relationship(nameItemType, BuiltInAtomicType.STRING) ne Affinity.DISJOINT) || (th.relationship(nameItemType, BuiltInAtomicType.UNTYPED_ATOMIC) ne Affinity.DISJOINT)
      val maybeQName = th.relationship(nameItemType, BuiltInAtomicType.QNAME) ne Affinity.DISJOINT
      if (!(maybeString || maybeQName)) {
        val err = new XPathException("The attribute name must be either an xs:string, an xs:QName, or untyped atomic")
        err.setErrorCode("XPTY0004")
        err.setIsTypeError(true)
        err.setLocation(getLocation)
        throw err
      }
    }
    else if (!th.isSubType(getNameExp.getItemType, BuiltInAtomicType.STRING))
      setNameExp(SystemFunction.makeCall("string", getRetainedStaticContext, getNameExp))
    if (getNamespaceExp != null) namespaceOp.typeCheck(visitor, contextItemType)
    if (Literal.isAtomic(getNameExp)) { // Check we have a valid lexical QName, whose prefix is in scope where necessary
      try {
        val `val` = getNameExp.asInstanceOf[Literal].getValue.asInstanceOf[AtomicValue]
        if (`val`.isInstanceOf[StringValue]) {
          val parts = NameChecker.checkQNameParts(`val`.getStringValueCS)
          if (getNamespaceExp == null) {
            val uri = getNamespaceResolver.getURIForPrefix(parts(0), useDefault = false)
            if (uri == null) {
              val se = new XPathException("Prefix " + parts(0) + " has not been declared")
              if (isXSLT) {
                se.setErrorCode("XTDE0860")
                se.setIsStaticError(true)
                throw se
              }
              else {
                se.setErrorCode("XQDY0074")
                se.setIsStaticError(false)
                throw se
              }
            }
            setNamespace(new StringLiteral(uri))
          }
        }
      } catch {
        case e: XPathException =>
          if (e.getErrorCodeQName == null || e.getErrorCodeLocalPart == "FORG0001") e.setErrorCode(if (isXSLT) "XTDE0850"
          else "XQDY0074")
          e.maybeSetLocation(getLocation)
          e.setIsStaticError(true)
          throw e
      }
    }
  }

  @throws[XPathException]
  override def optimize(visitor: ExpressionVisitor, contextItemType: ContextItemStaticInfo): Expression = {
    val exp = super.optimize(visitor, contextItemType)
    if (exp ne this) return exp
    // If the name is known statically, use a FixedAttribute instead
    if (getNameExp.isInstanceOf[Literal] && (getNamespaceExp == null || getNamespaceExp.isInstanceOf[Literal])) {
      val context = visitor.getStaticContext.makeEarlyEvaluationContext
      val nc = evaluateNodeName(context)
      val fa = new FixedAttribute(nc, getValidationAction, getSchemaType)
      fa.setSelect(getSelect)
      return fa
    }
    this
  }

  /**
   * Copy an expression. This makes a deep copy.
   *
   * @return the copy of the original expression
   * @param rebindings a mutable list of (old binding, new binding) pairs
   *                   that is used to update the bindings held in any
   *                   local variable references that are copied.
   */
  override def copy(rebindings: RebindingMap) = {
    val exp = new ComputedAttribute(if (getNameExp == null) null
    else getNameExp.copy(rebindings), if (getNamespaceExp == null) null
    else getNamespaceExp.copy(rebindings), getRetainedStaticContext, getValidationAction, getSchemaType, allowNameAsQName)
    ExpressionTool.copyLocationInfo(this, exp)
    exp.setSelect(getSelect.copy(rebindings))
    exp.setInstruction(isInstruction)
    exp
  }

  /**
   * Check that any elements and attributes constructed or returned by this expression are acceptable
   * in the content model of a given complex type. It's always OK to say yes, since the check will be
   * repeated at run-time. The process of checking element and attribute constructors against the content
   * model of a complex type also registers the type of content expected of those constructors, so the
   * static validation can continue recursively.
   */
  @throws[XPathException]
  override def checkPermittedContents(parentType: SchemaType, whole: Boolean) = if (parentType.isInstanceOf[SimpleType]) {
    var msg = "Attributes are not permitted here: "
    if (parentType.isAnonymousType) msg += "the containing element is defined to have a simple type"
    else msg += "the containing element is of simple type " + parentType.getDescription
    val err = new XPathException(msg)
    err.setIsTypeError(true)
    err.setLocation(getLocation)
    throw err
  }

  /**
   * Determine the name to be used for the attribute, as an integer name code
   *
   * @param context Dynamic evaluation context
   * @return the integer name code for the attribute name
   * @throws XPathException if a dynamic error occurs (for example, if the attribute name is invalid)
   */
  @throws[XPathException]
  override def evaluateNodeName(context: XPathContext) = {
    val pool = context.getNamePool
    val nameValue = getNameExp.evaluateItem(context)
    var prefix: String = null
    var localName: String = null
    var uri: String = null
    if (nameValue.isInstanceOf[StringValue]) { // this will always be the case in XSLT
      var rawName = nameValue.getStringValueCS
      rawName = Whitespace.trimWhitespace(rawName) // required in XSLT; possibly wrong in XQuery
      try {
        val parts = NameChecker.getQNameParts(rawName)
        prefix = parts(0)
        localName = parts(1)
      } catch {
        case err: QNameException =>
          val errorCode = if (isXSLT) "XTDE0850"
          else "XQDY0074"
          val err1 = new XPathException("Invalid attribute name: " + rawName, errorCode, this.getLocation)
          throw dynamicError(getLocation.toString, err1.toString, context)
      }
      if (rawName.toString == "xmlns") if (getNamespaceExp == null) {
        val errorCode = if (isXSLT) "XTDE0855"
        else "XQDY0044"
        val err = new XPathException("Invalid attribute name: " + rawName, errorCode, this.getLocation)
        throw dynamicError(getLocation.toString, err.toString, context)
      }
      if (prefix == "xmlns") if (getNamespaceExp == null) {
        val errorCode = if (isXSLT) "XTDE0860"
        else "XQDY0044"
        val err = new XPathException("Invalid attribute name: " + rawName, errorCode, this.getLocation)
        throw dynamicError(getLocation.toString, err.toString, context)
      }
      else { // ignore the prefix "xmlns"
        prefix = ""
      }
    }
    else if (nameValue.isInstanceOf[QNameValue] && allowNameAsQName) { // this is allowed in XQuery
      localName = nameValue.asInstanceOf[QNameValue].getLocalName
      uri = nameValue.asInstanceOf[QNameValue].getNamespaceURI
      if (localName == "xmlns" && uri.isEmpty) {
        val err = new XPathException("Invalid attribute name: xmlns", "XQDY0044", this.getLocation)
        throw dynamicError(getLocation.toString, err.toString, context)
      }
      if (uri.isEmpty) prefix = ""
      else {
        prefix = nameValue.asInstanceOf[QNameValue].getPrefix
        if (prefix.isEmpty) {
          prefix = pool.suggestPrefixForURI(uri)
          if (prefix == null) {
            prefix = "ns0"
            // If the prefix is a duplicate, a different one will be substituted
          }
        }
        if (uri == NamespaceConstant.XML != "xml" == prefix) {
          var message: String = null
          if ("xml" == prefix) message = "When the prefix is 'xml', the namespace URI must be " + NamespaceConstant.XML
          else message = "When the namespace URI is " + NamespaceConstant.XML + ", the prefix must be 'xml'"
          val errorCode = if (isXSLT) "XTDE0835"
          else "XQDY0044"
          val err = new XPathException(message, errorCode, this.getLocation)
          throw dynamicError(getLocation.toString, err.toString, context)
        }
      }
      if ("xmlns" == prefix) {
        val err = new XPathException("Invalid attribute namespace: http://www.w3.org/2000/xmlns/", "XQDY0044", this.getLocation)
        throw dynamicError(getLocation.toString, err.toString, context)
      }
    }
    else {
      val err = new XPathException("Attribute name must be either a string or a QName", "XPTY0004", this.getLocation)
      err.setIsTypeError(true)
      throw dynamicError(getLocation.toString, err.toString, context)
    }
    if (getNamespaceExp == null && uri == null) if (prefix.isEmpty) uri = ""
    else {
      uri = getRetainedStaticContext.getURIForPrefix(prefix, useDefault = false)
      if (uri == null) {
        val errorCode = if (isXSLT) "XTDE0860"
        else "XQDY0074"
        val err = new XPathException("Undeclared prefix in attribute name: " + prefix, errorCode, this.getLocation)
        throw dynamicError(getLocation.toString, err.toString, context)
      }
    }
    else {
      if (uri == null) { // generate a name using the supplied namespace URI
        if (getNamespaceExp.isInstanceOf[StringLiteral]) uri = getNamespaceExp.asInstanceOf[StringLiteral].getStringValue
        else {
          uri = getNamespaceExp.evaluateAsString(context).toString
          if (!StandardURIChecker.getInstance.isValidURI(uri)) {
            val de = new XPathException("The value of the namespace attribute must be a valid URI", "XTDE0865", this.getLocation)
            throw dynamicError(getLocation.toString, de.toString, context)
          }
        }
      }
      if (uri.isEmpty) { // there is a special rule for this case in the XSLT specification;
        // we force the attribute to go in the null namespace
        prefix = ""
      }
      else { // if a suggested prefix is given, use it; otherwise try to find a prefix
        // associated with this URI; if all else fails, invent one.
        if (prefix.isEmpty) {
          prefix = pool.suggestPrefixForURI(uri)
          if (prefix == null) {
            prefix = "ns0"
            // this will be replaced later if it is already in use
          }
        }
      }
    }
    if (uri == NamespaceConstant.XMLNS) {
      val errorCode = if (isXSLT) "XTDE0865"
      else "XQDY0044"
      val err = new XPathException("Cannot create attribute in namespace " + uri, errorCode, this.getLocation)
      throw dynamicError(getLocation.toString, err.toString, context)
    }
    new FingerprintedQName(prefix, uri, localName)
  }

  @throws[XPathException]
  override def evaluateItem(context: XPathContext) = {
    val node = super.evaluateItem(context)
    validateOrphanAttribute(node.asInstanceOf[Orphan], context)
    node
  }

  /**
   * Diagnostic print of expression structure. The abstract expression tree
   * is written to the supplied output destination.
   */
  @throws[XPathException]
  override def `export`(out: ExpressionPresenter) = {
    out.startElement("compAtt", this)
    if (getValidationAction != Validation.SKIP) out.emitAttribute("validation", Validation.toString(getValidationAction))
    val `type` = getSchemaType
    if (`type` != null) out.emitAttribute("type", `type`.getStructuredQName)
    var flags = ""
    if (isLocal) flags += "l"
    if (!flags.isEmpty) out.emitAttribute("flags", flags)
    out.setChildRole("name")
    getNameExp.`export`(out)
    if (getNamespaceExp != null) {
      out.setChildRole("namespace")
      getNamespaceExp.`export`(out)
    }
    out.setChildRole("select")
    getSelect.`export`(out)
    out.endElement
  }
}