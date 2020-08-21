package net.sf.saxon.expr.instruct

import net.sf.saxon.utils.Configuration

import net.sf.saxon.event.Outputter

import net.sf.saxon.event.ReceiverOption

import net.sf.saxon.expr._

import net.sf.saxon.expr.parser._

import net.sf.saxon.lib.Validation

import net.sf.saxon.model._

import net.sf.saxon.om._

import net.sf.saxon.pattern.CombinedNodeTest

import net.sf.saxon.pattern.ContentTypeTest

import net.sf.saxon.pattern.NameTest

import net.sf.saxon.pattern.NodeKindTest

import net.sf.saxon.trace.ExpressionPresenter

import net.sf.saxon.trans.XPathException

import net.sf.saxon.tree.util.FastStringBuffer

import java.util.function.BiConsumer

import scala.beans.{BeanProperty, BooleanBeanProperty}

import scala.jdk.CollectionConverters._
import scala.util.control.Breaks._

class FixedElement(@BeanProperty var elementName: NodeName,
                   var namespaceBindings: NamespaceMap,
                   inheritNamespacesToChildren: Boolean,
                   inheritNamespacesFromParent: Boolean,
                   schemaType: SchemaType,
                   validation: Int)
  extends ElementCreator {

  private var itemType: ItemType = _

  var infParent = inheritNamespacesFromParent

  this.bequeathNamespacesToChildren = inheritNamespacesToChildren

  this.infParent = inheritNamespacesFromParent

  setValidationAction(validation, schemaType)

  preservingTypes = schemaType == null && validation == Validation.PRESERVE

  override def operands(): java.lang.Iterable[Operand] = contentOp

  override def simplify(): Expression = {
    preservingTypes |= !getPackageData.isSchemaAware
    super.simplify()
  }

  override def checkContentSequence(env: StaticContext): Unit = {
    super.checkContentSequence(env)
    itemType = computeFixedElementItemType(this,
      env,
      getValidationAction,
      getSchemaType,
      elementName,
      getContentExpression)
  }

  override def optimize(visitor: ExpressionVisitor,
                        contextItemType: ContextItemStaticInfo): Expression = {
    val e: Expression = super.optimize(visitor, contextItemType)
    if (e != this) {
      return e
    }
    if (!bequeathNamespacesToChildren) {
      return this
    }
    this
  }

  private def removeRedundantNamespaces(
                                         visitor: ExpressionVisitor,
                                         parentNamespaces: NamespaceMap): Unit = {
    if (namespaceBindings.isEmpty.asInstanceOf[Boolean]) {
      return
    }
    val th: TypeHierarchy = visitor.getConfiguration.getTypeHierarchy
    val contentType: ItemType = getContentExpression.getItemType
    var ok: Boolean = th.relationship(contentType, NodeKindTest.ATTRIBUTE) ==
      Affinity.DISJOINT
    if (!ok) {
      if (getContentExpression.isInstanceOf[Block]) {
        ok = true
        breakable {
          for (o <- getContentExpression.operands().asScala) {
            val exp: Expression = o.getChildExpression
            if (exp.isInstanceOf[FixedAttribute]) {
              if (!exp
                .asInstanceOf[FixedAttribute]
                .getAttributeName
                .hasURI("")) {
                ok = false
                break()
              }
            } else {
              val childType: ItemType = exp.getItemType
              if (th.relationship(childType, NodeKindTest.ATTRIBUTE) != Affinity.DISJOINT) {
                ok = false
                break()
              }
            }
          }
        }
      }
    }
    if (ok) {
      var reduced: NamespaceMap = namespaceBindings
      for (childNamespace <- namespaceBindings.asScala
           if childNamespace.getURI == parentNamespaces.getURI(
             childNamespace.getPrefix)) {
        reduced = reduced.remove(childNamespace.getPrefix)
      }
      namespaceBindings = reduced
    }
  }

  def copy(rebindings: RebindingMap): Expression = {
    val fe: FixedElement = new FixedElement(elementName,
      namespaceBindings,
      bequeathNamespacesToChildren,
      inheritNamespacesFromParent,
      getSchemaType,
      getValidationAction)
    fe.setContentExpression(getContentExpression.copy(rebindings))
    ExpressionTool.copyLocationInfo(this, fe)
    fe
  }

  private def computeFixedElementItemType(instr: FixedElement,
                                          env: StaticContext,
                                          validation: Int,
                                          schemaType: SchemaType,
                                          elementName: NodeName,
                                          content: Expression): ItemType = {
    val config: Configuration = env.getConfiguration
    var itemType: ItemType = null
    val fp: Int = elementName.obtainFingerprint(config.getNamePool)
    var schTyp = schemaType
    if (schTyp == null) {
      if (validation == Validation.STRICT) {
        val decl: SchemaDeclaration = config.getElementDeclaration(fp)
        if (decl == null) {
          val err: XPathException = new XPathException(
            "There is no global element declaration for " + elementName.getStructuredQName.getEQName +
              ", so strict validation will fail")
          err.setErrorCode(if (instr.isXSLT) "XTTE1512" else "XQDY0084")
          err.setIsTypeError(true)
          err.setLocation(instr.getLocation)
          throw err
        }
        if (decl.isAbstract) {
          val err: XPathException = new XPathException(
            "The element declaration for " + elementName.getStructuredQName.getEQName +
              " is abstract, so strict validation will fail")
          err.setErrorCode(if (instr.isXSLT) "XTTE1512" else "XQDY0027")
          err.setIsTypeError(true)
          err.setLocation(instr.getLocation)
          throw err
        }
        val declaredType: SchemaType = decl.getType
        val xsiType: SchemaType = instr.getXSIType(env)
        schTyp = if (xsiType != null) xsiType else declaredType
        itemType = new CombinedNodeTest(
          new NameTest(Type.ELEMENT, fp, env.getConfiguration.getNamePool),
          Token.INTERSECT,
          new ContentTypeTest(Type.ELEMENT, schTyp, config, false))
        if (xsiType != null || !decl.hasTypeAlternatives()) {
          instr.getValidationOptions.setTopLevelType(schTyp)
          try schTyp.analyzeContentExpression(content, Type.ELEMENT)
          catch {
            case e: XPathException => {
              e.setErrorCode(if (instr.isXSLT) "XTTE1510" else "XQDY0027")
              e.setLocation(instr.getLocation)
              throw e
            }

          }
          if (xsiType != null) {
            try config.checkTypeDerivationIsOK(xsiType, declaredType, 0)
            catch {
              case e: SchemaException => {
                val ve: ValidationFailure = new ValidationFailure(
                  "The specified xsi:type " + xsiType.getDescription + " is not validly derived from the required type " +
                    declaredType.getDescription)
                ve.setConstraintReference(1, "cvc-elt", "4.3")
                ve.setErrorCode(if (instr.isXSLT) "XTTE1515" else "XQDY0027")
                ve.setLocator(instr.getLocation)
                throw ve.makeException()
              }

            }
          }
        }
      } else if (validation == Validation.LAX) {
        val decl: SchemaDeclaration = config.getElementDeclaration(fp)
        if (decl == null) {
          env.issueWarning(
            "There is no global element declaration for " + elementName.getDisplayName,
            instr.getLocation)
          itemType = new NameTest(Type.ELEMENT, fp, config.getNamePool)
        } else {
          schTyp = decl.getType
          instr.getValidationOptions.setTopLevelType(schTyp)
          itemType = new CombinedNodeTest(
            new NameTest(Type.ELEMENT, fp, config.getNamePool),
            Token.INTERSECT,
            new ContentTypeTest(Type.ELEMENT,
              instr.getSchemaType,
              config,
              false))
          try schTyp.analyzeContentExpression(content, Type.ELEMENT)
          catch {
            case e: XPathException => {
              e.setErrorCode(if (instr.isXSLT) "XTTE1515" else "XQDY0027")
              e.setLocation(instr.getLocation)
              throw e
            }

          }
        }
      } else
        itemType =
          if (validation == Validation.PRESERVE)
            new CombinedNodeTest(
              new NameTest(Type.ELEMENT, fp, config.getNamePool),
              Token.INTERSECT,
              new ContentTypeTest(Type.ELEMENT,
                AnyType.getInstance,
                config,
                false))
          else
            new CombinedNodeTest(
              new NameTest(Type.ELEMENT, fp, config.getNamePool),
              Token.INTERSECT,
              new ContentTypeTest(Type.ELEMENT,
                Untyped.getInstance,
                config,
                false))
    } else {
      itemType = new CombinedNodeTest(
        new NameTest(Type.ELEMENT, fp, config.getNamePool),
        Token.INTERSECT,
        new ContentTypeTest(Type.ELEMENT, schTyp, config, false))
      try schTyp.analyzeContentExpression(content, Type.ELEMENT)
      catch {
        case e: XPathException => {
          e.setErrorCode(if (instr.isXSLT) "XTTE1540" else "XQDY0027")
          e.setLocation(instr.getLocation)
          throw e
        }

      }
    }
    itemType
  }

  override def getItemType(): ItemType = {
    if (itemType == null) {
      super.getItemType
    }
    itemType
  }

  def getElementName(context: XPathContext, copiedNode: NodeInfo): NodeName =
    elementName

  override def gatherProperties(consumer: BiConsumer[String, Any]): Unit = {
    consumer.accept("name", getElementName)
  }

  def getNewBaseURI(context: XPathContext, copiedNode: NodeInfo): String =
    getStaticBaseURIString

  private def getXSIType(env: StaticContext): SchemaType =
    if (getContentExpression.isInstanceOf[FixedAttribute]) {
      testForXSIType(getContentExpression.asInstanceOf[FixedAttribute], env)
    } else if (getContentExpression.isInstanceOf[Block]) {
      for (o <- getContentExpression.operands().asScala) {
        val exp: Expression = o.getChildExpression
        if (exp.isInstanceOf[FixedAttribute]) {
          val `type`: SchemaType =
            testForXSIType(exp.asInstanceOf[FixedAttribute], env)
          if (`type` != null) {
            `type`
          }
        }
      }
      null
    } else {
      null
    }

  private def testForXSIType(fat: FixedAttribute,
                             env: StaticContext): SchemaType = {
    val att: Int = fat.getAttributeFingerprint
    if (att == StandardNames.XSI_TYPE) {
      val attValue: Expression = fat.getSelect
      if (attValue.isInstanceOf[StringLiteral]) {
        val parts: Array[String] = NameChecker.getQNameParts(
          attValue.asInstanceOf[StringLiteral].getStringValue)
        val uri: String = namespaceBindings.getURI(parts(0))
        if (uri == null) {
          return null
        } else {
          env.getConfiguration.getSchemaType(
            new StructuredQName("", uri, parts(1)))
        }
      }
    }
    null
  }

  override def checkPermittedContents(parentType: SchemaType, whole: Boolean): Unit = {
    if (parentType.isInstanceOf[SimpleType]) {
      val err: XPathException = new XPathException(
        "Element " + elementName.getDisplayName +
          " is not permitted here: the containing element is of simple type " +
          parentType.getDescription)
      err.setIsTypeError(true)
      err.setLocation(getLocation)
      throw err
    } else if (parentType.asInstanceOf[ComplexType].isSimpleContent) {
      val err: XPathException = new XPathException(
        "Element " + elementName.getDisplayName +
          " is not permitted here: the containing element has a complex type with simple content")
      err.setIsTypeError(true)
      err.setLocation(getLocation)
      throw err
    }
    if (whole) {
      val parent: Expression = getParentExpression
      val block: Block = new Block(Array(this))
      parentType.analyzeContentExpression(block, Type.ELEMENT)
      this.parentExpression = parent
    }
    var `type`: SchemaType = null
    val fp: Int = elementName.obtainFingerprint(getConfiguration.getNamePool)
    `type` =
      parentType.asInstanceOf[ComplexType].getElementParticleType(fp, true)
    if (`type` == null) {
      val err: XPathException = new XPathException(
        "Element " + elementName.getDisplayName +
          " is not permitted in the content model of the complex type " +
          parentType.getDescription)
      err.setIsTypeError(true)
      err.setLocation(getLocation)
      err.setErrorCode(if (isXSLT) "XTTE1510" else "XQDY0027")
      throw err
    }
    if (`type`.isInstanceOf[AnyType.AnyType]) {
      return
    }
    try getContentExpression.checkPermittedContents(`type`, true)
    catch {
      case e: XPathException => {
        e.maybeSetLocation(getLocation)
        throw e
      }

    }
  }

  def outputNamespaceNodes(out: Outputter,
                           nodeName: NodeName,
                           copiedNode: NodeInfo): Unit = {
    for (ns <- namespaceBindings.asScala) {
      out.namespace(ns.getPrefix, ns.getURI, ReceiverOption.NONE)
    }
  }

  def getActiveNamespaces(): NamespaceMap = namespaceBindings

  def export(out: ExpressionPresenter): Unit = {
    out.startElement("elem", this)
    out.emitAttribute("name", elementName.getDisplayName)
    out.emitAttribute("nsuri", elementName.getURI)
    var flags: String = getInheritanceFlags
    if (!elementName.getURI.isEmpty && elementName.getPrefix.isEmpty) {
      flags += "d"
    }
    if (isLocal) {
      flags += "l"
    }
    if (!flags.isEmpty) {
      out.emitAttribute("flags", flags)
    }
    val fsb: FastStringBuffer = new FastStringBuffer(FastStringBuffer.C256)
    if (!namespaceBindings.isEmpty.asInstanceOf[Boolean]) {
      for (ns <- namespaceBindings.asScala) {
        val prefix: String = ns.getPrefix
        if (prefix.!=("xml")) {
          fsb.append(if (prefix.isEmpty) "#" else prefix)
          if (ns.getURI != getRetainedStaticContext.getURIForPrefix(prefix,
            true)) {
            fsb.cat('=')
            fsb.append(ns.getURI)
          }
          fsb.cat(' ')
        }
      }
      fsb.setLength(fsb.length - 1)
      out.emitAttribute("namespaces", fsb.toString)
    }
    exportValidationAndType(out)
    getContentExpression.export(out)
    out.endElement()
  }

  override def toString(): String =
    "<" + elementName.getStructuredQName.getDisplayName +
      " {" +
      getContentExpression.toString +
      "}/>"

  override def toShortString(): String =
    "<" + elementName.getStructuredQName.getDisplayName +
      " {" +
      getContentExpression.toShortString() +
      "}/>"

  override def getExpressionName(): String = "element"

}
