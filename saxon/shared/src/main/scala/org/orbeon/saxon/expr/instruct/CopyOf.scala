package org.orbeon.saxon.expr.instruct

import org.orbeon.saxon.utils.Configuration
import org.orbeon.saxon.expr.Expression._

import org.orbeon.saxon.utils.Controller

import org.orbeon.saxon.event._

import org.orbeon.saxon.expr._

import org.orbeon.saxon.expr.parser._

import org.orbeon.saxon.lib.NamespaceConstant

import org.orbeon.saxon.lib.ParseOptions

import org.orbeon.saxon.lib.Validation

import org.orbeon.saxon.model._

import org.orbeon.saxon.om._

import org.orbeon.saxon.pattern.AnyNodeTest

import org.orbeon.saxon.pattern.ContentTypeTest

import org.orbeon.saxon.pattern.NodeKindTest

import org.orbeon.saxon.pattern.NodeTest

import org.orbeon.saxon.s9api.HostLanguage

import org.orbeon.saxon.trace.ExpressionPresenter

import org.orbeon.saxon.trans.XPathException

import org.orbeon.saxon.tree.tiny.TinyBuilder

import org.orbeon.saxon.tree.tiny.TinyNodeImpl

import org.orbeon.saxon.tree.util.FastStringBuffer

import org.orbeon.saxon.tree.wrapper.VirtualCopy

import org.orbeon.saxon.tree.wrapper.VirtualUntypedCopy

import org.orbeon.saxon.value.SequenceType

import java.net.URI

import java.net.URISyntaxException

import CopyOf._

import scala.beans.{BeanProperty, BooleanBeanProperty}

object CopyOf {

  def computeNewBaseUri(source: NodeInfo, staticBaseURI: String): String = {
    var newBaseUri: String = null
    val xmlBase: String =
      source.getAttributeValue(NamespaceConstant.XML, "base")
    if (xmlBase != null) {
      try {
        val xmlBaseUri: URI = new URI(xmlBase)
        if (xmlBaseUri.isAbsolute) {
          newBaseUri = xmlBase
        } else if (staticBaseURI != null) {
          val sbu: URI = new URI(staticBaseURI)
          val abs: URI = sbu.resolve(xmlBaseUri)
          newBaseUri = abs.toString
        } else {
          newBaseUri = source.getBaseURI
        }
      } catch {
        case err: URISyntaxException => newBaseUri = source.getBaseURI

      }
    } else {
      newBaseUri = source.getBaseURI
    }
    newBaseUri
  }

  def copyAttribute(source: NodeInfo,
                    schemaType: SimpleType,
                    validation: Int,
                    instruction: Instruction,
                    output: Outputter,
                    context: XPathContext,
                    rejectDuplicates: Boolean): Unit = {
    val opt: Int =
      if (rejectDuplicates) ReceiverOption.REJECT_DUPLICATES
      else ReceiverOption.NONE
    val value: CharSequence = source.getStringValueCS
    val annotation: SimpleType =
      validateAttribute(source, schemaType, validation, context)
    try output.attribute(NameOfNode.makeName(source),
      annotation,
      value,
      instruction.getLocation,
      opt)
    catch {
      case e: XPathException => {
        e.maybeSetContext(context)
        e.maybeSetLocation(instruction.getLocation)
        if (instruction.getPackageData.getHostLanguage == HostLanguage.XQUERY &&
          e.getErrorCodeLocalPart.==("XTTE0950")) {
          e.setErrorCode("XQTY0086")
        }
        throw e
      }

    }
  }

  def validateAttribute(source: NodeInfo,
                        schemaType: SimpleType,
                        validation: Int,
                        context: XPathContext): SimpleType = {
    val value: CharSequence = source.getStringValueCS
    var annotation: SimpleType = BuiltInAtomicType.UNTYPED_ATOMIC
    if (schemaType != null) {
      if (schemaType.isNamespaceSensitive) {
        val err = new XPathException(
          "Cannot create a parentless attribute whose " + "type is namespace-sensitive (such as xs:QName)")
        err.setErrorCode("XTTE1545")
        throw err
      }
      val err: ValidationFailure = schemaType.validateContent(
        value,
        DummyNamespaceResolver.getInstance,
        context.getConfiguration.getConversionRules)
      if (err != null) {
        err.setMessage(
          "Attribute being copied does not match the required type. " +
            err.getMessage)
        err.setErrorCode("XTTE1510")
        throw err.makeException()
      }
      annotation = schemaType
    } else if (validation == Validation.STRICT || validation == Validation.LAX) {
      try annotation = context.getConfiguration.validateAttribute(
        NameOfNode.makeName(source).getStructuredQName,
        value,
        validation)
      catch {
        case e: ValidationException => {
          val err = XPathException.makeXPathException(e)
          err.setErrorCodeQName(e.getErrorCodeQName)
          err.setIsTypeError(true)
          throw err
        }

      }
    } else if (validation == Validation.PRESERVE) {
      annotation = source.getSchemaType.asInstanceOf[SimpleType]
      if (annotation != BuiltInAtomicType.UNTYPED_ATOMIC && annotation.isNamespaceSensitive) {
        val err = new XPathException(
          "Cannot preserve type annotation when copying an attribute with namespace-sensitive content")
        err.setErrorCode(
          if (context.getController.getExecutable.getHostLanguage ==
            HostLanguage.XSLT) "XTTE0950"
          else "XQTY0086")
        err.setIsTypeError(true)
        throw err
      }
    }
    annotation
  }

}

class CopyOf(select: Expression,
             @BooleanBeanProperty var copyNamespaces: Boolean,
             private var validation: Int,
             @BeanProperty var schemaType: SchemaType,
             private var rejectDuplicateAttributes: Boolean)
  extends Instruction
    with ValidatingInstruction {

  private var selectOp: Operand =
    new Operand(this, select, OperandRole.SINGLE_ATOMIC)

  @BooleanBeanProperty
  var copyAccumulators: Boolean = _

  private var requireDocumentOrElement: Boolean = false

  @BooleanBeanProperty
  var validating: Boolean = schemaType != null || validation == Validation.STRICT ||
    validation == Validation.LAX

  private var copyLineNumbers: Boolean = false

  @BooleanBeanProperty
  var copyForUpdate: Boolean = false

  private var isSchemaAware: Boolean = true

  def getSelect: Expression = selectOp.getChildExpression

  def setSelect(select: Expression): Unit = {
    selectOp.setChildExpression(select)
  }

  override def operands: java.lang.Iterable[Operand] = selectOp

  def getValidationAction(): Int = validation

  def setSchemaAware(schemaAware: Boolean): Unit = {
    this.isSchemaAware = schemaAware
  }

  def setCopyLineNumbers(copy: Boolean): Unit = {
    copyLineNumbers = copy
  }

  override def mayCreateNewNodes(): Boolean = !getSelect.getItemType.isPlainType

  override def getInstructionNameCode(): Int = StandardNames.XSL_COPY_OF

  def setRequireDocumentOrElement(requireDocumentOrElement: Boolean): Unit = {
    this.requireDocumentOrElement = requireDocumentOrElement
  }

  def isDocumentOrElementRequired: Boolean = requireDocumentOrElement

  override def getImplementationMethod: Int =
    ITERATE_METHOD | PROCESS_METHOD | WATCH_METHOD

  def copy(rebindings: RebindingMap): Expression = {
    val c: CopyOf = new CopyOf(getSelect.copy(rebindings),
      copyNamespaces,
      validation,
      schemaType,
      rejectDuplicateAttributes)
    ExpressionTool.copyLocationInfo(this, c)
    c.setCopyForUpdate(copyForUpdate)
    c.setCopyLineNumbers(copyLineNumbers)
    c.isSchemaAware = isSchemaAware
    c.setCopyAccumulators(copyAccumulators)
    c
  }

  override def getItemType: ItemType = {
    val in: ItemType = getSelect.getItemType
    if (!isSchemaAware) {
      return in
    }
    val config: Configuration = getConfiguration
    if (schemaType != null) {
      val th = config.getTypeHierarchy
      val e: Affinity.Affinity = th.relationship(in, NodeKindTest.ELEMENT)
      if (e == Affinity.SAME_TYPE || e == Affinity.SUBSUMED_BY) {
        new ContentTypeTest(Type.ELEMENT, schemaType, config, false)
      }
      val a: Affinity.Affinity = th.relationship(in, NodeKindTest.ATTRIBUTE)
      if (a == Affinity.SAME_TYPE || a == Affinity.SUBSUMED_BY) {
        new ContentTypeTest(Type.ATTRIBUTE, schemaType, config, false)
      }
    } else {
      validation match {
        case Validation.PRESERVE => return in
        case Validation.STRIP => {
          val th = config.getTypeHierarchy
          val e: Affinity.Affinity = th.relationship(in, NodeKindTest.ELEMENT)
          if (e == Affinity.SAME_TYPE || e == Affinity.SUBSUMED_BY) {
            new ContentTypeTest(Type.ELEMENT,
              Untyped.getInstance,
              config,
              false)
          }
          val a: Affinity.Affinity = th.relationship(in, NodeKindTest.ATTRIBUTE)
          if (a == Affinity.SAME_TYPE || a == Affinity.SUBSUMED_BY) {
            new ContentTypeTest(Type.ATTRIBUTE,
              BuiltInAtomicType.UNTYPED_ATOMIC,
              config,
              false)
          }
          if (e != Affinity.DISJOINT || a != Affinity.DISJOINT) {
            if (in.isInstanceOf[NodeTest]) AnyNodeTest.getInstance
            else AnyItemType
          } else {
            return in
          }
        }
        case Validation.STRICT | Validation.LAX =>
          if (in.isInstanceOf[NodeTest]) {
            val th = config.getTypeHierarchy
            val fp: Int = in.asInstanceOf[NodeTest].getFingerprint
            if (fp != -1) {
              val e: Affinity.Affinity = th.relationship(in, NodeKindTest.ELEMENT)
              if (e == Affinity.SAME_TYPE || e == Affinity.SUBSUMED_BY) {
                val elem: SchemaDeclaration = config.getElementDeclaration(fp)
                if (elem != null) {
                  try new ContentTypeTest(Type.ELEMENT,
                    elem.getType,
                    config,
                    false)
                  catch {
                    case e1: MissingComponentException =>
                      new ContentTypeTest(Type.ELEMENT,
                        AnyType.getInstance,
                        config,
                        false)

                  }
                } else {
                  new ContentTypeTest(Type.ELEMENT,
                    AnyType.getInstance,
                    config,
                    false)
                }
              }
              val a: Affinity.Affinity = th.relationship(in, NodeKindTest.ATTRIBUTE)
              if (a == Affinity.SAME_TYPE || a == Affinity.SUBSUMED_BY) {
                val attr: SchemaDeclaration = config.getElementDeclaration(fp)
                if (attr != null) {
                  try new ContentTypeTest(Type.ATTRIBUTE,
                    attr.getType,
                    config,
                    false)
                  catch {
                    case e1: MissingComponentException =>
                      new ContentTypeTest(Type.ATTRIBUTE,
                        AnySimpleType,
                        config,
                        false)

                  }
                } else {
                  new ContentTypeTest(Type.ATTRIBUTE,
                    AnySimpleType,
                    config,
                    false)
                }
              }
            } else {
              val e: Affinity.Affinity = th.relationship(in, NodeKindTest.ELEMENT)
              if (e == Affinity.SAME_TYPE || e == Affinity.SUBSUMED_BY) {
                NodeKindTest.ELEMENT
              }
              val a: Affinity.Affinity = th.relationship(in, NodeKindTest.ATTRIBUTE)
              if (a == Affinity.SAME_TYPE || a == Affinity.SUBSUMED_BY) {
                NodeKindTest.ATTRIBUTE
              }
            }
            AnyNodeTest.getInstance
          } else if (in.isInstanceOf[AtomicType]) {
            return in
          } else {
            AnyItemType
          }

      }
    }
    getSelect.getItemType
  }

  override def getCardinality(): Int = getSelect.getCardinality

  override def getDependencies(): Int = getSelect.getDependencies

  override def typeCheck(visitor: ExpressionVisitor,
                         contextInfo: ContextItemStaticInfo): Expression = {
    typeCheckChildren(visitor, contextInfo)
    if (isDocumentOrElementRequired) {
      val role: RoleDiagnostic =
        new RoleDiagnostic(RoleDiagnostic.TYPE_OP, "validate", 0)
      role.setErrorCode("XQTY0030")
      val config: Configuration = visitor.getConfiguration
      setSelect(config
        .getTypeChecker(false)
        .staticTypeCheck(getSelect, SequenceType.SINGLE_NODE, role, visitor))
      val th = config.getTypeHierarchy
      val t: ItemType = getSelect.getItemType
      if (th.isSubType(t, NodeKindTest.ATTRIBUTE)) {
        throw new XPathException(
          "validate{} expression cannot be applied to an attribute",
          "XQTY0030")
      }
      if (th.isSubType(t, NodeKindTest.TEXT)) {
        throw new XPathException(
          "validate{} expression cannot be applied to a text node",
          "XQTY0030")
      }
      if (th.isSubType(t, NodeKindTest.COMMENT)) {
        throw new XPathException(
          "validate{} expression cannot be applied to a comment node",
          "XQTY0030")
      }
      if (th.isSubType(t, NodeKindTest.PROCESSING_INSTRUCTION)) {
        throw new XPathException(
          "validate{} expression cannot be applied to a processing instruction node",
          "XQTY0030")
      }
      if (th.isSubType(t, NodeKindTest.NAMESPACE)) {
        throw new XPathException(
          "validate{} expression cannot be applied to a namespace node",
          "XQTY0030")
      }
    }
    this
  }

  override def optimize(visitor: ExpressionVisitor,
                        contextItemType: ContextItemStaticInfo): Expression = {
    selectOp.optimize(visitor, contextItemType)
    if (Literal.isEmptySequence(getSelect)) {
      getSelect
    }
    adoptChildExpression(getSelect)
    if (getSelect.getItemType.isPlainType) {
      getSelect
    }
    this
  }

  def export(out: ExpressionPresenter): Unit = {
    out.startElement("copyOf", this)
    if (validation != Validation.SKIP) {
      out.emitAttribute("validation", Validation.toString(validation))
    }
    if (schemaType != null) {
      out.emitAttribute("type", schemaType.getStructuredQName)
    }
    val fsb: FastStringBuffer = new FastStringBuffer(FastStringBuffer.C16)
    if (requireDocumentOrElement) {
      fsb.cat('p')
    }
    if (rejectDuplicateAttributes) {
      fsb.cat('a')
    }
    if (validating) {
      fsb.cat('v')
    }
    if (copyLineNumbers) {
      fsb.cat('l')
    }
    if (copyForUpdate) {
      fsb.cat('u')
    }
    if (isSchemaAware) {
      fsb.cat('s')
    }
    if (copyNamespaces) {
      fsb.cat('c')
    }
    if (copyAccumulators) {
      fsb.cat('m')
    }
    if (!fsb.isEmpty) {
      out.emitAttribute("flags", fsb.toString)
    }
    getSelect.export(out)
    out.endElement()
  }

  override def getStreamerName: String = "CopyOf"

  override def addToPathMap(
                             pathMap: PathMap,
                             pathMapNodeSet: PathMap.PathMapNodeSet): PathMap.PathMapNodeSet = {
    val result: PathMap.PathMapNodeSet =
      super.addToPathMap(pathMap, pathMapNodeSet)
    result.setReturnable(false)
    val th = getConfiguration.getTypeHierarchy
    val `type`: ItemType = getItemType
    if (th.relationship(`type`, NodeKindTest.ELEMENT) != Affinity.DISJOINT ||
      th.relationship(`type`, NodeKindTest.DOCUMENT) != Affinity.DISJOINT) {
      result.addDescendants()
    }
    new PathMap.PathMapNodeSet(pathMap.makeNewRoot(this))
  }

  def processLeavingTail(out: Outputter, context: XPathContext): TailCall = {
    if (copyAccumulators) {
      if (mustPush()) {
        getSelect
          .iterate(context)
          .forEachOrFail((item) =>
            if (item.isInstanceOf[NodeInfo]) {
              val builder: TinyBuilder =
                new TinyBuilder(out.getPipelineConfiguration)
              val cco: ComplexContentOutputter =
                new ComplexContentOutputter(builder)
              cco.open()
              copyOneNode(context,
                cco,
                item.asInstanceOf[NodeInfo],
                CopyOptions.ALL_NAMESPACES)
              cco.close()
              val copy: TinyNodeImpl =
                builder.getCurrentRoot.asInstanceOf[TinyNodeImpl]
              copy.getTree.setCopiedFrom(item.asInstanceOf[NodeInfo])
              out.append(copy)
            } else {
              out.append(item)
            })
      } else {
        iterate(context).forEachOrFail(out.append)
      }
    } else {
      val copyOptions: Int = (if (validation == Validation.SKIP) 0
      else CopyOptions.TYPE_ANNOTATIONS) |
        (if (copyNamespaces) CopyOptions.ALL_NAMESPACES else 0) |
        (if (copyForUpdate) CopyOptions.FOR_UPDATE else 0)
      getSelect
        .iterate(context)
        .forEachOrFail((item) =>
          if (item.isInstanceOf[NodeInfo]) {
            copyOneNode(context, out, item.asInstanceOf[NodeInfo], copyOptions)
          } else {
            out.append(item, getLocation, ReceiverOption.ALL_NAMESPACES)
          })
    }
    null
  }

  @throws[XPathException]
  private def copyOneNode(context: XPathContext,
                          out: Outputter,
                          item: NodeInfo,
                          copyOptions: Int) = {
    val controller: Controller = context.getController
    val copyBaseURI: Boolean = out.getSystemId == null
    val kind: Int = item.getNodeKind
    if (requireDocumentOrElement && !(kind == Type.ELEMENT || kind == Type.DOCUMENT)) {
      val e: XPathException = new XPathException(
        "Operand of validate expression must be a document or element node")
      e.setXPathContext(context)
      e.setErrorCode("XQTY0030")
      throw e
    }
    val config: Configuration = controller.getConfiguration
    kind match {
      case Type.ELEMENT => {
        var eval: Outputter = out
        if (validating) {
          val options: ParseOptions = new ParseOptions()
          options.setSchemaValidationMode(validation)
          var `type`: SchemaType = schemaType
          if (`type` == null &&
            (validation == Validation.STRICT || validation == Validation.LAX)) {
            val xsitype: String =
              item.getAttributeValue(NamespaceConstant.SCHEMA_INSTANCE, "type")
            if (xsitype != null) {
              var typeName: StructuredQName = null
              typeName = StructuredQName.fromLexicalQName(
                xsitype,
                useDefault = true,
                allowEQName = false,
                item.getAllNamespaces)
              `type` = config.getSchemaType(typeName)
              if (`type` == null) {
                throw new XPathException(
                  "Unknown xsi:type in element being validated: " + xsitype,
                  "XTTE1510")
              }
            }
          }
          options.setTopLevelType(`type`)
          options.setTopLevelElement(
            NameOfNode.makeName(item).getStructuredQName)
          options.setErrorReporter(context.getErrorReporter)
          config.prepareValidationReporting(context, options)
          val validator: Receiver =
            config.getElementValidator(out, options, getLocation)
          eval = new ComplexContentOutputter(validator)
        }
        if (copyBaseURI) {
          eval.setSystemId(computeNewBaseUri(item, getStaticBaseURIString))
        }
        val pipe: PipelineConfiguration = out.getPipelineConfiguration
        if (copyLineNumbers) {
          val copier: LocationCopier = new LocationCopier(false)
          pipe.setComponent(classOf[CopyInformee[_ <: AnyRef]].getName, copier)
        }
        item.copy(eval, copyOptions, getLocation)
        if (copyLineNumbers) {
          pipe.setComponent(classOf[CopyInformee[_ <: AnyRef]].getName, null)
        }
      }
      case Type.ATTRIBUTE =>
        if (schemaType != null && schemaType.isComplexType) {
          val e: XPathException = new XPathException(
            "When copying an attribute with schema validation, the requested type must not be a complex type")
          e.setLocation(getLocation)
          e.setXPathContext(context)
          e.setErrorCode("XTTE1535")
          throw dynamicError(getLocation.toString, e.toString, context)
        }
        try copyAttribute(item,
          schemaType.asInstanceOf[SimpleType],
          validation,
          this,
          out,
          context,
          rejectDuplicateAttributes)
        catch {
          case err: NoOpenStartTagException => {
            val e: XPathException = new XPathException(err.getMessage)
            e.setLocation(getLocation)
            e.setXPathContext(context)
            e.setErrorCodeQName(err.getErrorCodeQName)
            throw dynamicError(getLocation.toString, e.toString, context)
          }

        }
      case Type.TEXT =>
        out.characters(item.getStringValueCS, getLocation, ReceiverOption.NONE)
      case Type.PROCESSING_INSTRUCTION =>
        if (copyBaseURI) {
          out.setSystemId(item.getBaseURI)
        }
        out.processingInstruction(item.getDisplayName,
          item.getStringValueCS,
          getLocation,
          ReceiverOption.NONE)
      case Type.COMMENT =>
        out.comment(item.getStringValueCS, getLocation, ReceiverOption.NONE)
      case Type.NAMESPACE =>
        try out.namespace(item.getLocalPart,
          item.getStringValue,
          ReceiverOption.NONE)
        catch {
          case err: NoOpenStartTagException => {
            val e: XPathException = new XPathException(err.getMessage)
            e.setXPathContext(context)
            e.setErrorCodeQName(err.getErrorCodeQName)
            throw dynamicError(getLocation.toString, e.toString, context)
          }

        }
      case Type.DOCUMENT => {
        val options: ParseOptions = new ParseOptions()
        options.setSchemaValidationMode(validation)
        options.setSpaceStrippingRule(NoElementsSpaceStrippingRule.getInstance)
        options.setTopLevelType(schemaType)
        options.setErrorReporter(context.getErrorReporter)
        config.prepareValidationReporting(context, options)
        val `val`: Receiver = config.getDocumentValidator(out,
          item.getBaseURI,
          options,
          getLocation)
        if (copyBaseURI) {
          `val`.setSystemId(item.getBaseURI)
        }
        var savedPipe: PipelineConfiguration = null
        if (copyLineNumbers) {
          savedPipe = new PipelineConfiguration(`val`.getPipelineConfiguration)
          val copier: LocationCopier = new LocationCopier(true)
          `val`.getPipelineConfiguration
            .setComponent(classOf[CopyInformee[_ <: AnyRef]].getName, copier)
        }
        item.copy(`val`, copyOptions, getLocation)
        if (copyLineNumbers) {
          `val`.setPipelineConfiguration(savedPipe)
        }
      }
      case _ =>
        throw new IllegalArgumentException(
          "Unknown node kind " + item.getNodeKind)

    }
  }

  private def mustPush(): Boolean =
    schemaType != null || validation == Validation.LAX || validation == Validation.STRICT ||
      copyForUpdate

  override def iterate(context: XPathContext): SequenceIterator = {
    val controller: Controller = context.getController
    assert(controller != null)
    if (schemaType == null && !copyForUpdate) {
      if (validation == Validation.PRESERVE) {
        val copier: ItemMappingFunction = (item) =>
          if (item.isInstanceOf[NodeInfo]) {
            if (item.asInstanceOf[NodeInfo].getTreeInfo.isTyped) {
              if (!copyNamespaces &&
                item.asInstanceOf[NodeInfo].getNodeKind == Type.ELEMENT) {
                var sink: Sink = new Sink(
                  controller.makePipelineConfiguration)
                item
                  .asInstanceOf[NodeInfo]
                  .copy(sink, CopyOptions.TYPE_ANNOTATIONS, getLocation)
              }
              if (item.asInstanceOf[NodeInfo].getNodeKind == Type.ATTRIBUTE &&
                item
                  .asInstanceOf[NodeInfo]
                  .getSchemaType
                  .asInstanceOf[SimpleType]
                  .isNamespaceSensitive) {
                throw new XPathException(
                  "Cannot copy an attribute with namespace-sensitive content except as part of its containing element",
                  "XTTE0950")
              }
            }
            var vc: VirtualCopy =
              VirtualCopy.makeVirtualCopy(item.asInstanceOf[NodeInfo])
            vc.setDropNamespaces(!copyNamespaces)
            vc.getTreeInfo.setCopyAccumulators(copyAccumulators)
            if (item.asInstanceOf[NodeInfo].getNodeKind == Type.ELEMENT) {
              vc.setSystemId(
                computeNewBaseUri(item.asInstanceOf[NodeInfo],
                  getStaticBaseURIString))
            }
            vc
          } else {
            item
          }
        new ItemMappingIterator(getSelect.iterate(context), copier, true)
      } else if (validation == Validation.STRIP) {
        val copier: ItemMappingFunction = item => {
          if (! item.isInstanceOf[NodeInfo]) {
            item
          } else {
            val vc: VirtualCopy = VirtualUntypedCopy.makeVirtualUntypedTree(
              item.asInstanceOf[NodeInfo],
              item.asInstanceOf[NodeInfo])
            vc.getTreeInfo.setCopyAccumulators(copyAccumulators)
            vc.setDropNamespaces(!copyNamespaces)
            if (item.asInstanceOf[NodeInfo].getNodeKind == Type.ELEMENT) {
              vc.setSystemId(
                computeNewBaseUri(item.asInstanceOf[NodeInfo], getStaticBaseURIString))
            }
            vc
          }
        }
        new ItemMappingIterator(getSelect.iterate(context), copier, true)
      }
    }
    val pipe: PipelineConfiguration = controller.makePipelineConfiguration
    pipe.setXPathContext(context)
    val out: SequenceCollector = new SequenceCollector(pipe)
    if (copyForUpdate) {
      out.setTreeModel(TreeModel.LINKED_TREE)
    }
    pipe.setHostLanguage(getPackageData.getHostLanguage)
    try process(new ComplexContentOutputter(out), context)
    catch {
      case err: XPathException => {
        err.maybeSetLocation(getLocation)
        err.maybeSetContext(context)
        throw err
      }

    }
    out.getSequence.iterate()
  }

}
