package net.sf.saxon.expr.instruct

import java.util.Iterator

import net.sf.saxon.event._
import net.sf.saxon.expr._
import net.sf.saxon.expr.instruct.Copy._
import net.sf.saxon.expr.parser.{ContextItemStaticInfo, ExpressionTool, ExpressionVisitor, RebindingMap}
import net.sf.saxon.lib.{ParseOptions, Validation}
import net.sf.saxon.model.Affinity.Affinity
import net.sf.saxon.model._
import net.sf.saxon.om._
import net.sf.saxon.pattern._
import net.sf.saxon.trace.ExpressionPresenter
import net.sf.saxon.trans.XPathException
import net.sf.saxon.utils.{Configuration, Controller}
import net.sf.saxon.value.{BooleanValue, SequenceType}

import scala.beans.BooleanBeanProperty

object Copy {

  def copyUnparsedEntities(source: NodeInfo, out: Outputter): Unit = {
    val unparsedEntities: Iterator[String] =
      source.getTreeInfo.getUnparsedEntityNames
    while (unparsedEntities.hasNext) {
      val n: String = unparsedEntities.next()
      val details: Array[String] = source.getTreeInfo.getUnparsedEntity(n)
      out.setUnparsedEntity(n, details(0), details(1))
    }
  }

}

class Copy(@BooleanBeanProperty var copyNamespaces: Boolean,
           inheritNamespaces: Boolean,
           schemaType: SchemaType,
           validation: Int)
  extends ElementCreator {

  private var selectItemType: ItemType = AnyItemType.getInstance

  private var resultItemType: ItemType = _

  this.bequeathNamespacesToChildren = inheritNamespaces

  setValidationAction(validation, schemaType)

  preservingTypes = schemaType == null && validation == Validation.PRESERVE

  override def simplify(): Expression = {
    preservingTypes |= !getPackageData.isSchemaAware
    super.simplify()
  }

  override def typeCheck(visitor: ExpressionVisitor,
                         contextInfo: ContextItemStaticInfo): Expression = {
    var selectItemType: ItemType = contextInfo.getItemType
    typeCheckChildren(visitor, contextInfo)
    selectItemType = contextInfo.getItemType
    if (selectItemType == ErrorType.getInstance) {
      val err: XPathException =
        new XPathException("No context item supplied for xsl:copy", "XTTE0945")
      err.setIsTypeError(true)
      err.setLocation(getLocation)
      throw err
    }
    if (selectItemType.isInstanceOf[NodeTest]) {
      selectItemType.getPrimitiveType match {
        case Type.ELEMENT => this.resultItemType = NodeKindTest.ELEMENT
        case Type.DOCUMENT => this.resultItemType = NodeKindTest.DOCUMENT
        case Type.ATTRIBUTE | Type.TEXT | Type.COMMENT |
             Type.PROCESSING_INSTRUCTION | Type.NAMESPACE =>
          var dot: ContextItemExpression = new ContextItemExpression()
          ExpressionTool.copyLocationInfo(this, dot)
          var c: CopyOf = new CopyOf(dot,
            copyNamespaces,
            getValidationAction,
            getSchemaType,
            false)
          ExpressionTool.copyLocationInfo(this, c)
          c.typeCheck(visitor, contextInfo)
        case _ => this.resultItemType = selectItemType

      }
    } else {
      this.resultItemType = selectItemType
    }
    checkContentSequence(visitor.getStaticContext)
    this
  }

  def copy(rebindings: RebindingMap): Expression = {
    val copy: Copy = new Copy(copyNamespaces,
      bequeathNamespacesToChildren,
      getSchemaType,
      getValidationAction)
    ExpressionTool.copyLocationInfo(this, copy)
    copy.setContentExpression(getContentExpression.copy(rebindings))
    copy.resultItemType = resultItemType
    copy
  }

  def setSelectItemType(`type`: ItemType): Unit = {
    selectItemType = `type`
  }

  override def getIntrinsicDependencies(): Int = StaticProperty.DEPENDS_ON_CONTEXT_ITEM

  override def getInstructionNameCode(): Int = StandardNames.XSL_COPY

  override def operands(): java.lang.Iterable[Operand] = contentOp

  override def getItemType(): ItemType =
    if (resultItemType != null) {
      resultItemType
    } else {
      val th: TypeHierarchy = getConfiguration.getTypeHierarchy
      resultItemType = computeItemType(th)
      resultItemType
    }

  private def computeItemType(th: TypeHierarchy): ItemType = {
    val selectItemType: ItemType = this.selectItemType
    if (!getPackageData.isSchemaAware) {
      return selectItemType
    }
    if (selectItemType.getUType.overlaps(
      UType.ANY_ATOMIC.union(UType.FUNCTION))) {
      return selectItemType
    }
    val config: Configuration = th.getConfiguration
    if (getSchemaType != null) {
      val e: Affinity = th.relationship(selectItemType, NodeKindTest.ELEMENT)
      if (e == Affinity.SAME_TYPE || e == Affinity.SUBSUMED_BY) {
        new ContentTypeTest(Type.ELEMENT, getSchemaType, config, false)
      }
      val a: Affinity = th.relationship(selectItemType, NodeKindTest.ATTRIBUTE)
      if (a == Affinity.SAME_TYPE || a == Affinity.SUBSUMED_BY) {
        new ContentTypeTest(Type.ATTRIBUTE, getSchemaType, config, false)
      }
      AnyNodeTest.getInstance
    } else {
      getValidationAction match {
        case Validation.PRESERVE => selectItemType
        case Validation.STRIP => {
          val e: Affinity =
            th.relationship(selectItemType, NodeKindTest.ELEMENT)
          if (e == Affinity.SAME_TYPE || e == Affinity.SUBSUMED_BY) {
            new ContentTypeTest(Type.ELEMENT,
              Untyped.getInstance,
              config,
              false)
          }
          val a: Affinity =
            th.relationship(selectItemType, NodeKindTest.ATTRIBUTE)
          if (a == Affinity.SAME_TYPE || a == Affinity.SUBSUMED_BY) {
            new ContentTypeTest(Type.ATTRIBUTE,
              BuiltInAtomicType.UNTYPED_ATOMIC,
              config,
              false)
          }
          if (e != Affinity.DISJOINT || a != Affinity.DISJOINT) {
            AnyNodeTest.getInstance
          } else {
            selectItemType
          }
        }
        case Validation.STRICT | Validation.LAX =>
          if (selectItemType.isInstanceOf[NodeTest]) {
            val fp: Int = selectItemType.asInstanceOf[NodeTest].getFingerprint
            if (fp != -1) {
              val e: Affinity =
                th.relationship(selectItemType, NodeKindTest.ELEMENT)
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
              val a: Affinity =
                th.relationship(selectItemType, NodeKindTest.ATTRIBUTE)
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
              val e: Affinity =
                th.relationship(selectItemType, NodeKindTest.ELEMENT)
              if (e == Affinity.SAME_TYPE || e == Affinity.SUBSUMED_BY) {
                NodeKindTest.ELEMENT
              }
              val a: Affinity =
                th.relationship(selectItemType, NodeKindTest.ATTRIBUTE)
              if (a == Affinity.SAME_TYPE || a == Affinity.SUBSUMED_BY) {
                NodeKindTest.ATTRIBUTE
              }
            }
            AnyNodeTest.getInstance
          } else if (selectItemType.isInstanceOf[AtomicType]) {
            selectItemType
          } else {
            AnyItemType.getInstance
          }
        case _ => throw new IllegalStateException()

      }
    }
  }

  override def optimize(visitor: ExpressionVisitor,
                        contextItemType: ContextItemStaticInfo): Expression = {
    val exp: Expression = super.optimize(visitor, contextItemType)
    if (exp == this) {
      if (resultItemType == null) {
        resultItemType = computeItemType(
          visitor.getConfiguration.getTypeHierarchy)
      }
      if (visitor.isOptimizeForStreaming) {
        val `type`: UType = contextItemType.getItemType.getUType
        if (`type`.intersection(MultipleNodeKindTest.LEAF.getUType) != UType.VOID) {
          val p: Expression = getParentExpression
          if (p.isInstanceOf[Choose] && p.asInstanceOf[Choose].size == 2 &&
            p.asInstanceOf[Choose].getAction(1) == this &&
            p.asInstanceOf[Choose].getAction(0).isInstanceOf[CopyOf]) {
            return exp
          }
          val copyOf: Expression = new CopyOf(new ContextItemExpression(),
            false,
            getValidationAction,
            getSchemaType,
            false)
          val leafTest: NodeTest = new MultipleNodeKindTest(
            `type`.intersection(MultipleNodeKindTest.LEAF.getUType))
          val conditions: Array[Expression] = Array(
            new InstanceOfExpression(
              new ContextItemExpression(),
              SequenceType.makeSequenceType(leafTest,
                StaticProperty.EXACTLY_ONE)),
            Literal.makeLiteral(BooleanValue.TRUE, this)
          )
          val actions: Array[Expression] = Array(copyOf, this)
          val choose: Choose = new Choose(conditions, actions)
          ExpressionTool.copyLocationInfo(this, choose)
          return choose
        }
      }
    }
    exp
  }

  def getElementName(context: XPathContext, copiedNode: NodeInfo): NodeName =
    NameOfNode.makeName(copiedNode)

  def getNewBaseURI(context: XPathContext, copiedNode: NodeInfo): String =
    copiedNode.getBaseURI

  def outputNamespaceNodes(receiver: Outputter,
                           nodeName: NodeName,
                           copiedNode: NodeInfo): Unit = {
    if (copyNamespaces) {
      receiver.namespaces(copiedNode.getAllNamespaces,
        ReceiverOption.NAMESPACE_OK)
    } else {
      val ns: NamespaceBinding = nodeName.getNamespaceBinding
      if (!ns.isDefaultUndeclaration) {
        receiver.namespace(ns.getPrefix, ns.getURI, ReceiverOption.NONE)
      }
    }
  }

  override def processLeavingTail(out: Outputter, context: XPathContext): TailCall = {
    var outPutter = out
    val controller: Controller = context.getController
    val item: Item = context.getContextItem
    if (item == null) {
      return null
    }
    if (!(item.isInstanceOf[NodeInfo])) {
      outPutter.append(item, getLocation, ReceiverOption.ALL_NAMESPACES)
      return null
    }
    val source: NodeInfo = item.asInstanceOf[NodeInfo]
    source.getNodeKind match {
      case Type.ELEMENT =>
        super.processLeavingTail(outPutter, context, item.asInstanceOf[NodeInfo])
      case Type.ATTRIBUTE =>
        if (getSchemaType.isInstanceOf[ComplexType]) {
          dynamicError(
            "Cannot copy an attribute when the type requested for validation is a complex type",
            "XTTE1535",
            context)
        }
        try CopyOf.copyAttribute(source,
          getSchemaType.asInstanceOf[SimpleType],
          getValidationAction,
          this,
          outPutter,
          context,
          false)
        catch {
          case err: NoOpenStartTagException => {
            err.setXPathContext(context)
            throw dynamicError(getLocation.toString, err.toString, context)
          }

        }
      case Type.TEXT =>
        var tval: CharSequence = source.getStringValueCS
        outPutter.characters(tval, getLocation, ReceiverOption.NONE)
      case Type.PROCESSING_INSTRUCTION =>
        var pval: CharSequence = source.getStringValueCS
        outPutter.processingInstruction(source.getDisplayName,
          pval,
          getLocation,
          ReceiverOption.NONE)
      case Type.COMMENT =>
        var cval: CharSequence = source.getStringValueCS
        outPutter.comment(cval, getLocation, ReceiverOption.NONE)
      case Type.NAMESPACE =>
        outPutter.namespace(item.asInstanceOf[NodeInfo].getLocalPart,
          item.getStringValue,
          ReceiverOption.NONE)
      case Type.DOCUMENT =>
        if (!preservingTypes) {
          val options: ParseOptions = new ParseOptions(getValidationOptions)
          options.setSpaceStrippingRule(
            NoElementsSpaceStrippingRule.getInstance)
          controller.getConfiguration
            .prepareValidationReporting(context, options)
          val `val`: Receiver = controller.getConfiguration
            .getDocumentValidator(outPutter, source.getBaseURI, options, getLocation)
          outPutter = new ComplexContentOutputter(`val`)
        }
        if (outPutter.getSystemId == null) {
          outPutter.setSystemId(source.getBaseURI)
        }
        outPutter.startDocument(ReceiverOption.NONE)
        copyUnparsedEntities(source, outPutter)
        getContentExpression.process(outPutter, context)
        outPutter.endDocument()
      case _ =>
        throw new IllegalArgumentException(
          "Unknown node kind " + source.getNodeKind)

    }
    null
  }

  override def evaluateItem(context: XPathContext): Item = {
    val controller: Controller = context.getController
    val seq: SequenceCollector = controller.allocateSequenceOutputter(1)
    seq.getPipelineConfiguration.setHostLanguage(
      getPackageData.getHostLanguage)
    process(new ComplexContentOutputter(seq), context)
    seq.close()
    val item: Item = seq.getFirstItem
    seq.reset()
    item
  }

  override def getStreamerName(): String = "Copy"

  def export(out: ExpressionPresenter): Unit = {
    out.startElement("copy", this)
    exportValidationAndType(out)
    var flags: String = ""
    if (copyNamespaces) {
      flags = "c"
    }
    if (bequeathNamespacesToChildren) {
      flags += "i"
    }
    if (inheritNamespacesFromParent) {
      flags += "n"
    }
    if (isLocal) {
      flags += "l"
    }
    out.emitAttribute("flags", flags)
    val sType: String = SequenceType.makeSequenceType(selectItemType, getCardinality).toAlphaCode
    if (sType != null) {
      out.emitAttribute("sit", sType)
    }
    out.setChildRole("content")
    getContentExpression.export(out)
    out.endElement()
  }

}
