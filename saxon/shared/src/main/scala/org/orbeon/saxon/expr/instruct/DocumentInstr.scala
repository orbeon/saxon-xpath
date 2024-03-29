package org.orbeon.saxon.expr.instruct

import org.orbeon.saxon.utils.Configuration

import org.orbeon.saxon.utils.Controller

import org.orbeon.saxon.event._

import org.orbeon.saxon.expr._

import org.orbeon.saxon.expr.parser.ExpressionTool

import org.orbeon.saxon.expr.parser.RebindingMap

import org.orbeon.saxon.functions.SystemFunction

import org.orbeon.saxon.lib.ParseOptions

import org.orbeon.saxon.lib.Validation

import org.orbeon.saxon.model._

import org.orbeon.saxon.om.Item

import org.orbeon.saxon.om.NodeInfo

import org.orbeon.saxon.om.SequenceIterator

import org.orbeon.saxon.om.StandardNames

import org.orbeon.saxon.pattern.NodeKindTest

import org.orbeon.saxon.pattern.NodeTest

import org.orbeon.saxon.trace.ExpressionPresenter

import org.orbeon.saxon.trans.XPathException

import org.orbeon.saxon.tree.tiny.TinyBuilder

import org.orbeon.saxon.tree.util.FastStringBuffer

import org.orbeon.saxon.value.StringValue

import org.orbeon.saxon.value.TextFragmentValue

import org.orbeon.saxon.value.UntypedAtomicValue

import scala.beans.{BeanProperty, BooleanBeanProperty}

object DocumentInstr {

   def checkContentSequence(env: StaticContext,
                                     content: Operand,
                                     validationOptions: ParseOptions): Unit = {
    var components: Array[Operand] = null
    components =
      if (content.getChildExpression.isInstanceOf[Block])
        content.getChildExpression.asInstanceOf[Block].getOperanda
      else Array(content)
    val validation: Int =
      if (validationOptions == null) Validation.PRESERVE
      else validationOptions.getSchemaValidationMode
    val `type`: SchemaType =
      if (validationOptions == null) null
      else validationOptions.getTopLevelType
    var elementCount: Int = 0
    val isXSLT: Boolean = content.getChildExpression.getPackageData.isXSLT
    for (o <- components) {
      val component: Expression = o.getChildExpression
      val it: ItemType = component.getItemType
      if (it.isInstanceOf[NodeTest]) {
        val possibleNodeKinds: UType = it.getUType
        if (possibleNodeKinds == UType.ATTRIBUTE) {
          val de: XPathException = new XPathException(
            "Cannot create an attribute node whose parent is a document node")
          de.setErrorCode(if (isXSLT) "XTDE0420" else "XPTY0004")
          de.setLocator(component.getLocation)
          throw de
        } else if (possibleNodeKinds == UType.NAMESPACE) {
          val de: XPathException = new XPathException(
            "Cannot create a namespace node whose parent is a document node")
          de.setErrorCode(if (isXSLT) "XTDE0420" else "XQTY0024")
          de.setLocator(component.getLocation)
          throw de
        } else if (possibleNodeKinds == UType.ELEMENT) {
          elementCount += 1
          if (elementCount > 1 &&
            (validation == Validation.STRICT || validation == Validation.LAX ||
              `type` != null)) {
            val de: XPathException = new XPathException(
              "A valid document must have only one child element")
            if (isXSLT) {
              de.setErrorCode("XTTE1550")
            } else {
              de.setErrorCode("XQDY0061")
            }
            de.setLocator(component.getLocation)
            throw de
          }
          if (validation == Validation.STRICT && component
            .isInstanceOf[FixedElement]) {
            val decl: SchemaDeclaration =
              env.getConfiguration.getElementDeclaration(
                component
                  .asInstanceOf[FixedElement]
                  .getElementName
                  .getFingerprint)
            if (decl != null) {
              component
                .asInstanceOf[FixedElement]
                .getContentExpression
                .checkPermittedContents(decl.getType, whole = true)
            }
          }
        }
      }
    }
  }

}

class DocumentInstr(@BooleanBeanProperty var textOnly: Boolean,
                    @BeanProperty var constantText: String)
  extends ParentNodeConstructor {

  override def operands: java.lang.Iterable[Operand] = contentOp

  override def getImplementationMethod: Int = Expression.EVALUATE_METHOD

   def checkContentSequence(env: StaticContext): Unit = {
    DocumentInstr.checkContentSequence(env, getContentOperand, getValidationOptions)
  }

  override def computeSpecialProperties(): Int = {
    var p: Int = super.computeSpecialProperties()
    p |= StaticProperty.SINGLE_DOCUMENT_NODESET
    if (getValidationAction == Validation.SKIP) {
      p |= StaticProperty.ALL_NODES_UNTYPED
    }
    p
  }

  def getStringValueExpression: Expression =
    if (textOnly) {
      if (constantText != null) {
        new StringLiteral(new UntypedAtomicValue(constantText))
      } else if (getContentExpression.isInstanceOf[ValueOf]) {
        getContentExpression.asInstanceOf[ValueOf].convertToCastAsString
      } else {
        val fn: Expression = SystemFunction.makeCall(
          "string-join",
          getRetainedStaticContext,
          getContentExpression,
          new StringLiteral(StringValue.EMPTY_STRING))
        val cast: CastExpression =
          new CastExpression(fn, BuiltInAtomicType.UNTYPED_ATOMIC, false)
        ExpressionTool.copyLocationInfo(this, cast)
        cast
      }
    } else {
      throw new AssertionError(
        "getStringValueExpression() called on non-text-only document instruction")
    }

  def copy(rebindings: RebindingMap): Expression = {
    val doc: DocumentInstr = new DocumentInstr(textOnly, constantText)
    ExpressionTool.copyLocationInfo(this, doc)
    doc.setContentExpression(getContentExpression.copy(rebindings))
    doc.setValidationAction(getValidationAction, getSchemaType)
    doc
  }

override  def getItemType: ItemType = NodeKindTest.DOCUMENT

  def processLeavingTail(output: Outputter, context: XPathContext): TailCall =
    if (preservingTypes && !textOnly) {
      output.startDocument(ReceiverOption.NONE)
      getContentExpression.process(output, context)
      output.endDocument()
      null
    } else {
      val item: Item = evaluateItem(context)
      if (item != null) {
        output.append(item, getLocation, ReceiverOption.ALL_NAMESPACES)
      }
      null
    }

 override def evaluateItem(context: XPathContext): NodeInfo = {
    val controller: Controller = context.getController
    val config: Configuration = controller.getConfiguration
    var root: NodeInfo = null
    if (textOnly) {
      var textValue: CharSequence = null
      if (constantText != null) {
        textValue = constantText
      } else {
        val sb = new FastStringBuffer(FastStringBuffer.C64)
        val iter: SequenceIterator = getContentExpression.iterate(context)
        var item: Item = null
        while (({
          item = iter.next()
          item
        }) != null) sb.cat(item.getStringValueCS)
        textValue = sb.condense()
      }
      root = TextFragmentValue.makeTextFragment(config,
        textValue,
        getStaticBaseURIString)
    } else {
      try {
        val pipe: PipelineConfiguration =
          controller.makePipelineConfiguration
        pipe.setXPathContext(context)
        var builder: Builder = null
        builder = controller.makeBuilder
        builder.setUseEventLocation(false)
        if (builder.isInstanceOf[TinyBuilder]) {
          builder
            .asInstanceOf[TinyBuilder]
            .setStatistics(config.getTreeStatistics.SOURCE_DOCUMENT_STATISTICS)
        }
        builder.setBaseURI(getStaticBaseURIString)
        builder.setTiming(false)
        pipe.setHostLanguage(getPackageData.getHostLanguage)
        builder.setPipelineConfiguration(pipe)
        val out: ComplexContentOutputter = ComplexContentOutputter
          .makeComplexContentReceiver(builder, getValidationOptions)
        out.open()
        out.startDocument(ReceiverOption.NONE)
        getContentExpression.process(out, context)
        out.endDocument()
        out.close()
        root = builder.getCurrentRoot
      } catch {
        case e: XPathException => {
          e.maybeSetLocation(getLocation)
          e.maybeSetContext(context)
          throw e
        }

      }
    }
    root
  }

  override def getInstructionNameCode(): Int = StandardNames.XSL_DOCUMENT

  def export(out: ExpressionPresenter): Unit = {
    out.startElement("doc", this)
    if (!out.isRelocatable) {
      out.emitAttribute("base", getStaticBaseURIString)
    }
    var flags: String = ""
    if (textOnly) {
      flags += "t"
    }
    if (isLocal) {
      flags += "l"
    }
    if (!flags.isEmpty) {
      out.emitAttribute("flags", flags)
    }
    if (constantText != null) {
      out.emitAttribute("text", constantText)
    }
    if (getValidationAction != Validation.SKIP && getValidationAction != Validation.BY_TYPE) {
      out.emitAttribute("validation", Validation.toString(getValidationAction))
    }
    val schemaType: SchemaType = getSchemaType
    if (schemaType != null) {
      out.emitAttribute("type", schemaType.getStructuredQName)
    }
    getContentExpression.export(out)
    out.endElement()
  }

  override def getStreamerName: String = "DocumentInstr"

}
