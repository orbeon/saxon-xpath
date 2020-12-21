package org.orbeon.saxon.expr.instruct

import org.orbeon.saxon.event.ComplexContentOutputter

import org.orbeon.saxon.event.Outputter

import org.orbeon.saxon.event.Receiver

import org.orbeon.saxon.event.ReceiverOption

import org.orbeon.saxon.expr._

import org.orbeon.saxon.lib.ParseOptions

import org.orbeon.saxon.lib.Validation

import org.orbeon.saxon.ma.arrays.ArrayItemType

import org.orbeon.saxon.ma.map.MapType

import org.orbeon.saxon.model._

import org.orbeon.saxon.om.NodeInfo

import org.orbeon.saxon.om.NodeName

import org.orbeon.saxon.pattern.NodeKindTest

import org.orbeon.saxon.pattern.NodeTest

import org.orbeon.saxon.trace.ExpressionPresenter

import org.orbeon.saxon.trans.XPathException

import org.orbeon.saxon.value.Cardinality

abstract class ElementCreator extends ParentNodeConstructor {

  var bequeathNamespacesToChildren: Boolean = true

  var inheritNamespacesFromParent: Boolean = true

  override def getItemType: ItemType = NodeKindTest.ELEMENT

  override def getCardinality(): Int = StaticProperty.EXACTLY_ONE

  def setBequeathNamespacesToChildren(inherit: Boolean): Unit = {
    bequeathNamespacesToChildren = inherit
  }

  def isBequeathNamespacesToChildren: Boolean = bequeathNamespacesToChildren

  def setInheritNamespacesFromParent(inherit: Boolean): Unit = {
    inheritNamespacesFromParent = inherit
  }

  def isInheritNamespacesFromParent: Boolean = inheritNamespacesFromParent

  override def computeSpecialProperties(): Int = {
    var p: Int = super
      .computeSpecialProperties() | StaticProperty.SINGLE_DOCUMENT_NODESET
    if (getValidationAction == Validation.STRIP) {
      p |= StaticProperty.ALL_NODES_UNTYPED
    }
    p
  }

  override def suppressValidation(parentValidationMode: Int): Unit = {
    if (getValidationAction == parentValidationMode && getSchemaType == null) {
      setValidationAction(Validation.PRESERVE, null)
    }
  }

   def checkContentSequence(env: StaticContext): Unit = {
    var components: Array[Operand] = null
    components =
      if (getContentExpression.isInstanceOf[Block])
        getContentExpression.asInstanceOf[Block].getOperanda
      else Array(contentOp)
    var foundChild: Boolean = false
    var foundPossibleChild: Boolean = false
    for (o <- components) {
      val component: Expression = o.getChildExpression
      val it: ItemType = component.getItemType
      if (it.isAtomicType) {
        foundChild = true
      } else if (it.isInstanceOf[FunctionItemType] && !(it
        .isInstanceOf[ArrayItemType])) {
        val which: String = if (it.isInstanceOf[MapType]) "map" else "function"
        val de: XPathException = new XPathException(
          "Cannot add a " + which + " as a child of a constructed element")
        de.setErrorCode(if (isXSLT) "XTDE0450" else "XQTY0105")
        de.setLocator(component.getLocation)
        de.setIsTypeError(true)
        throw de
      } else if (it.isInstanceOf[NodeTest]) {
        val maybeEmpty: Boolean =
          Cardinality.allowsZero(component.getCardinality)
        val possibleNodeKinds: UType = it.getUType
        if (possibleNodeKinds.overlaps(UType.TEXT)) {
          if (component.isInstanceOf[ValueOf] &&
            component
              .asInstanceOf[ValueOf]
              .getSelect
              .isInstanceOf[StringLiteral]) {
            val value: String = component
              .asInstanceOf[ValueOf]
              .getSelect
              .asInstanceOf[StringLiteral]
              .getStringValue
            if (value.isEmpty) {} else {
              foundChild = true
            }
          } else {
            foundPossibleChild = true
          }
        } else if (!possibleNodeKinds.overlaps(UType.CHILD_NODE_KINDS)) {
          if (maybeEmpty) {
            foundPossibleChild = true
          } else {
            foundChild = true
          }
        } else if (foundChild && possibleNodeKinds == UType.ATTRIBUTE && !maybeEmpty) {
          val de: XPathException = new XPathException(
            "Cannot create an attribute node after creating a child of the containing element")
          de.setErrorCode(if (isXSLT) "XTDE0410" else "XQTY0024")
          de.setLocator(component.getLocation)
          throw de
        } else if (foundChild && possibleNodeKinds == UType.NAMESPACE && !maybeEmpty) {
          val de: XPathException = new XPathException(
            "Cannot create a namespace node after creating a child of the containing element")
          de.setErrorCode(if (isXSLT) "XTDE0410" else "XQTY0024")
          de.setLocator(component.getLocation)
          throw de
        } else if ((foundChild || foundPossibleChild) && possibleNodeKinds == UType.ATTRIBUTE) {
          env.issueWarning(
            "Creating an attribute here will fail if previous instructions create any children",
            component.getLocation)
        } else if ((foundChild || foundPossibleChild) && possibleNodeKinds == UType.NAMESPACE) {
          env.issueWarning(
            "Creating a namespace node here will fail if previous instructions create any children",
            component.getLocation)
        }
      }
    }
  }

  def getElementName(context: XPathContext, copiedNode: NodeInfo): NodeName

  def getNewBaseURI(context: XPathContext, copiedNode: NodeInfo): String

  def outputNamespaceNodes(receiver: Outputter,
                           nodeName: NodeName,
                           copiedNode: NodeInfo): Unit

 override def getImplementationMethod: Int = Expression.PROCESS_METHOD

  def processLeavingTail(output: Outputter, context: XPathContext): TailCall =
    processLeavingTail(output, context, null)

  def processLeavingTail(out: Outputter,
                         context: XPathContext,
                         copiedNode: NodeInfo): TailCall =
    try {
      var outPutter = out
      val elemName: NodeName = getElementName(context, copiedNode)
      val typeCode: SchemaType =
        if (getValidationAction == Validation.PRESERVE) AnyType.getInstance
        else Untyped.getInstance
      val elemOut: Receiver = outPutter
      if (!preservingTypes) {
        val options: ParseOptions = new ParseOptions(getValidationOptions)
        options.setTopLevelElement(elemName.getStructuredQName)
        context.getConfiguration.prepareValidationReporting(context, options)
        val validator: Receiver = context.getConfiguration.getElementValidator(
          elemOut,
          options,
          getLocation)
        if (validator != elemOut) {
          outPutter = new ComplexContentOutputter(validator)
        }
      }
      if (outPutter.getSystemId == null) {
        outPutter.setSystemId(getNewBaseURI(context, copiedNode))
      }
      var properties: Int = ReceiverOption.NONE
      if (!bequeathNamespacesToChildren) {
        properties |= ReceiverOption.DISINHERIT_NAMESPACES
      }
      if (!inheritNamespacesFromParent) {
        properties |= ReceiverOption.REFUSE_NAMESPACES
      }
      properties |= ReceiverOption.ALL_NAMESPACES
      outPutter.startElement(elemName, typeCode, getLocation, properties)
      outputNamespaceNodes(outPutter, elemName, copiedNode)
      getContentExpression.process(outPutter, context)
      out.endElement()
      null
    } catch {
      case e: XPathException => {
        e.maybeSetLocation(getLocation)
        e.maybeSetContext(context)
        throw e
      }

    }

  def exportValidationAndType(out: ExpressionPresenter): Unit = {
    if (getValidationAction != Validation.SKIP && getValidationAction != Validation.BY_TYPE) {
      out.emitAttribute("validation", Validation.toString(getValidationAction))
    }
    if (getValidationAction == Validation.BY_TYPE) {
      val `type`: SchemaType = getSchemaType
      if (`type` != null) {
        out.emitAttribute("type", `type`.getStructuredQName)
      }
    }
  }

  def getInheritanceFlags: String = {
    var flags: String = ""
    if (!inheritNamespacesFromParent) {
      flags += "P"
    }
    if (!bequeathNamespacesToChildren) {
      flags += "C"
    }
    flags
  }

  def setInheritanceFlags(flags: String): Unit = {
    inheritNamespacesFromParent = !flags.contains("P")
    bequeathNamespacesToChildren = !flags.contains("C")
  }

  override def getStreamerName: String = "ElementCreator"

}
