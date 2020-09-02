////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
package net.sf.saxon.expr.instruct

import net.sf.saxon.utils.Configuration

import net.sf.saxon.utils.Controller

import net.sf.saxon.event.Outputter

import net.sf.saxon.event.ReceiverOption

import net.sf.saxon.expr.StaticProperty

import net.sf.saxon.expr.XPathContext

import net.sf.saxon.lib.ConversionRules

import net.sf.saxon.lib.Validation

import net.sf.saxon.model._

import net.sf.saxon.om.NameOfNode

import net.sf.saxon.om.NodeName

import net.sf.saxon.om.StandardNames

import net.sf.saxon.pattern.NodeKindTest

import net.sf.saxon.trans.Err

import net.sf.saxon.trans.XPathException

import net.sf.saxon.tree.util.Orphan

import net.sf.saxon.value.Whitespace

import scala.beans.{BeanProperty, BooleanBeanProperty}


abstract class AttributeCreator
  extends SimpleNodeConstructor
    with ValidatingInstruction {

  /*@Nullable*/

  var schemaType: SimpleType = null

  @BeanProperty
  var validationAction: Int = _

  @BeanProperty
  var options: Int = ReceiverOption.NONE

  var isInstruct: Boolean = _

  def setInstruction(inst: Boolean): Unit = {
    isInstruct = inst
  }

  def setSchemaType(`type`: SimpleType): Unit = {
    schemaType = `type`
  }

  /*@Nullable*/

  def getSchemaType: SimpleType = schemaType

  def setRejectDuplicates(): Unit = {
    options |= ReceiverOption.REJECT_DUPLICATES
  }

  def setNoSpecialChars(): Unit = {
    options |= ReceiverOption.NO_SPECIAL_CHARS
  }

  /**
   * Get the static properties of this expression (other than its type). The result is
   * bit-signficant. These properties are used for optimizations. In general, if
   * property bit is set, it is true, but if it is unset, the value is unknown.
   *
   * @return a set of flags indicating static properties of this expression
   */
  override def computeSpecialProperties(): Int = {
    var p: Int = super.computeSpecialProperties()
    if (getValidationAction == Validation.SKIP) {
      p |= StaticProperty.ALL_NODES_UNTYPED
    }
    p
  }

  /*@NotNull*/

  override def getItemType: ItemType = NodeKindTest.ATTRIBUTE

  def processValue(value: CharSequence,
                   output: Outputter,
                   context: XPathContext): Unit = {
    var charSeq = value
    val attName: NodeName = evaluateNodeName(context)
    val opt: Int = getOptions
    var ann: SimpleType = null
    val schemaType: SimpleType = getSchemaType
    val validateAction: Int = getValidationAction
    if (schemaType != null) {
      ann = schemaType
      // test whether the value actually conforms to the given type
      val err: ValidationFailure = schemaType.validateContent(
        charSeq,
        DummyNamespaceResolver.getInstance,
        context.getConfiguration.getConversionRules)
      if (err != null) {
        val ve: ValidationFailure = new ValidationFailure(
          "Attribute value " + Err
            .wrap(charSeq, Err.VALUE) + " does not match the required type " +
            schemaType.getDescription +
            ". " +
            err.getMessage)
        ve.setSchemaType(schemaType)
        ve.setErrorCode("XTTE1540")
        throw ve.makeException()
      }
    } else if (validateAction == Validation.STRICT || validateAction == Validation.LAX) {
      try {
        val config: Configuration = context.getConfiguration
        ann = config.validateAttribute(attName.getStructuredQName,
          charSeq,
          validateAction)
      } catch {
        case e: ValidationException => {
          val err = XPathException.makeXPathException(e)
          err.maybeSetErrorCode(
            if (validateAction == Validation.STRICT) "XTTE1510"
            else "XTTE1515")
          err.setXPathContext(context)
          err.maybeSetLocation(getLocation)
          err.setIsTypeError(true)
          throw err
        }

      }
    } else {
      ann = BuiltInAtomicType.UNTYPED_ATOMIC
    }
    if (attName == StandardNames.XML_ID_NAME) {
      charSeq = Whitespace.collapseWhitespace(charSeq)
    }
    output.attribute(attName, ann, charSeq, getLocation, opt)
  }

  // we may need to change the namespace prefix if the one we chose is
  // already in use with a different namespace URI: this is done behind the scenes
  // by the ComplexContentOutputter
  // we may need to change the namespace prefix if the one we chose is
  // already in use with a different namespace URI: this is done behind the scenes
  // by the ComplexContentOutputter

   def validateOrphanAttribute(orphan: Orphan,
                                        context: XPathContext): Unit = {
    val rules: ConversionRules = context.getConfiguration.getConversionRules
    val schemaType: SimpleType = getSchemaType
    val validateAction: Int = getValidationAction
    if (schemaType != null) {
      val err: ValidationFailure = schemaType.validateContent(
        orphan.getStringValueCS,
        DummyNamespaceResolver.getInstance,
        rules)
      if (err != null) {
        err.setMessage(
          "Attribute value " + Err.wrap(orphan.getStringValueCS, Err.VALUE) +
            " does not the match the required type " +
            schemaType.getDescription +
            ". " +
            err.getMessage)
        err.setErrorCode("XTTE1555")
        err.setLocator(getLocation)
        throw err.makeException()
      }
      orphan.setTypeAnnotation(schemaType)
      if (schemaType.isNamespaceSensitive) {
        throw new XPathException(
          "Cannot validate a parentless attribute whose content is namespace-sensitive",
          "XTTE1545")
      }
    } else if (validateAction == Validation.STRICT || validateAction == Validation.LAX) {
      try {
        val controller: Controller = context.getController
        assert(controller != null)
        val ann: SimpleType = controller.getConfiguration.validateAttribute(
          NameOfNode.makeName(orphan).getStructuredQName,
          orphan.getStringValueCS,
          validateAction)
        orphan.setTypeAnnotation(ann)
      } catch {
        case e: ValidationException => {
          val err = XPathException.makeXPathException(e)
          err.setErrorCodeQName(e.getErrorCodeQName)
          err.setXPathContext(context)
          err.setLocation(getLocation)
          err.setIsTypeError(true)
          throw err
        }

      }
    }
  }

}

// Copyright (c) 2018-2020 Saxonica Limited
// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is "Incompatible With Secondary Licenses", as defined by the Mozilla Public License, v. 2.0.
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
/**
 * Abstract class for fixed and computed attribute constructor expressions
 */
