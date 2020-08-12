////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
package net.sf.saxon.functions.registry

import javax.xml.transform.SourceLocator
import net.sf.saxon.expr.{Expression, Literal, XPathContext}
import net.sf.saxon.expr.parser.{ContextItemStaticInfo, ExpressionVisitor, XPathParser}
import net.sf.saxon.functions.{ApplyFn, Doc_2, SystemFunction}
import net.sf.saxon.functions.registry.BuiltInFunctionSet._
import net.sf.saxon.functions.registry.VendorFunctionSetHE._
import net.sf.saxon.lib.NamespaceConstant
import net.sf.saxon.ma.arrays.ArrayItemType
import net.sf.saxon.ma.map.{MapCreate, MapType, MapUntypedContains}
import net.sf.saxon.model._
import net.sf.saxon.om.{NodeInfo, Sequence, StructuredQName}
import net.sf.saxon.pattern.NodeKindTest
import net.sf.saxon.trans.XPathException
import net.sf.saxon.tree.tiny.TinyElementImpl
import net.sf.saxon.value._

//remove if not needed

object VendorFunctionSetHE {

  private var THE_INSTANCE: VendorFunctionSetHE = new VendorFunctionSetHE()

  def getInstance(): VendorFunctionSetHE = THE_INSTANCE

  class IsWholeNumberFn extends SystemFunction {

    /**
      * Allow the function to create an optimized call based on the values of the actual arguments
      *
      * @param visitor     the expression visitor
      * @param contextInfo information about the context item
      * @param arguments   the supplied arguments to the function call. Note: modifying the contents
      *                    of this array should not be attempted, it is likely to have no effect.
      * @return either a function call on this function, or an expression that delivers
      * the same result, or null indicating that no optimization has taken place
      * @throws XPathException if an error is detected
      */
    override def makeOptimizedFunctionCall(
        visitor: ExpressionVisitor,
        contextInfo: ContextItemStaticInfo,
        arguments: Expression*): Expression = {
      if (arguments(0).getItemType.getPrimitiveItemType == BuiltInAtomicType.INTEGER) {
        Literal.makeLiteral(BooleanValue.TRUE)
      }
      null
    }

    def call(context: XPathContext, arguments: Array[Sequence]): Sequence = {
      val `val`: NumericValue = arguments(0).head().asInstanceOf[NumericValue]
      BooleanValue.get(`val` != null && `val`.isWholeNumber)
    }

  }

  class HasLocalNamespaces extends SystemFunction {

    def call(context: XPathContext, arguments: Array[Sequence]): BooleanValue = {
      val child: NodeInfo = arguments(0).head().asInstanceOf[NodeInfo]
      val parent: NodeInfo = child.getParent
      BooleanValue.get(
        parent == null || parent.getNodeKind == Type.DOCUMENT ||
          child.getAllNamespaces != parent.getAllNamespaces)
    }

  }

  class HasUniformNamespaces extends SystemFunction {

    def call(context: XPathContext, arguments: Array[Sequence]): BooleanValue = {
      val `val`: NodeInfo = arguments(0).head().asInstanceOf[NodeInfo]
      if (`val`.isInstanceOf[TinyElementImpl]) {
        BooleanValue.get(
          `val`.asInstanceOf[TinyElementImpl].hasUniformNamespaces())
      } else {
        BooleanValue.FALSE
      }
    }

  }

  class DynamicErrorInfoFn extends SystemFunction {

    /**
      * Determine the special properties of this function. The general rule
      * is that a system function call is non-creative unless more details
      * are defined in a subclass.
      *
      * @param arguments the actual arguments supplied in a call to the function
      */
    override def getSpecialProperties(arguments: Array[Expression]): Int = // treat as creative to avoid loop-lifting: test case try-catch-err-code-variable-14
      0

    def call(context: XPathContext, arguments: Array[Sequence]): Sequence = {
      val `var`: String = arguments(0).head().getStringValue
      val error: XPathException = context.getCurrentException
      if (error == null) {
        EmptySequence.getInstance
      }
      val locator: SourceLocator = error.getLocator
      `var` match {
        case "code" =>
          var errorCodeQName: StructuredQName = error.getErrorCodeQName
          if (errorCodeQName == null) {
            errorCodeQName =
              new StructuredQName("saxon", NamespaceConstant.SAXON, "XXXX9999")
          }
          new QNameValue(errorCodeQName, BuiltInAtomicType.QNAME)
        case "description" =>
          var s: String = error.getMessage
          if (error.getCause != null) {
            s += "(" + error.getCause.getMessage + ")"
          }
          new StringValue(s)
        case "value" =>
          var value: Sequence = error.getErrorObject
          if (value == null) {
            EmptySequence.getInstance
          } else {
            value
          }
        case "module" =>
          var module: String =
            if (locator == null) null else locator.getSystemId
          if (module == null) {
            EmptySequence.getInstance
          } else {
            new StringValue(module)
          }
        case "line-number" =>
          var line: Int = if (locator == null) -1 else locator.getLineNumber
          if (line == -1) {
            EmptySequence.getInstance
          } else {
            new Int64Value(line)
          }
        case "column-number" =>
// Bug 4144
          var column: Int = -1
          if (locator == null) {
            EmptySequence.getInstance
          } else
            column =
              if (locator.isInstanceOf[XPathParser.NestedLocation])
                locator
                  .asInstanceOf[XPathParser.NestedLocation]
                  .getContainingLocation
                  .getColumnNumber
              else locator.getColumnNumber
          if (column == -1) {
            EmptySequence.getInstance
          } else {
            new Int64Value(column)
          }
        case _ => EmptySequence.getInstance

      }
    }

  }

}

/**
  * Implementation of vendor functions in the Saxon namespace, available in Saxon-HE because they
  * are used internally. This library is available in all Saxon versions.
  */
class VendorFunctionSetHE private () extends BuiltInFunctionSet {

  init()

  private def init(): Unit = {
// Test whether supplied argument is equal to an integer
    register("is-whole-number",
             1,
             classOf[IsWholeNumberFn],
             BuiltInAtomicType.BOOLEAN,
             ONE,
             0).arg(0, NumericType.getInstance, OPT, EMPTY)
// Evaluate the value of a try-catch variable such as $err:code
    register("dynamic-error-info",
             1,
             classOf[DynamicErrorInfoFn],
             AnyItemType.getInstance,
             STAR,
             FOCUS | LATE | SIDE).arg(0, BuiltInAtomicType.STRING, ONE, null)
// saxon:apply is the same as fn:apply, but does not require the HOF feature
    register("apply", 2, classOf[ApplyFn], AnyItemType.getInstance, STAR, LATE)
      .arg(0, AnyFunctionType.getInstance, ONE, null)
      .arg(1, ArrayItemType.ANY_ARRAY_TYPE, ONE, null)
// Create a map according to the semantics of the XPath map constructor and XSLT xsl:map instruction
    register("create-map", 1, classOf[MapCreate], MapType.ANY_MAP_TYPE, ONE, 0)
      .arg(0, MapType.ANY_MAP_TYPE, STAR, null)
// Variant of the doc() function with an options parameter
    register("doc", 2, classOf[Doc_2], NodeKindTest.DOCUMENT, ONE, LATE)
      .arg(0, BuiltInAtomicType.STRING, ONE, null)
      .arg(1, MapType.ANY_MAP_TYPE, ONE, EMPTY)
      .optionDetails(Doc_2.makeOptionsParameter())
// Ask whether the supplied element node has any local namespace declarations
    register("has-local-namespaces",
             1,
             classOf[HasLocalNamespaces],
             BuiltInAtomicType.BOOLEAN,
             ONE,
             0).arg(0, NodeKindTest.ELEMENT, ONE, null)
// Ask whether the supplied element node has consistent in scope namespaces throughout its subtree
    register("has-uniform-namespaces",
             1,
             classOf[HasUniformNamespaces],
             BuiltInAtomicType.BOOLEAN,
             ONE,
             0).arg(0, NodeKindTest.ELEMENT, ONE, null)
// Function analogous to map:contains except in the way it handles untyped key values
    register("map-untyped-contains",
             2,
             classOf[MapUntypedContains],
             BuiltInAtomicType.BOOLEAN,
             ONE,
             0)
      .arg(0, MapType.ANY_MAP_TYPE, STAR, null)
      .arg(1, BuiltInAtomicType.ANY_ATOMIC, ONE, null)
  }

  override def getNamespace(): String = NamespaceConstant.SAXON

  override def getConventionalPrefix(): String = "saxon"

}

// Copyright (c) 2018-2020 Saxonica Limited
// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is "Incompatible With Secondary Licenses", as defined by the Mozilla Public License, v. 2.0.
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// Copyright (c) 2018-2020 Saxonica Limited
