package net.sf.saxon.trace

import net.sf.saxon.expr.XPathContext

import net.sf.saxon.expr.instruct._

import net.sf.saxon.lib.Logger

import net.sf.saxon.lib.NamespaceConstant

import net.sf.saxon.ma.arrays.ArrayItem

import net.sf.saxon.ma.map.MapItem

import net.sf.saxon.om.Function

import net.sf.saxon.om.Item

import net.sf.saxon.om.NodeInfo

import net.sf.saxon.om.StructuredQName

import net.sf.saxon.s9api.Location

import net.sf.saxon.tree.util.Navigator

import net.sf.saxon.value.AtomicValue

import scala.beans.{BeanProperty, BooleanBeanProperty}

object ContextStackFrame {

  class CallingApplication extends ContextStackFrame {

    def print(out: Logger): Unit = {}

  }

  class BuiltInTemplateRule(private var xPathcontext: XPathContext)
    extends ContextStackFrame {

    def print(out: Logger): Unit = {
      val contextItem: Item = xPathcontext.getContextItem
      var diag: String = null
      diag =
        if (contextItem.isInstanceOf[NodeInfo])
          Navigator.getPath(contextItem.asInstanceOf[NodeInfo])
        else if (contextItem.isInstanceOf[AtomicValue])
          "value " + contextItem.toString
        else if (contextItem.isInstanceOf[MapItem]) "map"
        else if (contextItem.isInstanceOf[ArrayItem]) "array"
        else if (contextItem.isInstanceOf[Function]) "function"
        else "item"
      out.error(
        "  in built-in template rule for " + diag + " in " +
          xPathcontext.getCurrentMode.getActor.getModeTitle.toLowerCase())
    }

  }

  class FunctionCall extends ContextStackFrame {

    var functionName: StructuredQName = _

    def getFunctionName(): StructuredQName = functionName

    def setFunctionName(functionName: StructuredQName): Unit = {
      this.functionName = functionName
    }

    def print(out: Logger): Unit = {
      out.error(
        "  at " +
          (if (functionName == null) "(anonymous)"
          else functionName.getDisplayName) +
          "() " +
          showLocation())
    }

  }

  class ApplyTemplates extends ContextStackFrame {

    def print(out: Logger): Unit = {
      out.error("  at xsl:apply-templates " + showLocation())
      val node: Item = getContextItem
      if (node.isInstanceOf[NodeInfo]) {
        out.error(
          "     processing " + Navigator.getPath(node.asInstanceOf[NodeInfo]))
      }
    }

  }

  class CallTemplate extends ContextStackFrame {

    def getTemplateName(): StructuredQName = templateName

    def setTemplateName(templateName: StructuredQName): Unit = {
      this.templateName = templateName
    }

    var templateName: StructuredQName = _

    def print(out: Logger): Unit = {
      val name: String =
        if (templateName == null) "??" else templateName.getDisplayName
      out.error(
        "  at xsl:call-template name=\"" + name + "\" " + showLocation())
    }

  }

  class VariableEvaluation extends ContextStackFrame {

    def getVariableName(): StructuredQName = variableName

    def setVariableName(variableName: StructuredQName): Unit = {
      this.variableName = variableName
    }

    var variableName: StructuredQName = _

    def print(out: Logger): Unit = {
      out.error(
        "  in " + displayContainer(getContainer) + " " + showLocation())
    }

  }

  private def displayContainer(container: AnyRef): String = {
    if (container.isInstanceOf[Actor]) {
      val name: StructuredQName =
        container.asInstanceOf[Actor].getComponentName
      val objectName: String = if (name == null) "" else name.getDisplayName
      if (container.isInstanceOf[NamedTemplate]) {
        return "template name=\"" + objectName + "\""
      } else if (container.isInstanceOf[UserFunction]) {
        return "function " + objectName + "()"
//      } else if (container.isInstanceOf[AttributeSet]) {
//        return "attribute-set " + objectName
      } /*else if (container.isInstanceOf[KeyDefinition]) { // KeyDefinition not exist
        return "key " + objectName
      }*/ else if (container.isInstanceOf[GlobalVariable]) {
        val qName: StructuredQName =
          container.asInstanceOf[GlobalVariable].getVariableQName
        if (qName.hasURI(NamespaceConstant.SAXON_GENERATED_VARIABLE)) {
          return "optimizer-created global variable"
        } else {
          return "global variable $" + qName.getDisplayName
        }
      }
    } else if (container.isInstanceOf[TemplateRule]) {
      return "template match=\"" +
        container.asInstanceOf[TemplateRule].getMatchPattern.toString +
        "\""
    }
    ""
  }

}

abstract class ContextStackFrame {

  @BeanProperty
  var context: XPathContext = _

  private var location: Location = _

  @BeanProperty
  var contextItem: Item = _

  @BeanProperty
  var container: AnyRef = _

  def setLocation(loc: Location): Unit = {
    this.location = loc
  }

  def getSystemId(): String = location.getSystemId

  def getLineNumber(): Int = location.getLineNumber

  def setComponent(container: AnyRef): Unit = {
    this.container = container
  }

  def print(out: Logger): Unit

   def showLocation(): String = {
    if (getSystemId == null) {
      return ""
    }
    val line: Int = getLineNumber
    if (line == -1 || line == 0xfffff) {
      "(" + getSystemId + ")"
    } else {
      "(" + getSystemId + "#" + getLineNumber + ")"
    }
  }

}
