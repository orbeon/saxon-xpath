package net.sf.saxon.lib

import java.util.List

import javax.xml.transform.SourceLocator
import javax.xml.transform.dom.DOMLocator
import net.sf.saxon.expr._
import net.sf.saxon.expr.instruct._
import net.sf.saxon.expr.parser.{Loc, XPathParser}
import net.sf.saxon.lib.StandardDiagnostics._
import net.sf.saxon.model.{Type, ValidationException, ValidationFailure}
import net.sf.saxon.om.{NodeInfo, StandardNames, StructuredQName}
import net.sf.saxon.regex.{BMPString, GeneralUnicodeString, LatinString, UnicodeString}
import net.sf.saxon.s9api.Location
import net.sf.saxon.serialize.charcode.UTF16CharacterSet
import net.sf.saxon.trans.{Err, Mode}
import net.sf.saxon.trans.rules.{BuiltInRuleSet, Rule}
import net.sf.saxon.tree.AttributeLocation
import net.sf.saxon.tree.util.{FastStringBuffer, Navigator}
import net.sf.saxon.utils.Controller

import scala.jdk.CollectionConverters._

object StandardDiagnostics {

  def getInstructionNameDefault(inst: Instruction): String =
    try {
      if (inst.isInstanceOf[FixedElement]) {
        val qName: StructuredQName = inst.getObjectName
        "element constructor <" + qName.getDisplayName + '>'
      } else if (inst.isInstanceOf[FixedAttribute]) {
        val qName: StructuredQName = inst.getObjectName
        "attribute constructor " + qName.getDisplayName + "=\"{...}\""
      }
      val construct: Int = inst.getInstructionNameCode
      if (construct < 0) {
        return ""
      }
      if (construct < 1024 && construct != StandardNames.XSL_FUNCTION &&
        construct != StandardNames.XSL_TEMPLATE) {
        if (inst.getPackageData.isXSLT) {
          StandardNames.getDisplayName(construct)
        } else {
          val s: String = StandardNames.getDisplayName(construct)
          val colon: Int = s.indexOf(':')
          if (colon > 0) {
            val local: String = s.substring(colon + 1)
            if (local.==("document")) {
              return "document node constructor"
            } else if (local.==("text") || s.==("value-of")) {
              return "text node constructor"
            } else if (local.==("element")) {
              return "computed element constructor"
            } else if (local.==("attribute")) {
              return "computed attribute constructor"
            } else if (local.==("variable")) {
              return "variable declaration"
            } else if (local.==("param")) {
              return "external variable declaration"
            } else if (local.==("comment")) {
              return "comment constructor"
            } else if (local.==("processing-instruction")) {
              return "processing-instruction constructor"
            } else if (local.==("namespace")) {
              return "namespace node constructor"
            }
          }
          s
        }
      } else {
        ""
      }
    } catch {
      case err: Exception => ""

    }

  def abbreviateLocationURIDefault(uri: String): String = {
    if (uri == null) {
      return "*unknown*"
    }
    val slash: Int = uri.lastIndexOf('/')
    if (slash >= 0 && slash < uri.length - 1) {
      uri.substring(slash + 1)
    } else {
      uri
    }
  }

}

class StandardDiagnostics {

  def getLocationMessageText(loc: SourceLocator): String = {
    var sorLoc = loc
    var locMessage: String = ""
    var systemId: String = null
    var node: NodeInfo = null
    var path: String = null
    var nodeMessage: String = null
    var lineNumber: Int = -1
    if (sorLoc == null) {
      sorLoc = Loc.NONE
    }
    if (sorLoc.isInstanceOf[XPathParser.NestedLocation]) {
      sorLoc = sorLoc.asInstanceOf[XPathParser.NestedLocation].getContainingLocation
    }
    if (sorLoc.isInstanceOf[AttributeLocation]) {
      val saLoc: AttributeLocation = sorLoc.asInstanceOf[AttributeLocation]
      nodeMessage = "in " + saLoc.getElementName.getDisplayName
      if (saLoc.getAttributeName != null) {
        nodeMessage += "/@" + saLoc.getAttributeName
      }
      nodeMessage += ' '
    } else if (sorLoc.isInstanceOf[DOMLocator]) {
      nodeMessage = "at " +
        sorLoc.asInstanceOf[DOMLocator].getOriginatingNode.getNodeName +
        ' '
    } else if (sorLoc.isInstanceOf[NodeInfo]) {
      node = sorLoc.asInstanceOf[NodeInfo]
      nodeMessage = "at " + node.getDisplayName + ' '
    } else if (sorLoc.isInstanceOf[ValidationException] &&
      sorLoc.asInstanceOf[ValidationException].getNode != null) {
      node = sorLoc.asInstanceOf[ValidationException].getNode
      nodeMessage = "at " + node.getDisplayName + ' '
    } else if (sorLoc.isInstanceOf[ValidationException] && sorLoc.getLineNumber == -1 &&
      sorLoc.asInstanceOf[ValidationException].getPath != null) {
      path = sorLoc.asInstanceOf[ValidationException].getPath
      nodeMessage = "at " + path + ' '
    } else if (sorLoc.isInstanceOf[Instruction]) {
      val instructionName: String = getInstructionName(
        sorLoc.asInstanceOf[Instruction])
      if ("" != instructionName) {
        nodeMessage = "at " + instructionName + ' '
      }
      systemId = sorLoc.getSystemId
      lineNumber = sorLoc.getLineNumber
    } else if (sorLoc.isInstanceOf[Actor]) {
      var kind: String = "procedure"
      if (sorLoc.isInstanceOf[UserFunction]) {
        kind = "function"
      } else if (sorLoc.isInstanceOf[NamedTemplate]) {
        kind = "template"
        //      } else if (sorLoc.isInstanceOf[AttributeSet]) {
        //        kind = "attribute-set"
      } /*else if (sorLoc.isInstanceOf[KeyDefinition]) { // KeyDefinition not found
        kind = "key"
      }*/ else if (sorLoc.isInstanceOf[GlobalParam]) {
        kind = "parameter"
      } else if (sorLoc.isInstanceOf[GlobalVariable]) {
        kind = "variable"
      }
      systemId = sorLoc.getSystemId
      lineNumber = sorLoc.getLineNumber
      nodeMessage = "at " + kind + " "
      val name: StructuredQName = sorLoc.asInstanceOf[Actor].getComponentName
      if (name != null) {
        nodeMessage += name.toString
        nodeMessage += " "
      }
    }
    if (lineNumber == -1) {
      lineNumber = sorLoc.getLineNumber
    }
    val containsLineNumber: Boolean = lineNumber != -1
    if (node != null && !containsLineNumber) {
      nodeMessage = "at " + Navigator.getPath(node) + ' '
    }
    if (nodeMessage != null) {
      locMessage += nodeMessage
    }
    if (containsLineNumber) {
      locMessage += "on line " + lineNumber + ' '
      if (sorLoc.getColumnNumber != -1) {
        locMessage += "column " + sorLoc.getColumnNumber + ' '
      }
    }
    if (systemId != null && systemId.isEmpty) {
      systemId = null
    }
    if (systemId == null) {
      try systemId = sorLoc.getSystemId
      catch {
        case err: Exception => err.printStackTrace()

      }
    }
    if (systemId != null && !systemId.isEmpty) {
      locMessage += (if (containsLineNumber) "of " else "in ") + abbreviateLocationURI(
        systemId) +
        ':'
    }
    locMessage
  }

  def getInstructionName(inst: Instruction): String =
    getInstructionNameDefault(inst)

  def printStackTrace(context: XPathContext, out: Logger, level: Int): Unit = {
    var xPathContext = context
    if (level > 0) {
      var depth: Int = 20
      depth -= 1
      while (depth > 0) {
        val component: Component = xPathContext.getCurrentComponent
        if (component != null) {
          if (component.getActor.isInstanceOf[Mode]) {
            val rule: Rule = xPathContext.getCurrentTemplateRule
            if (rule != null) {
              val sb: StringBuilder = new StringBuilder()
              val loc: Location = rule.getPattern.getLocation
              sb.append("  In template rule with match=\"")
                .append(rule.getPattern.toShortString())
                .append("\" ")
              if (loc != null && loc.getLineNumber != -1) {
                sb.append("on line ").append(loc.getLineNumber).append(" ")
              }
              if (loc != null && loc.getSystemId != null) {
                sb.append("of ").append(abbreviateLocationURI(loc.getSystemId))
              }
              out.error(sb.toString)
            }
          } else {
            out.error(
              getLocationMessageText(component.getActor.asInstanceOf[Location])
                .replace("$at ", "In "))
          }
        }
        try xPathContext.getStackFrame.getStackFrameMap.showStackFrame(xPathContext, out)
        catch {
          case e: Exception => {}

        }
        while (!(xPathContext.isInstanceOf[XPathContextMajor])) xPathContext =
          xPathContext.getCaller
        val originator: ContextOriginator =
          xPathContext.asInstanceOf[XPathContextMajor].getOrigin
        if (originator == null || originator.isInstanceOf[Controller]) {
          return
        } else {
          out.error("     invoked by " + showOriginator(originator))
        }
        xPathContext = xPathContext.getCaller
      }
    }
  }

  def showOriginator(originator: ContextOriginator): String = {
    val sb: StringBuilder = new StringBuilder()
    if (originator == null) {
      sb.append("unknown caller (null)")
    } else if (originator.isInstanceOf[Instruction]) {
      sb.append(getInstructionName(originator.asInstanceOf[Instruction]))
    } else if (originator.isInstanceOf[UserFunctionCall]) {
      sb.append("function call")
    } else if (originator.isInstanceOf[Controller]) {
      sb.append("external application")
    } else if (originator.isInstanceOf[BuiltInRuleSet]) {
      sb.append("built-in template rule (")
        .append(originator.asInstanceOf[BuiltInRuleSet].getName)
        .append(")")
    } /*else if (originator.isInstanceOf[KeyDefinition]) {  // KeyDefinition class not found
      sb.append("xsl:key definition")
    } */
    else if (originator.isInstanceOf[GlobalParam]) {
      sb.append("global parameter ")
        .append(
          originator.asInstanceOf[GlobalParam].getVariableQName.getDisplayName)
    } else if (originator.isInstanceOf[GlobalVariable]) {
      sb.append(originator.asInstanceOf[GlobalVariable].getDescription)
    } else {
      sb.append("unknown caller (").append(originator.getClass).append(")")
    }
    if (originator.isInstanceOf[Locatable]) {
      val loc: Location = originator.asInstanceOf[Locatable].getLocation
      if (loc.getLineNumber != -1) {
        sb.append(" at ")
          .append(
            if (loc.getSystemId == null) "line " else (loc.getSystemId + "#"))
        sb.append(loc.getLineNumber)
      }
    }
    sb.toString
  }

  def formatListOfOffendingNodes(
                                  failure: ValidationFailure): String = {
    val message: StringBuilder = new StringBuilder()
    val offendingNodes: List[NodeInfo] = failure.getOffendingNodes
    if (!offendingNodes.isEmpty) {
      message.append("\n  Nodes for which the assertion fails:")
      for (offender <- offendingNodes.asScala) {
        var nodeDesc: String = Type.displayTypeName(offender)
        if (offender.getNodeKind == Type.TEXT) {
          nodeDesc += " " + Err.wrap(offender.getStringValueCS, Err.VALUE)
        }
        if (offender.getLineNumber != -1) {
          nodeDesc += " on line " + offender.getLineNumber
          if (offender.getColumnNumber != -1) {
            nodeDesc += " column " + offender.getColumnNumber
          }
          if (offender.getSystemId != null) {
            nodeDesc += " of " + offender.getSystemId
          }
        } else {
          nodeDesc += " at " + Navigator.getPath(offender)
        }
        message.append("\n  * ").append(nodeDesc)
      }
    }
    message.toString
  }

  def abbreviateLocationURI(uri: String): String =
    abbreviateLocationURIDefault(uri)

  var MAX_MESSAGE_LENGTH: Int = 1000

  var MAX_MESSAGE_LINE_LENGTH: Int = 100

  var MIN_MESSAGE_LINE_LENGTH: Int = 10

  var TARGET_MESSAGE_LINE_LENGTH: Int = 90

  def wordWrap(message: String): String = {
    var msgStr = message
    if (msgStr.length > MAX_MESSAGE_LENGTH) {
      msgStr = msgStr.substring(0, MAX_MESSAGE_LENGTH)
    }
    var nl: Int = msgStr.indexOf('\n')
    if (nl < 0) {
      nl = msgStr.length
    }
    if (nl > MAX_MESSAGE_LINE_LENGTH) {
      var i: Int = TARGET_MESSAGE_LINE_LENGTH
      while (msgStr.charAt(i) != ' ' && i > 0) {
        i -= 1
      }
      if (i > MIN_MESSAGE_LINE_LENGTH) {
        msgStr.substring(0, i) + "\n  " + wordWrap(msgStr.substring(i + 1))
      } else {
        msgStr
      }
    } else if (nl < msgStr.length) {
      msgStr.substring(0, nl) + '\n' + wordWrap(msgStr.substring(nl + 1))
    } else {
      msgStr
    }
  }

  def expandSpecialCharacters(in: CharSequence, threshold: Int): CharSequence = {
    if (threshold >= UTF16CharacterSet.NONBMP_MAX) {
      return in
    }
    var max: Int = 0
    var isAstral: Boolean = false
    for (i <- 0 until in.length) {
      val c: Char = in.charAt(i)
      if (c > max) {
        max = c
      }
      if (UTF16CharacterSet.isSurrogate(c)) {
        isAstral = true
      }
    }
    if (max <= threshold && !isAstral) {
      return in
    }
    var str: UnicodeString = null
    str =
      if (max <= 255) new LatinString(in)
      else if (!isAstral) new BMPString(in)
      else new GeneralUnicodeString(in)
    val fsb: FastStringBuffer = new FastStringBuffer(str.uLength() * 2)
    for (i <- 0 until str.uLength()) {
      val ch: Int = str.uCharAt(i)
      fsb.appendWideChar(ch)
      if (ch > threshold) {
        fsb.append("[x")
        fsb.append(java.lang.Integer.toHexString(ch))
        fsb.append("]")
      }
    }
    fsb
  }

}
