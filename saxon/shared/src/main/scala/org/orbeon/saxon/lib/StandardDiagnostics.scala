package org.orbeon.saxon.lib

import java.{util => ju}

import javax.xml.transform.SourceLocator
import org.orbeon.saxon.expr._
import org.orbeon.saxon.expr.instruct._
import org.orbeon.saxon.expr.parser.{Loc, XPathParser}
import org.orbeon.saxon.lib.StandardDiagnostics._
import org.orbeon.saxon.model.{Type, ValidationException, ValidationFailure}
import org.orbeon.saxon.om.{NodeInfo, StandardNames, StructuredQName}
import org.orbeon.saxon.regex.{BMPString, GeneralUnicodeString, LatinString, UnicodeString}
import org.orbeon.saxon.s9api.Location
import org.orbeon.saxon.serialize.charcode.UTF16CharacterSet
import org.orbeon.saxon.trans.rules.{BuiltInRuleSet, Rule}
import org.orbeon.saxon.trans.{Err, Mode}
import org.orbeon.saxon.tree.AttributeLocation
import org.orbeon.saxon.tree.util.{FastStringBuffer, Navigator}
import org.orbeon.saxon.utils.Controller

//import scala.collection.compat._
import scala.jdk.CollectionConverters._


object StandardDiagnostics {

  def getInstructionNameDefault(inst: Instruction): String =
    try {
      inst match {
        case _: FixedElement =>
          val qName: StructuredQName = inst.getObjectName
          "element constructor <" + qName.getDisplayName + '>'
        case _: FixedAttribute =>
          val qName: StructuredQName = inst.getObjectName
          "attribute constructor " + qName.getDisplayName + "=\"{...}\""
        case _ =>
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
    sorLoc match {
      case location: XPathParser.NestedLocation =>
        sorLoc = location.getContainingLocation
      case _ =>
    }
    sorLoc match {
      case saLoc: AttributeLocation =>
        nodeMessage = "in " + saLoc.getElementName.getDisplayName
        if (saLoc.getAttributeName != null) {
          nodeMessage += "/@" + saLoc.getAttributeName
        }
        nodeMessage += ' '
      // ORBEON: No W3C DOM.
      //    } else if (sorLoc.isInstanceOf[DOMLocator]) {
      //      nodeMessage = "at " +
      //        sorLoc.asInstanceOf[DOMLocator].getOriginatingNode.getNodeName +
      //        ' '
      case info: NodeInfo =>
        node = info
        nodeMessage = "at " + node.getDisplayName + ' '
      case exception: ValidationException if exception.getNode != null =>
        node = exception.getNode
        nodeMessage = "at " + node.getDisplayName + ' '
      case exception: ValidationException if exception.getPath != null && sorLoc.getLineNumber == -1 =>
        path = exception.getPath
        nodeMessage = "at " + path + ' '
      case instruction: Instruction =>
        val instructionName: String = getInstructionName(
          instruction)
        if ("" != instructionName) {
          nodeMessage = "at " + instructionName + ' '
        }
        systemId = sorLoc.getSystemId
        lineNumber = sorLoc.getLineNumber
      case actor: Actor =>
        var kind: String = "procedure"
        sorLoc match {
          case _: UserFunction =>
            kind = "function"
          case _: NamedTemplate =>
            kind = "template"
          //      } else if (sorLoc.isInstanceOf[AttributeSet]) {
          //        kind = "attribute-set"
          case _: GlobalParam =>
            kind = "parameter"
          case _: GlobalVariable =>
            kind = "variable"
          case _ =>
        }
        systemId = sorLoc.getSystemId
        lineNumber = sorLoc.getLineNumber
        nodeMessage = "at " + kind + " "
        val name: StructuredQName = actor.getComponentName
        if (name != null) {
          nodeMessage += name.toString
          nodeMessage += " "
        }
      case _ =>
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
                .append(rule.getPattern.toShortString)
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
          case _: Exception =>
        }
        while (! xPathContext.isInstanceOf[XPathContextMajor])
          xPathContext = xPathContext.getCaller
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
    } else originator match {
      case instruction: Instruction =>
        sb.append(getInstructionName(instruction))
      case _: UserFunctionCall =>
        sb.append("function call")
      case _: Controller =>
        sb.append("external application")
      case set: BuiltInRuleSet =>
        sb.append("built-in template rule (")
          .append(set.getName)
          .append(")")
      case param: GlobalParam =>
        sb.append("global parameter ")
          .append(
            param.getVariableQName.getDisplayName)
      case variable: GlobalVariable =>
        sb.append(variable.getDescription)
      case _ =>
        sb.append("unknown caller (").append(originator.getClass).append(")")
    }
    originator match {
      case locatable: Locatable =>
        val loc: Location = locatable.getLocation
        if (loc.getLineNumber != -1) {
          sb.append(" at ")
            .append(
              if (loc.getSystemId == null) "line " else loc.getSystemId + "#")
          sb.append(loc.getLineNumber)
        }
      case _ =>
    }
    sb.toString
  }

  def formatListOfOffendingNodes(
                                  failure: ValidationFailure): String = {
    val message: StringBuilder = new StringBuilder()
    val offendingNodes: ju.List[NodeInfo] = failure.getOffendingNodes
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
    val fsb = new FastStringBuffer(str.uLength * 2)
    for (i <- 0 until str.uLength) {
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
