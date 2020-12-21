package org.orbeon.saxon.expr.instruct

import org.orbeon.saxon.utils.Controller

import org.orbeon.saxon.event.ComplexContentOutputter

import org.orbeon.saxon.event.Outputter

import org.orbeon.saxon.event.PipelineConfiguration

import org.orbeon.saxon.event.ReceiverOption

import org.orbeon.saxon.expr.Expression

import org.orbeon.saxon.expr.Operand

import org.orbeon.saxon.expr.OperandRole

import org.orbeon.saxon.expr.XPathContext

import org.orbeon.saxon.expr.parser.RebindingMap

import org.orbeon.saxon.model.Type

import org.orbeon.saxon.om.AxisInfo

import org.orbeon.saxon.om.NodeInfo

import org.orbeon.saxon.om.SequenceIterator

import org.orbeon.saxon.om.StandardNames

import org.orbeon.saxon.trace.ExpressionPresenter

import org.orbeon.saxon.trans.XPathException

import org.orbeon.saxon.tree.tiny.TinyBuilder

//import scala.collection.compat._
import scala.jdk.CollectionConverters._
import scala.util.control.Breaks._

class Doctype(content: Expression) extends Instruction {

  private var contentOp: Operand =
    new Operand(this, content, OperandRole.SINGLE_ATOMIC)

  def getContent: Expression = contentOp.getChildExpression

  def setContent(content: Expression): Unit = {
    contentOp.setChildExpression(content)
  }

  override def operands: java.lang.Iterable[Operand] = contentOp

  def copy(rebindings: RebindingMap): Expression =
    throw new UnsupportedOperationException("Doctype.copy()")

  override def mayCreateNewNodes(): Boolean = true

  override def getInstructionNameCode(): Int = StandardNames.SAXON_DOCTYPE

  def processLeavingTail(out: Outputter, context: XPathContext): TailCall = {
    val controller: Controller = context.getController
    val pipe: PipelineConfiguration = controller.makePipelineConfiguration
    pipe.setXPathContext(context)
    pipe.setHostLanguage(getPackageData.getHostLanguage)
    val builder: TinyBuilder = new TinyBuilder(pipe)
    builder.setStatistics(
      pipe.getConfiguration.getTreeStatistics.RESULT_TREE_STATISTICS)
    builder.open()
    builder.startDocument(ReceiverOption.NONE)
    getContent.process(
      ComplexContentOutputter.makeComplexContentReceiver(builder, null),
      context)
    builder.endDocument()
    builder.close()
    val dtdRoot: NodeInfo = builder.getCurrentRoot
    var children: SequenceIterator = dtdRoot.iterateAxis(AxisInfo.CHILD)
    val docType: NodeInfo = children.next().asInstanceOf[NodeInfo]
    if (docType == null || "doctype" != docType.getLocalPart) {
      val e: XPathException = new XPathException(
        "saxon:doctype instruction must contain dtd:doctype")
      e.setXPathContext(context)
      throw e
    }
    val name: String = docType.getAttributeValue("", "name")
    val system: String = docType.getAttributeValue("", "system")
    val publicid: String = docType.getAttributeValue("", "public")
    if (name == null) {
      val e: XPathException = new XPathException(
        "dtd:doctype must have a name attribute")
      e.setXPathContext(context)
      throw e
    }
    write(out, "<!DOCTYPE " + name + ' ')
    if (system != null) {
      if (publicid != null) {
        write(out, "PUBLIC \"" + publicid + "\" \"" + system + '\"')
      } else {
        write(out, "SYSTEM \"" + system + '\"')
      }
    }
    var openSquare: Boolean = false
    children = docType.iterateAxis(AxisInfo.CHILD)
    var child: NodeInfo = children.next().asInstanceOf[NodeInfo]
    if (child != null) {
      write(out, " [")
      openSquare = true
    }
    while (child != null) {
      val localname: String = child.getLocalPart
      if ("element" == localname) {
        val elname: String = child.getAttributeValue("", "name")
        val content: String = child.getAttributeValue("", "content")
        if (elname == null) {
          val e: XPathException = new XPathException(
            "dtd:element must have a name attribute")
          e.setXPathContext(context)
          throw e
        }
        if (content == null) {
          val e: XPathException = new XPathException(
            "dtd:element must have a content attribute")
          e.setXPathContext(context)
          throw e
        }
        write(out, "\n  <!ELEMENT " + elname + ' ' + content + '>')
      } else if (localname.==("attlist")) {
        val elname: String = child.getAttributeValue("", "element")
        if (elname == null) {
          val e: XPathException = new XPathException(
            "dtd:attlist must have an attribute named 'element'")
          e.setXPathContext(context)
          throw e
        }
        write(out, "\n  <!ATTLIST " + elname + ' ')
        val attributes: SequenceIterator = child.iterateAxis(AxisInfo.CHILD)
        breakable {
          while (true) {
            val attDef: NodeInfo = attributes.next().asInstanceOf[NodeInfo]
            if (attDef == null) {
              break()
            }
            if ("attribute" == attDef.getLocalPart) {
              val atname: String = attDef.getAttributeValue("", "name")
              val `type`: String = attDef.getAttributeValue("", "type")
              val value: String = attDef.getAttributeValue("", "value")
              if (atname == null) {
                val e: XPathException = new XPathException(
                  "dtd:attribute must have a name attribute")
                e.setXPathContext(context)
                throw e
              }
              if (`type` == null) {
                val e: XPathException = new XPathException(
                  "dtd:attribute must have a type attribute")
                e.setXPathContext(context)
                throw e
              }
              if (value == null) {
                val e: XPathException = new XPathException(
                  "dtd:attribute must have a value attribute")
                e.setXPathContext(context)
                throw e
              }
              write(out, "\n    " + atname + ' ' + `type` + ' ' + value)
            } else {
              val e: XPathException = new XPathException(
                "Unrecognized element within dtd:attlist")
              e.setXPathContext(context)
              throw e
            }
          }
        }
        write(out, ">")
      } else if (localname.==("entity")) {
        val entname: String = child.getAttributeValue("", "name")
        val parameter: String = child.getAttributeValue("", "parameter")
        val esystem: String = child.getAttributeValue("", "system")
        val epublicid: String = child.getAttributeValue("", "public")
        val notation: String = child.getAttributeValue("", "notation")
        if (entname == null) {
          val e: XPathException = new XPathException(
            "dtd:entity must have a name attribute")
          e.setXPathContext(context)
          throw e
        }
        write(out, "\n  <!ENTITY ")
        if ("yes" == parameter) {
          write(out, "% ")
        }
        write(out, entname + ' ')
        if (esystem != null) {
          if (epublicid != null) {
            write(out, "PUBLIC \"" + epublicid + "\" \"" + esystem + "\" ")
          } else {
            write(out, "SYSTEM \"" + esystem + "\" ")
          }
        }
        if (notation != null) {
          write(out, "NDATA " + notation + ' ')
        }
        for (content <- child.children) {
          content.copy(out, 0, getLocation)
        }
        write(out, ">")
      } else if (localname.==("notation")) {
        val notname: String = child.getAttributeValue("", "name")
        val nsystem: String = child.getAttributeValue("", "system")
        val npublicid: String = child.getAttributeValue("", "public")
        if (notname == null) {
          val e: XPathException = new XPathException(
            "dtd:notation must have a name attribute")
          e.setXPathContext(context)
          throw e
        }
        if ((nsystem == null) && (npublicid == null)) {
          val e: XPathException = new XPathException(
            "dtd:notation must have a system attribute or a public attribute")
          e.setXPathContext(context)
          throw e
        }
        write(out, "\n  <!NOTATION " + notname)
        if (npublicid != null) {
          write(out, " PUBLIC \"" + npublicid + "\" ")
          if (nsystem != null) {
            write(out, "\"" + nsystem + "\" ")
          }
        } else {
          write(out, " SYSTEM \"" + nsystem + "\" ")
        }
        write(out, ">")
      } else if (child.getNodeKind == Type.TEXT) {
        write(out, child.getStringValue)
      } else {
        val e: XPathException = new XPathException(
          "Unrecognized element " + localname + " in DTD output")
        e.setXPathContext(context)
        throw e
      }
      child = children.next().asInstanceOf[NodeInfo]
    }
    if (openSquare) {
      write(out, "\n]")
    }
    write(out, ">\n")
    null
  }

  private def write(out: Outputter, s: String): Unit = {
    out.characters(s, getLocation, ReceiverOption.DISABLE_ESCAPING)
  }

  def export(out: ExpressionPresenter): Unit = {
    out.startElement("saxonDoctype", this)
    getContent.export(out)
    out.endElement()
  }

}
