package net.sf.saxon.expr.instruct

import net.sf.saxon.event.Outputter

import net.sf.saxon.event.ReceiverOption

import net.sf.saxon.expr._

import net.sf.saxon.expr.parser.ContextItemStaticInfo

import net.sf.saxon.expr.parser.ExpressionTool

import net.sf.saxon.expr.parser.ExpressionVisitor

import net.sf.saxon.expr.parser.RebindingMap

import net.sf.saxon.model.ItemType

import net.sf.saxon.om.StandardNames

import net.sf.saxon.pattern.NodeKindTest

import net.sf.saxon.trace.ExpressionPresenter

import net.sf.saxon.trans.XPathException

import Comment._

object Comment {

  def checkContentXSLT(comment: String): String = {
    var hh: Int = 0
    var comStr = comment

    while ( {
      hh = comStr.indexOf("--")
      hh >= 0
    })
      comStr = comStr.substring(
        0,
        hh + 1) + ' ' + comStr.substring(hh + 1)
    if (comStr.endsWith("-")) {
      comStr = comStr + ' '
    }
    comStr
  }

  def checkContentXQuery(comment: String): String = {
    if (comment.contains("--")) {
      throw new XPathException("Invalid characters (--) in comment",
        "XQDY0072")
    }
    if (comment.length > 0 && comment.charAt(comment.length - 1) == '-') {
      throw new XPathException("Comment cannot end in '-'", "XQDY0072")
    }
    comment
  }

}

class Comment extends SimpleNodeConstructor {

  override def getInstructionNameCode(): Int = StandardNames.XSL_COMMENT

  override def getItemType: ItemType = NodeKindTest.COMMENT

  override def getCardinality(): Int = StaticProperty.EXACTLY_ONE

  def copy(rebindings: RebindingMap): Expression = {
    val exp: Comment = new Comment()
    ExpressionTool.copyLocationInfo(this, exp)
    exp.setSelect(getSelect.copy(rebindings))
    exp
  }

  def localTypeCheck(visitor: ExpressionVisitor,
                     contextItemType: ContextItemStaticInfo): Unit = {
    if (getSelect.isInstanceOf[Literal]) {
      val s: String = getSelect.asInstanceOf[Literal].getValue.getStringValue
      val s2: String =
        checkContent(s, visitor.getStaticContext.makeEarlyEvaluationContext())
      if (s2 != s) {
        this.select = new StringLiteral(s2)
      }
    }
  }

  def processValue(value: CharSequence,
                   output: Outputter,
                   context: XPathContext): Unit = {
    val comment: String = checkContent(value.toString, context)
    output.comment(comment, getLocation, ReceiverOption.NONE)
  }

   override def checkContent(comment: String,
                                      context: XPathContext): String =
    if (isXSLT) {
      checkContentXSLT(comment)
    } else {
      try checkContentXQuery(comment)
      catch {
        case err: XPathException => {
          err.setXPathContext(context)
          err.setLocation(getLocation)
          throw err
        }

      }
    }

  def export(out: ExpressionPresenter): Unit = {
    out.startElement("comment", this)
    var flags: String = ""
    if (isLocal) {
      flags += "l"
    }
    if (!flags.isEmpty) {
      out.emitAttribute("flags", flags)
    }
    getSelect.export(out)
    out.endElement()
  }

}
