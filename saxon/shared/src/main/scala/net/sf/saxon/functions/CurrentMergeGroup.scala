package net.sf.saxon.functions

import net.sf.saxon.expr.Expression

import net.sf.saxon.expr.SystemFunctionCall

import net.sf.saxon.expr.XPathContext

import net.sf.saxon.expr.sort.GroupIterator

import net.sf.saxon.expr.sort.MergeGroupingIterator

import net.sf.saxon.expr.sort.MergeInstr

import net.sf.saxon.model.AnyItemType

import net.sf.saxon.model.ItemType

import net.sf.saxon.om.Sequence

import net.sf.saxon.om.SequenceIterator

import net.sf.saxon.om.SequenceTool

import net.sf.saxon.trans.XPathException

import java.util.HashSet

import java.util.Set

import scala.beans.{BeanProperty, BooleanBeanProperty}

class CurrentMergeGroup extends SystemFunction {

  var isInLoop: Boolean = false

  @BeanProperty
  var controllingInstruction: MergeInstr = null

  private var allowedNames: Set[String] = new HashSet()

  def setControllingInstruction(instruction: MergeInstr,
                                isInLoop: Boolean): Unit = {
    this.controllingInstruction = instruction
    this.isInLoop = isInLoop
    for (m <- instruction.getMergeSources) {
      val name: String = m.sourceName
      if (name != null) {
        allowedNames.add(name)
      }
    }
  }

  override def getResultItemType(): ItemType = AnyItemType.getInstance

  override def getSpecialProperties(arguments: Array[Expression]): Int = 0

  override def makeFunctionCall(arguments: Expression*): Expression =
    new SystemFunctionCall(this, arguments.toArray) {
      override def getScopingExpression(): Expression =
        getControllingInstruction
    }

  def call(context: XPathContext, arguments: Array[Sequence]): Sequence = {
    var source: String = null
    if (arguments.length > 0) {
      source = arguments(0).head().getStringValue
    }
    SequenceTool.toLazySequence(currentGroup(source, context))
  }

  private def currentGroup(source: String, c: XPathContext): SequenceIterator = {
    val gi: GroupIterator = c.getCurrentMergeGroupIterator
    if (gi == null) {
      throw new XPathException("There is no current merge group", "XTDE3480")
    }
    if (source == null) {
      gi.iterateCurrentGroup()
    } else {
      if (!allowedNames.contains(source)) {
        throw new XPathException(
          "Supplied argument (" + source +
            ") is not the name of any xsl:merge-source in the containing xsl:merge instruction",
          "XTDE3490")
      }
      gi.asInstanceOf[MergeGroupingIterator].iterateCurrentGroup(source)
    }
  }

  override def getStreamerName(): String = "CurrentMergeGroup"

}
