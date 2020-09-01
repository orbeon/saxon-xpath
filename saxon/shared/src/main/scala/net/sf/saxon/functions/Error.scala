package net.sf.saxon.functions

import net.sf.saxon.expr.Callable

import net.sf.saxon.expr.Expression

import net.sf.saxon.expr.StaticProperty

import net.sf.saxon.expr.XPathContext

import net.sf.saxon.expr.parser.Loc

import net.sf.saxon.lib.NamespaceConstant

import net.sf.saxon.model.BuiltInAtomicType

import net.sf.saxon.model.Type

import net.sf.saxon.om._

import net.sf.saxon.pattern.NameTest

import net.sf.saxon.trans.XPathException

import net.sf.saxon.tree.iter.AxisIterator

import net.sf.saxon.value.QNameValue

import net.sf.saxon.value.StringValue

import Error._

object Error {

  class UserDefinedXPathException(message: String)
    extends XPathException(message)

}

class Error extends SystemFunction with Callable {

  override def getSpecialProperties(arguments: Array[Expression]): Int =
    super.getSpecialProperties(arguments) & ~StaticProperty.NO_NODES_NEWLY_CREATED

  def isVacuousExpression: Boolean = true

  def error(context: XPathContext,
            errorCode: QNameValue,
            desc: StringValue,
            errObject: SequenceIterator): Item = {
    var qname: QNameValue = null
    if (getArity > 0) {
      qname = errorCode
    }
    if (qname == null) {
      qname = new QNameValue("err",
        NamespaceConstant.ERR,
        if (getArity == 1) "FOTY0004" else "FOER0000",
        BuiltInAtomicType.QNAME,
        false)
    }
    var description: String = null
    description =
      if (getArity > 1) if (desc == null) "" else desc.getStringValue
      else "Error signalled by application call on error()"
    val e: XPathException = new UserDefinedXPathException(description)
    e.setErrorCodeQName(qname.getStructuredQName)
    e.setXPathContext(context)
    if (getArity > 2 && errObject != null) {
      val errorObject: Sequence = errObject.materialize()
      if (errorObject.isInstanceOf[ZeroOrOne[_ <: Item]]) {
        val root: Item = errorObject.asInstanceOf[ZeroOrOne[_ <: Item]].head
        if ((root.isInstanceOf[NodeInfo]) &&
          root.asInstanceOf[NodeInfo].getNodeKind == Type.DOCUMENT) {
          val iter: AxisIterator = root
            .asInstanceOf[NodeInfo]
            .iterateAxis(AxisInfo.CHILD,
              new NameTest(Type.ELEMENT,
                "",
                "error",
                context.getConfiguration.getNamePool))
          val errorElement: NodeInfo = iter.next()
          if (errorElement != null) {
            val module: String = errorElement.getAttributeValue("", "module")
            val lineVal: String = errorElement.getAttributeValue("", "line")
            val line: Int =
              if (lineVal == null) -1 else java.lang.Integer.parseInt(lineVal)
            val columnVal: String =
              errorElement.getAttributeValue("", "column")
            val col: Int =
              if (columnVal == null) -1
              else java.lang.Integer.parseInt(columnVal)
            val locator: Loc = new Loc(module, line, col)
            e.setLocator(locator)
          }
        }
      }
      e.setErrorObject(errorObject)
    }
    throw e
  }

  def call(context: XPathContext, arguments: Array[Sequence]): Sequence = {
    val len: Int = arguments.length
    len match {
      case 0 => error(context, null, null, null)
      case 1 =>
        var arg0: QNameValue = arguments(0).head.asInstanceOf[QNameValue]
        if (arg0 == null) {
          arg0 = new QNameValue("err", NamespaceConstant.ERR, "FOER0000")
        }
        error(context, arg0, null, null)
      case 2 =>
        error(context,
          arguments(0).head.asInstanceOf[QNameValue],
          arguments(1).head.asInstanceOf[StringValue],
          null)
      case 3 =>
        error(context,
          arguments(0).head.asInstanceOf[QNameValue],
          arguments(1).head.asInstanceOf[StringValue],
          arguments(2).iterate())
      case _ => null

    }
  }

}
