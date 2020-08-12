package net.sf.saxon.s9api

import net.sf.saxon.model.Type

import scala.beans.{BeanProperty, BooleanBeanProperty}


object XdmNodeKind extends Enumeration {

  val DOCUMENT: XdmNodeKind = new XdmNodeKind(Type.DOCUMENT)

  val ELEMENT: XdmNodeKind = new XdmNodeKind(Type.ELEMENT)

  val ATTRIBUTE: XdmNodeKind = new XdmNodeKind(Type.ATTRIBUTE)

  val TEXT: XdmNodeKind = new XdmNodeKind(Type.TEXT)

  val COMMENT: XdmNodeKind = new XdmNodeKind(Type.COMMENT)

  val PROCESSING_INSTRUCTION: XdmNodeKind = new XdmNodeKind(Type.PROCESSING_INSTRUCTION)

  val NAMESPACE: XdmNodeKind = new XdmNodeKind(Type.NAMESPACE)

  class XdmNodeKind(@BeanProperty  var number: Int)
    extends Val

  def forType(`type`: Int): XdmNodeKind = `type` match {
    case Type.DOCUMENT => DOCUMENT
    case Type.ELEMENT => ELEMENT
    case Type.ATTRIBUTE => ATTRIBUTE
    case Type.TEXT => TEXT
    case Type.COMMENT => COMMENT
    case Type.PROCESSING_INSTRUCTION => PROCESSING_INSTRUCTION
    case Type.NAMESPACE => NAMESPACE
    case _ => throw new IllegalArgumentException()

  }

  implicit def convertValue(v: Value): XdmNodeKind =
    v.asInstanceOf[XdmNodeKind]

}
