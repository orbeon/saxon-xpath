package net.sf.saxon.event

import net.sf.saxon.utils.Configuration
import net.sf.saxon.expr.parser.Loc
import net.sf.saxon.model.BuiltInAtomicType
import net.sf.saxon.model.Untyped
import net.sf.saxon.om.FingerprintedQName
import net.sf.saxon.om.NoNamespaceName
import net.sf.saxon.om.NodeName
import net.sf.saxon.s9api.Push
import net.sf.saxon.s9api.Push.{Container, Document, Element}
import net.sf.saxon.s9api.QName
import net.sf.saxon.s9api.SaxonApiException

class PushToReceiver(out: Receiver) extends Push {

  private var outReceiver: ComplexContentOutputter = new ComplexContentOutputter(new RegularSequenceChecker(out, false))
  private var config: Configuration = out.getPipelineConfiguration.getConfiguration

  override def document(wellFormed: Boolean): Document = {
    outReceiver.open()
    new DocImpl(wellFormed)
  }

  private abstract class ContainerImpl(private var defaultNamespace: String)
    extends Push.Container {

    private var elementAwaitingClosure: ElemImpl = _

    private var closed: Boolean = _

    override def setDefaultNamespace(uri: String): Unit = {
      this.defaultNamespace = uri
    }

    override def element(name: QName): Element = {
      implicitClose()
      val fp: FingerprintedQName =
        new FingerprintedQName(name.getStructuredQName, config.getNamePool)
      outReceiver.startElement(fp, Untyped.getInstance, Loc.NONE, ReceiverOption.NONE)
      elementAwaitingClosure = new ElemImpl(defaultNamespace)
      elementAwaitingClosure
    }

    override def element(name: String): Element = {
      implicitClose()
      val fp: NodeName =
        if (defaultNamespace.isEmpty) new NoNamespaceName(name)
        else new FingerprintedQName("", defaultNamespace, name)
      outReceiver.startElement(fp, Untyped.getInstance, Loc.NONE, ReceiverOption.NONE)
      elementAwaitingClosure = new ElemImpl(defaultNamespace)
      elementAwaitingClosure
    }

    override def text(value: CharSequence): Container = {
      implicitClose()
      if (value != null && value.length > 0) {
        outReceiver.characters(value, Loc.NONE, ReceiverOption.NONE)
      }
      this
    }

    override def comment(value: CharSequence): Container = {
      implicitClose()
      if (value != null) {
        outReceiver.comment(value, Loc.NONE, ReceiverOption.NONE)
      }
      this
    }

    override def processingInstruction(name: String,
                                       value: CharSequence): Container = {
      implicitClose()
      if (value != null) {
        outReceiver.processingInstruction(name, value, Loc.NONE, ReceiverOption.NONE)
      }
      this
    }

    override def close(): Unit = {
      if (!closed) {
        implicitClose()
        sendEndEvent()
        closed = true
      }
    }

    private def implicitClose(): Unit = {
      if (closed) {
        throw new SaxonApiException("The container has been closed")
      }
      if (elementAwaitingClosure != null) {
        elementAwaitingClosure.close()
        elementAwaitingClosure = null
      }
    }

    def sendEndEvent(): Unit

  }

  private class DocImpl(wellFormed: Boolean) extends ContainerImpl("") with Document {

    private var foundElement: Boolean = false
    outReceiver.startDocument(ReceiverOption.NONE)


    override def element(name: QName): Element = {
      if (wellFormed && foundElement) {
        throw new SaxonApiException(
          "A well-formed document cannot have more than one element child")
      }
      foundElement = true
      super.element(name)
    }

    override def element(name: String): Element = {
      if (wellFormed && foundElement) {
        throw new SaxonApiException(
          "A well-formed document cannot have more than one element child")
      }
      foundElement = true
      super.element(name)
    }

    override def text(value: CharSequence): DocImpl = {
      if (wellFormed && value != null && value.length > 0) {
        throw new SaxonApiException(
          "A well-formed document cannot contain text outside any element")
      }
      super.text(value).asInstanceOf[DocImpl]
    }

    override def comment(value: CharSequence): Document =
      super.comment(value).asInstanceOf[Document]

    override def processingInstruction(name: String,
                                       value: CharSequence): Document =
      super.processingInstruction(name, value).asInstanceOf[Document]

    override def sendEndEvent(): Unit = {
      if (wellFormed && !foundElement) {
        throw new SaxonApiException(
          "A well-formed document must contain an element node")
      }
      outReceiver.endDocument()
      outReceiver.close()
    }

  }

  private class ElemImpl(defaultNamespace: String)
    extends ContainerImpl(defaultNamespace)
      with Element {

    private var foundChild: Boolean = _

    override def attribute(name: QName, value: String): Element = {
      checkChildNotFound()
      if (value != null) {
        val fp: FingerprintedQName =
          new FingerprintedQName(name.getStructuredQName, config.getNamePool)
        outReceiver.attribute(fp,
          BuiltInAtomicType.UNTYPED_ATOMIC,
          value,
          Loc.NONE,
          ReceiverOption.NONE)
      }
      this
    }

    override def attribute(name: String, value: String): Element = {
      checkChildNotFound()
      if (value != null) {
        val fp: NodeName = new NoNamespaceName(name)
        outReceiver.attribute(fp,
          BuiltInAtomicType.UNTYPED_ATOMIC,
          value,
          Loc.NONE,
          ReceiverOption.NONE)
      }
      this
    }

    override def namespace(prefix: String, uri: String): Element = {
      checkChildNotFound()
      outReceiver.namespace(prefix, uri, ReceiverOption.NONE)
      this
    }

    private def checkChildNotFound(): Unit = {
      if (foundChild) {
        throw new SaxonApiException(
          "Attribute nodes must be attached to an element before any children")
      }
    }

    override def element(name: QName): Element = {
      foundChild = true
      super.element(name)
    }

    override def element(name: String): Element = {
      foundChild = true
      super.element(name)
    }

    override def text(value: CharSequence): Element = {
      foundChild = true
      super.text(value).asInstanceOf[Element]
    }

    override def comment(value: CharSequence): Element = {
      foundChild = true
      super.comment(value).asInstanceOf[Element]
    }

    override def processingInstruction(name: String,
                                       value: CharSequence): Element = {
      foundChild = true
      super.processingInstruction(name, value).asInstanceOf[Element]
    }

    override def sendEndEvent(): Unit = {
      outReceiver.endElement()
    }

  }

}
