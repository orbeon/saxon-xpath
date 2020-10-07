package org.orbeon.saxon.dom

import org.w3c.dom.Document
import org.xml.sax.InputSource
import javax.xml.parsers.DocumentBuilder
import javax.xml.parsers.DocumentBuilderFactory
import javax.xml.transform.Source
import javax.xml.transform.stream.StreamSource
import java.util.ArrayList
import java.util.List

import org.orbeon.saxon.utils.Configuration

//import scala.collection.compat._
import scala.jdk.CollectionConverters._


object DOMTransform {

  def main(args: Array[String]): Unit = {
    // new DOMTransform().doTransform(args, "DOMTransform")
  }

}

class DOMTransform { // extends Transform // Transform class not exist

  def preprocess(sources: List[Source]): List[Source] = {
    val domSources: ArrayList[Source] = new ArrayList[Source](sources.size)
    for (source <- sources.asScala) {
      val src: StreamSource = source.asInstanceOf[StreamSource]
      val ins: InputSource = new InputSource(src.getSystemId)
      val factory: DocumentBuilderFactory =
        DocumentBuilderFactory.newInstance()
      factory.setNamespaceAware(true)
      val builder: DocumentBuilder = factory.newDocumentBuilder()
      val doc: Document = builder.parse(ins)
      val dom: DocumentWrapper =
        new DocumentWrapper(doc, src.getSystemId, new Configuration)
      domSources.add(dom.getRootNode)
    }
    domSources
  }

}
