////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
package org.orbeon.saxon.s9api

import Push._




object Push {

  trait Container {

    def setDefaultNamespace(uri: String): Unit

    def element(name: QName): Element

    def element(name: String): Element

    def text(value: CharSequence): Container

    def comment(value: CharSequence): Container

    def processingInstruction(name: String, value: CharSequence): Container

    def close(): Unit

  }

  trait Document extends Container {

    override def text(value: CharSequence): Document

    override def comment(value: CharSequence): Document

    override def processingInstruction(name: String,
                                       value: CharSequence): Document

  }

  trait Element extends Container {

    def attribute(name: QName, value: String): Element

    def attribute(name: String, value: String): Element

    def namespace(prefix: String, uri: String): Element

    override def text(value: CharSequence): Element

    override def comment(value: CharSequence): Element

    override def processingInstruction(name: String,
                                       value: CharSequence): Element

  }

}

trait Push {

  def document(wellFormed: Boolean): Document

}

// Copyright (c) 2018-2020 Saxonica Limited
// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is "Incompatible With Secondary Licenses", as defined by the Mozilla Public License, v. 2.0.
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
/**
  * An interface designed for applications to generate XML documents by issuing events. Functionally
  * similar to the SAX {@link org.xml.sax.ContentHandler} or the Stax {@link javax.xml.stream.XMLStreamWriter},
  * it is designed eliminate the usability problems and ambiguities in those specifications.
  *
  * <p>The {@code Push} interface can be used to create a single tree rooted at a document node.
  * It is possible to constrain the document node to be well-formed (in which case it must have
  * a single element node child, plus optionally comment and processing instruction children).
  * Some implementations may only accept well-formed documents.</p>
  *
  * <p>Here is an example of application code written to construct a simple XML document:</p>
  *
  * <pre>{@code
  * Push.Document doc = processor.newPush(destination).document(true);
  * doc.setDefaultNamespace("http://www.example.org/ns");
  * Push.Element top = doc.element("root");
  * top.attribute("version", "1.5");
  * for (Employee emp : employees) {
  *     top.element("emp")
  *        .attribute("ssn", emp.getSSN())
  *        .text(emp.getName);
  * }
  * doc.close();
  * }</pre>
  */
