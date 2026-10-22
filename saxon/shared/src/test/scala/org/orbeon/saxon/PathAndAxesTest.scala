package org.orbeon.saxon

import org.orbeon.saxon.jaxp.SaxonTransformerFactory
import org.orbeon.saxon.om.{NodeInfo, TreeModel}
import org.xml.sax.helpers.AttributesImpl

import javax.xml.transform.sax.TransformerHandler

class PathAndAxesTest extends XPathTestBase {

  val doc: NodeInfo = {
    val treeBuilder = TreeModel.TINY_TREE.makeBuilder(configuration.makePipelineConfiguration)
    val handler: TransformerHandler = new SaxonTransformerFactory(configuration).newTransformerHandler
    handler.setResult(treeBuilder)

    def writeText(t: String): Unit =
      handler.characters(t.toCharArray, 0, t.length)

    def makeAtts(pairs: (String, String)*): AttributesImpl = {
      val atts = new AttributesImpl
      for ((name, value) <- pairs) {
        atts.addAttribute("", name, name, "CDATA", value)
      }
      atts
    }

    def element(name: String, atts: AttributesImpl = new AttributesImpl)(body: => Unit): Unit = {
      handler.startElement("", name, name, atts)
      body
      handler.endElement("", name, name)
    }

    def textElement(name: String, text: String, atts: AttributesImpl = new AttributesImpl): Unit = {
      element(name, atts) {
        writeText(text)
      }
    }

    handler.startDocument()
    element("catalog", makeAtts("id" -> "c1", "version" -> "2.0")) {
      element("section", makeAtts("id" -> "s1", "name" -> "books")) {
        element("book", makeAtts("id" -> "b1", "genre" -> "fiction", "in-stock" -> "true")) {
          textElement("title", "The Great Gatsby", makeAtts("lang" -> "en"))
          textElement("author", "F. Scott Fitzgerald")
          textElement("price", "12.99", makeAtts("currency" -> "USD"))
          element("reviews") {
            textElement("review", "Classic", makeAtts("rating" -> "5"))
            textElement("review", "Great", makeAtts("rating" -> "4"))
          }
        }
        element("book", makeAtts("id" -> "b2", "genre" -> "science", "in-stock" -> "false")) {
          textElement("title", "A Brief History of Time", makeAtts("lang" -> "en"))
          textElement("author", "Stephen Hawking")
          textElement("price", "15.50", makeAtts("currency" -> "USD"))
          handler.processingInstruction("status", "checked")
        }
      }
      element("section", makeAtts("id" -> "s2", "name" -> "periodicals")) {
        element("magazine", makeAtts("id" -> "m1", "genre" -> "tech", "in-stock" -> "true")) {
          textElement("title", "Wired", makeAtts("lang" -> "en"))
          textElement("issue", "42")
          textElement("price", "7.99", makeAtts("currency" -> "USD"))
        }
        element("newspaper", makeAtts("id" -> "n1", "genre" -> "news", "in-stock" -> "true")) {
          textElement("title", "The New York Times", makeAtts("lang" -> "en"))
          textElement("price", "2.50", makeAtts("currency" -> "USD"))
        }
      }
    }
    handler.endDocument()

    treeBuilder.getCurrentRoot
  }

  describe("Forward Axes") {

    describe("child axis") {
      check("count(child::catalog)", "1", doc)
      check("count(/catalog/child::section)", "2", doc)
      check("name(child::catalog)", "catalog", doc)
      checkStrings("/catalog/section[1]/child::book/title/string()", List("The Great Gatsby", "A Brief History of Time"), doc)
      checkStrings("/catalog/section[2]/child::*/title/string()", List("Wired", "The New York Times"), doc)
    }

    describe("descendant and descendant-or-self axes") {
      check("count(//book)", "2", doc)
      check("count(//title)", "4", doc)
      checkStrings("//book/title/string()", List("The Great Gatsby", "A Brief History of Time"), doc)
      checkStrings("//section[@name = 'periodicals']/descendant::price/string()", List("7.99", "2.50"), doc)
      check("count(/descendant-or-self::node())", "36", doc)
      check("count(//review/descendant-or-self::review)", "2", doc)
    }

    describe("following-sibling axis") {
      checkStrings(
        "//book[@id = 'b1']/following-sibling::book/@id/string()",
        List("b2"),
        doc
      )
      checkStrings(
        "//section[@id = 's1']/following-sibling::section/@name/string()",
        List("periodicals"),
        doc
      )
      check(
        "count(//book[@id = 'b2']/following-sibling::*)",
        "0",
        doc
      )
      checkStrings(
        "//book[@id = 'b1']/title/following-sibling::*/name()",
        List("author", "price", "reviews"),
        doc
      )
    }

    describe("following axis") {
      // Elements that follow b1 in document order, excluding descendants
      checkStrings(
        "//book[@id = 'b1']/following::book/@id/string()",
        List("b2"),
        doc
      )
      checkStrings(
        "//book[@id = 'b1']/following::magazine/@id/string()",
        List("m1"),
        doc
      )
      check(
        "count(//newspaper/following::*)",
        "0",
        doc
      )
    }

    describe("attribute axis (@)") {
      check("/catalog/@id/string()", "c1", doc)
      check("/catalog/@version/string()", "2.0", doc)
      checkStrings("//book/@id/string()", List("b1", "b2"), doc)
      checkStrings("//book/@genre/string()", List("fiction", "science"), doc)
      check("count(//book[@in-stock = 'true'])", "1", doc)
      check("count(//*[@in-stock = 'true'])", "3", doc)
      check("count(//book/@*)", "6", doc)
    }

    describe("self axis (.)") {
      check("/catalog/self::catalog/@id/string()", "c1", doc)
      check("count(/catalog/self::section)", "0", doc)
      check("//book[@id = 'b1']/(self::book/@id)/string()", "b1", doc)
    }
  }

  describe("Reverse Axes") {

    describe("parent axis (..)") {
      check("//book[@id = 'b1']/parent::section/@name/string()", "books", doc)
      check("//book[@id = 'b1']/../@id/string()", "s1", doc)
      check("//title/../name()", "book", doc)
      check("name(/catalog/..)", "", doc) // document node has no name
      check("count(/..)", "0", doc)
    }

    describe("ancestor and ancestor-or-self axes") {
      checkStrings(
        "//review[1]/ancestor::*/name()",
        List("catalog", "section", "book", "reviews"),
        doc
      )
      checkStrings(
        "//review[1]/ancestor-or-self::*/name()",
        List("catalog", "section", "book", "reviews", "review"),
        doc
      )
      check(
        "//review[1]/ancestor::section/@name/string()",
        "books",
        doc
      )
    }

    describe("preceding-sibling axis") {
      checkStrings(
        "//book[@id = 'b2']/preceding-sibling::book/@id/string()",
        List("b1"),
        doc
      )
      check(
        "count(//book[@id = 'b1']/preceding-sibling::*)",
        "0",
        doc
      )
      checkStrings(
        "//book[@id = 'b1']/reviews/preceding-sibling::*/name()",
        List("title", "author", "price"),
        doc
      )
    }

    describe("preceding axis") {
      // Elements that precede b2 in document order, excluding ancestors
      checkStrings(
        "//book[@id = 'b2']/preceding::book/@id/string()",
        List("b1"),
        doc
      )
      check(
        "count(//book[@id = 'b1']/title/preceding::*)",
        "0",
        doc
      )
    }
  }

  describe("Node Kind Tests") {

    describe("node() and text()") {
      check("count(//review/text())", "2", doc)
      check("//review[@rating = '5']/text()/string()", "Classic", doc)
      check("count(//reviews/node())", "2", doc)
    }

    describe("Wildcard *") {
      check("count(/catalog/*)", "2", doc)
      check("count(/catalog/*/*)", "4", doc)
      checkStrings("/catalog/*/@id/string()", List("s1", "s2"), doc)
    }

    describe("processing-instruction()") {
      check("count(//processing-instruction())", "1", doc)
      check("count(//processing-instruction('status'))", "1", doc)
      check("count(//processing-instruction('other'))", "0", doc)
      check("//processing-instruction('status')/string()", "checked", doc)
      check("//processing-instruction('status')/name()", "status", doc)
    }
  }

  describe("Predicates and Filtering") {

    describe("Positional predicates") {
      check("//book[1]/title/string()", "The Great Gatsby", doc)
      check("//book[2]/title/string()", "A Brief History of Time", doc)
      check("//book[last()]/title/string()", "A Brief History of Time", doc)
      check("//section[1]/*[last()]/title/string()", "A Brief History of Time", doc)
      check("//section[2]/*[1]/title/string()", "Wired", doc)
      check("//section[2]/*[2]/title/string()", "The New York Times", doc)

      // Step vs Filter precedence: //book/title[1] vs (//book/title)[1]
      checkStrings("//book/title[1]/string()", List("The Great Gatsby", "A Brief History of Time"), doc)
      check("(//book/title)[1]/string()", "The Great Gatsby", doc)
      check("(//book/title)[2]/string()", "A Brief History of Time", doc)
      check("(//title)[last()]/string()", "The New York Times", doc)
      check("(//title)[last() - 1]/string()", "Wired", doc)

      // Position range
      checkStrings(
        "(//title)[position() >= 2 and position() <= 3]/string()",
        List("A Brief History of Time", "Wired"),
        doc
      )
    }

    describe("Boolean predicates") {
      check("//book[@id = 'b1']/author/string()", "F. Scott Fitzgerald", doc)
      check("//book[@genre = 'science']/price/string()", "15.50", doc)
      checkStrings("//*[@in-stock = 'true']/title/string()", List("The Great Gatsby", "Wired", "The New York Times"), doc)
      check("//book[reviews]/title/string()", "The Great Gatsby", doc)
      check("count(//book[not(reviews)])", "1", doc)
      check("//book[price < 13]/title/string()", "The Great Gatsby", doc)
      check("//book[price > 13]/title/string()", "A Brief History of Time", doc)

      // Multiple predicates
      check("//*[@genre = 'fiction'][@in-stock = 'true']/title/string()", "The Great Gatsby", doc)
      check("count(//*[@genre = 'fiction'][@in-stock = 'false'])", "0", doc)
    }
  }

  describe("Node Comparison and Identity") {

    describe("Node identity (is)") {
      check("(/catalog/section[1]) is (//section[@id = 's1'])", "true", doc)
      check("(/catalog/section[1]) is (//section[@id = 's2'])", "false", doc)
      check("not((/catalog/section[1]) is (//section[@id = 's2']))", "true", doc)
      check("not((/catalog/section[1]) is (//section[@id = 's1']))", "false", doc)
      check("empty(() is //book[1])", "true", doc)
      check("empty(//book[1] is ())", "true", doc)
    }

    describe("Document order (<< and >>)") {
      check("(//book[@id = 'b1']) << (//book[@id = 'b2'])", "true", doc)
      check("(//book[@id = 'b2']) << (//book[@id = 'b1'])", "false", doc)
      check("(//book[@id = 'b2']) >> (//book[@id = 'b1'])", "true", doc)
      check("(//book[@id = 'b1']) >> (//book[@id = 'b2'])", "false", doc)
      check("(//section[1]) << (//section[2])", "true", doc)
      check("(//section[1]) << (//book[@id = 'b1'])", "true", doc) // parent precedes child
      check("(//book[@id = 'b1']) >> (//section[1])", "true", doc)
      check("(//book[1]) << (//book[1])", "false", doc)
      check("(//book[1]) >> (//book[1])", "false", doc)
    }
  }

  describe("Set Operators (Union, Intersect, Except)") {

    describe("Union (| and union)") {
      // Must deduplicate and return in document order
      checkStrings(
        "(//book[@id = 'b2'] | //book[@id = 'b1'])/@id/string()",
        List("b1", "b2"),
        doc
      )
      checkStrings(
        "(//book[@id = 'b2'] union //book[@id = 'b1'])/@id/string()",
        List("b1", "b2"),
        doc
      )
      checkStrings(
        "(//book | //magazine | //newspaper)/@id/string()",
        List("b1", "b2", "m1", "n1"),
        doc
      )
      // Duplicate nodes are eliminated
      check(
        "count(//book | //book)",
        "2",
        doc
      )
      // Empty sequence handling
      checkStrings(
        "(//book | ())/@id/string()",
        List("b1", "b2"),
        doc
      )
    }

    describe("Intersect") {
      checkStrings(
        "(//book intersect //*[@genre = 'fiction'])/@id/string()",
        List("b1"),
        doc
      )
      checkStrings(
        "(//book intersect //*[@in-stock = 'true'])/@id/string()",
        List("b1"),
        doc
      )
      check(
        "count(//book intersect //magazine)",
        "0",
        doc
      )
      check(
        "count(//book intersect ())",
        "0",
        doc
      )
    }

    describe("Except") {
      checkStrings(
        "(//book except //*[@genre = 'fiction'])/@id/string()",
        List("b2"),
        doc
      )
      checkStrings(
        "(//book except //book[@id = 'b1'])/@id/string()",
        List("b2"),
        doc
      )
      check(
        "count(//book except //book)",
        "0",
        doc
      )
      checkStrings(
        "(//book except ())/@id/string()",
        List("b1", "b2"),
        doc
      )
    }
  }

  describe("XPath 3.0 Node Functions") {

    describe("has-children()") {
      check("has-children(/catalog)", "true", doc)
      check("has-children(//book[@id = 'b1'])", "true", doc)
      check("has-children((//price)[1])", "true", doc) // price has text node child
      check("has-children(//processing-instruction('status'))", "false", doc)
      check("has-children()", "true", doc) // context item
    }

    describe("innermost() and outermost()") {
      // Elements that have no descendants in the set
      checkStrings(
        "innermost(//section | //book | //title)/name()",
        List("title", "title", "title", "title"),
        doc
      )
      // Elements that have no ancestors in the set
      checkStrings(
        "outermost(//section | //book | //title)/@id/string()",
        List("s1", "s2"), // sections are outermost
        doc
      )
      checkStrings(
        "outermost(//book | //review)/@id/string()",
        List("b1", "b2"),
        doc
      )
    }

    describe("path()") {
      check("path(/catalog)", "/Q{}catalog[1]", doc)
      check("path(/catalog/section[1])", "/Q{}catalog[1]/Q{}section[1]", doc)
      check("path(//book[@id = 'b2'])", "/Q{}catalog[1]/Q{}section[1]/Q{}book[2]", doc)
      check("path(//book[@id = 'b1']/@id)", "/Q{}catalog[1]/Q{}section[1]/Q{}book[1]/@id", doc)
    }

    describe("generate-id()") {
      check("generate-id(/catalog) = generate-id(/catalog)", "true", doc)
      check("generate-id(//book[1]) = generate-id(//book[1])", "true", doc)
      check("generate-id(//book[1]) != generate-id(//book[2])", "true", doc)
      check("string-length(generate-id(//book[1])) > 0", "true", doc)
    }

    describe("root(), name(), local-name(), namespace-uri()") {
      check("root(//review[1]) is /", "true", doc)
      check("name(/catalog)", "catalog", doc)
      check("local-name(/catalog)", "catalog", doc)
      check("namespace-uri(/catalog)", "", doc)
      check("name(//processing-instruction('status'))", "status", doc)
    }
  }
}
