package net.sf.saxon.regex.charclass

import net.sf.saxon.utils.Configuration

import net.sf.saxon.event.Builder

import net.sf.saxon.lib.ParseOptions

import net.sf.saxon.lib.Validation

import net.sf.saxon.model.Type

import net.sf.saxon.om.AxisInfo

import net.sf.saxon.om.NodeInfo

import net.sf.saxon.pattern.NameTest

import net.sf.saxon.pattern.NodeKindTest

import net.sf.saxon.serialize.charcode.XMLCharacterData

import scala.jdk.CollectionConverters._

import net.sf.saxon.tree.iter.AxisIterator

import net.sf.saxon.tree.tiny.TinyElementImpl

import net.sf.saxon.z._

import javax.xml.transform.stream.StreamSource

import java.io.InputStream

import java.util.ArrayList

import java.util.HashMap

import java.util.Map

import java.util.function.IntPredicate

object Categories {

  object Category {

    private def extent(predicate: IntPredicate): IntSet = {
      if (predicate.isInstanceOf[IntSetPredicate]) {
        predicate.asInstanceOf[IntSetPredicate].getIntSet
      }
      null
    }

  }

  class Category(private var label: String,
                 private var predicate: java.util.function.IntPredicate)
    extends CharacterClass {

    def test(value: Int): Boolean = predicate.test(value)

    def isDisjoint(other: CharacterClass): Boolean =
      if (other.isInstanceOf[Category]) {
        val majorCat0: Char = label.charAt(0)
        val otherLabel: String = other.asInstanceOf[Category].label
        val majorCat1: Char = otherLabel.charAt(0)
        majorCat0 != majorCat1 ||
          (label.length > 1 && otherLabel.length > 1 && label != otherLabel)
      } else if (other.isInstanceOf[InverseCharacterClass]) {
        other.isDisjoint(this)
      } else if (other.isInstanceOf[SingletonCharacterClass]) {
        !test(other.asInstanceOf[SingletonCharacterClass].getCodepoint)
      } else if (other.isInstanceOf[IntSetCharacterClass]) {
        val intSet: IntSet = other.getIntSet
        if (intSet.size > 100) {
          return false
        }
        val ii: IntIterator = intSet.iterator()
        while (ii.hasNext) if (test(ii.next)) {
          false
        }
        true
      } else {
        false
      }

    def getIntSet(): IntSet = Category.extent(predicate)

  }

  private var CATEGORIES: HashMap[String, Category] = null

  def build(): Unit = {
    CATEGORIES = new HashMap(30)
    val in: InputStream = Configuration.locateResource("categories.xml",
      new ArrayList(),
      new ArrayList()).asInstanceOf[InputStream]
    if (in == null) {
      throw new RuntimeException("Unable to read categories.xml file")
    }
    val config: Configuration = new Configuration()
    val options: ParseOptions = new ParseOptions()
    options.setSchemaValidationMode(Validation.SKIP)
    options.setDTDValidationMode(Validation.SKIP)
    options.setTreeModel(Builder.TINY_TREE)
    var doc: NodeInfo = null
    doc = config
      .buildDocumentTree(new StreamSource(in, "categories.xml"), options)
      .getRootNode
    val fp_name: Int = config.getNamePool.allocateFingerprint("", "name")
    val fp_f: Int = config.getNamePool.allocateFingerprint("", "f")
    val fp_t: Int = config.getNamePool.allocateFingerprint("", "t")
    val iter: AxisIterator = doc.iterateAxis(
      AxisInfo.DESCENDANT,
      new NameTest(Type.ELEMENT, "", "cat", config.getNamePool))
    iter.forEach((item) => {
      val cat: String =
        item.asInstanceOf[TinyElementImpl].getAttributeValue(fp_name)
      val irs: IntRangeSet = new IntRangeSet()
      for (r <- item.asInstanceOf[NodeInfo].children(NodeKindTest.ELEMENT)) {
        val from: String =
          r.asInstanceOf[TinyElementImpl].getAttributeValue(fp_f)
        val to: String =
          r.asInstanceOf[TinyElementImpl].getAttributeValue(fp_t)
        irs.addRange(java.lang.Integer.parseInt(from, 16),
          java.lang.Integer.parseInt(to, 16))
      }
      CATEGORIES.put(cat, new Category(cat, new IntSetPredicate(irs)))
    })
    val c: String = "CLMNPSZ"
    for (i <- 0 until c.length) {
      val ch: Char = c.charAt(i)
      var ip: IntPredicate = null
      for ((key, value) <- CATEGORIES.asScala if key.charAt(0) == ch) {
        ip = if (ip == null) value else ip.or(value)
      }
      val label: String = s"$ch"
      CATEGORIES.put(label, new Category(label, ip))
    }
  }

  val ESCAPE_s: CharacterClass = new IntSetCharacterClass(
    IntArraySet.make(Array(9, 10, 13, 32), 4))

  val ESCAPE_S: CharacterClass = new InverseCharacterClass(ESCAPE_s)

  val ESCAPE_i: PredicateCharacterClass = new PredicateCharacterClass(
    (value) => XMLCharacterData.isNCNameStart11(value) || value == ':')

  val ESCAPE_I: CharacterClass = new InverseCharacterClass(ESCAPE_i)

  val ESCAPE_c: PredicateCharacterClass = new PredicateCharacterClass(
    (value) => XMLCharacterData.isNCName11(value) || value == ':')

  val ESCAPE_C: CharacterClass = new InverseCharacterClass(ESCAPE_c)

  val ESCAPE_d: Category = getCategory("Nd")

  val ESCAPE_D: CharacterClass = new InverseCharacterClass(ESCAPE_d)

  var CATEGORY_P: Category = getCategory("P")

  var CATEGORY_Z: Category = getCategory("Z")

  var CATEGORY_C: Category = getCategory("C")

  val ESCAPE_w: PredicateCharacterClass = new PredicateCharacterClass(
    (value) =>
      !(CATEGORY_P.test(value) || CATEGORY_Z.test(value) || CATEGORY_C.test(
        value)))

  val ESCAPE_W: CharacterClass = new InverseCharacterClass(ESCAPE_w)

  def getCategory(cat: String): Category = synchronized {
    if (CATEGORIES == null) {
      build()
    }
    CATEGORIES.get(cat)
  }

}
