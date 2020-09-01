package net.sf.saxon.expr.sort

import net.sf.saxon.lib.SubstringMatcher

import net.sf.saxon.model.StringConverter

import net.sf.saxon.model.ValidationFailure

import net.sf.saxon.trans.XPathException

import net.sf.saxon.value.AnyURIValue

import java.net.URI

import java.net.URISyntaxException

import java.text.CollationElementIterator

import java.text.CollationKey

import java.text.Collator

import java.text.RuleBasedCollator

import java.util._

import UcaCollatorUsingJava._

import scala.beans.{BeanProperty, BooleanBeanProperty}

import scala.jdk.CollectionConverters._

object UcaCollatorUsingJava {

  private var keywords: Array[String] = Array("fallback",
    "lang",
    "version",
    "strength",
    "alternate",
    "backwards",
    "normalization",
    "maxVariable",
    "caseLevel",
    "caseFirst",
    "numeric",
    "reorder")

  private var keys: Set[String] =
    new HashSet[String](Arrays.asList(keywords: _*))

  private trait Strength {

    def compare(ce1: Int, ce2: Int): Int

  }

}

class UcaCollatorUsingJava(private var uri: String) extends SubstringMatcher {

  private var uca: RuleBasedCollator = _

  private var strengthLevel: Strength = _

  @BeanProperty
  var properties: Properties = _

  this.setProps(parseProps(uri))

  def getRuleBasedCollator: RuleBasedCollator = uca

  private def error(field: String, allowed: String): Unit = {
    error("value of " + field + " must be " + allowed)
  }

  private def error(field: String, allowed: String, requested: String): Unit = {
    error(
      "value of " + field + " must be " + allowed + ", requested was:" +
        requested)
  }

  private def error(message: String): Unit = {
    throw new XPathException(
      "Error in UCA Collation URI " + uri + ": " + message,
      "FOCH0002")
  }

  def getJavaCollationKey(source: String): CollationKey =
    uca.getCollationKey(source)

  override def hashCode(): Int = uca.hashCode

  private def setProps(props: Properties): Unit = {
    this.properties = props
    val fallbackError: Boolean = false
    val fallback: String = props.getProperty("fallback")
    if (fallback != null) {
      fallback match {
        case "yes" =>
        case "no" => error("fallback=no is not supported in Saxon-HE")
        case _ => error("fallback", "yes|no")

      }
    }
    val lang: String = props.getProperty("lang")
    if (lang != null && !lang.isEmpty) {
      val vf: ValidationFailure =
        StringConverter.StringToLanguage.INSTANCE.validate(lang)
      if (vf != null) {
        error("lang", "a valid language code")
      }
      var language: String = null
      var country: String = ""
      var variant: String = ""
      val parts: Array[String] = lang.split("-")
      language = parts(0)
      if (parts.length > 1) {
        country = parts(1)
      }
      if (parts.length > 2) {
        variant = parts(2)
      }
      val loc: Locale = new Locale(language, country, variant)
      uca = Collator.getInstance(loc).asInstanceOf[RuleBasedCollator]
    }
    val strength: String = props.getProperty("strength")
    if (strength != null) {
      strength match {
        case "primary" | "1" => this.setStrength(Collator.PRIMARY)
        case "secondary" | "2" => this.setStrength(Collator.SECONDARY)
        case "tertiary" | "3" => this.setStrength(Collator.TERTIARY)
        case "quaternary" | "4" => this.setStrength(Collator.IDENTICAL)
        case "identical" | "5" => this.setStrength(Collator.IDENTICAL)

      }
    }
    val normalization: String = props.getProperty("normalization")
    if (normalization != null) {
      if (normalization.==("yes")) {
        uca.setDecomposition(java.text.Collator.CANONICAL_DECOMPOSITION)
      } else if (normalization.==("no")) {
        uca.setDecomposition(java.text.Collator.NO_DECOMPOSITION)
      }
    }
  }

  private def parseProps(uri: String): Properties = {
    var uuri: URI = null
    uuri = new URI(uri)
    val unknownKeys: ArrayList[String] = new ArrayList[String]()
    val props: Properties = new Properties()
    val query: String = AnyURIValue.decode(uuri.getRawQuery)
    if (query != null && !query.isEmpty) {
      for (s <- query.split(";")) {
        val tokens: Array[String] = s.split("=")
        if (!keys.contains(tokens(0))) {
          unknownKeys.add(tokens(0))
        }
        props.setProperty(tokens(0), tokens(1))
      }
    }
    val fallback: String = props.getProperty("fallback")
    if (fallback != null && fallback.==("no") && !unknownKeys.isEmpty) {
      val message: StringBuilder = new StringBuilder(
        if (unknownKeys.size > 1) "unknown parameters:"
        else "unknown parameter:")
      for (u <- unknownKeys.asScala) {
        message.append(u).append(" ")
      }
      error(message.toString)
    }
    props
  }

  def setStrength(newStrength: Int): Unit = {
    uca.setStrength(newStrength)
  }

  def getStrength: Int = uca.getStrength

  override def comparesEqual(s1: CharSequence, s2: CharSequence): Boolean =
    uca.compare(s1, s2) == 0

  override def getCollationURI(): String = uri

  override def compareStrings(o1: CharSequence, o2: CharSequence): Int =
    uca.compare(o1, o2)

  override def getCollationKey(s: CharSequence): AtomicMatchKey = {
    val ck: CollationKey = uca.getCollationKey(s.toString)
    new CollationMatchKey(ck)
  }

  def contains(s1: String, s2: String): Boolean = {
    val collator: RuleBasedCollator = getRuleBasedCollator
    val iter1: CollationElementIterator =
      collator.getCollationElementIterator(s1)
    val iter2: CollationElementIterator =
      collator.getCollationElementIterator(s2)
    collationContains(iter1, iter2, null, matchAtEnd = false)
  }

  def endsWith(s1: String, s2: String): Boolean = {
    val collator: RuleBasedCollator = getRuleBasedCollator
    val iter1: CollationElementIterator =
      collator.getCollationElementIterator(s1)
    val iter2: CollationElementIterator =
      collator.getCollationElementIterator(s2)
    collationContains(iter1, iter2, null, matchAtEnd = true)
  }

  def startsWith(s1: String, s2: String): Boolean = {
    val collator: RuleBasedCollator = getRuleBasedCollator
    val iter1: CollationElementIterator =
      collator.getCollationElementIterator(s1)
    val iter2: CollationElementIterator =
      collator.getCollationElementIterator(s2)
    collationStartsWith(iter1, iter2)
  }

  def substringAfter(s1: String, s2: String): String = {
    val collator: RuleBasedCollator = getRuleBasedCollator
    val iter1: CollationElementIterator =
      collator.getCollationElementIterator(s1)
    val iter2: CollationElementIterator =
      collator.getCollationElementIterator(s2)
    val ia: Array[Int] = Array.ofDim[Int](2)
    val ba: Boolean = collationContains(iter1, iter2, ia, matchAtEnd = false)
    if (ba) {
      s1.substring(ia(1))
    } else {
      ""
    }
  }

  def substringBefore(s1: String, s2: String): String = {
    val collator: RuleBasedCollator = getRuleBasedCollator
    val iter1: CollationElementIterator =
      collator.getCollationElementIterator(s1)
    val iter2: CollationElementIterator =
      collator.getCollationElementIterator(s2)
    val ib: Array[Int] = Array.ofDim[Int](2)
    val bb: Boolean = collationContains(iter1, iter2, ib, matchAtEnd = false)
    if (bb) {
      s1.substring(0, ib(0))
    } else {
      ""
    }
  }

  private def collationStartsWith(s0: CollationElementIterator,
                                  s1: CollationElementIterator): Boolean = {
    makeStrengthObject()
    while (true) {
      var e0: Int = 0
      var e1: Int = 0
      e1 = s1.next()
      if (e1 == CollationElementIterator.NULLORDER) return true
      e0 = s0.next()
      if (e0 == CollationElementIterator.NULLORDER) return false
      if (strengthLevel.compare(e0, e1) != 0) return false
    }
    false
  }

  private def show(ce: Int): String =
    "" + CollationElementIterator.primaryOrder(ce) + "/" +
      CollationElementIterator.secondaryOrder(ce) +
      "/" +
      CollationElementIterator.tertiaryOrder(ce)

  private def makeStrengthObject(): Unit = {
    if (strengthLevel == null) {
      getStrength match {
        case com.ibm.icu.text.Collator.PRIMARY => strengthLevel = new Primary()
        case com.ibm.icu.text.Collator.SECONDARY =>
          strengthLevel = new Secondary()
        case com.ibm.icu.text.Collator.TERTIARY =>
          strengthLevel = new Tertiary()
        case _ => strengthLevel = new Identical()

      }
    }
  }

  private def collationContains(s0: CollationElementIterator,
                                s1: CollationElementIterator,
                                offsets: Array[Int],
                                matchAtEnd: Boolean): Boolean = {
    makeStrengthObject()
    var e0: Int = 0
    var e1: Int = 0
    e1 = s1.next()
    if (e1 == CollationElementIterator.NULLORDER) return true
    e0 = CollationElementIterator.NULLORDER
    while (true) {
      while (strengthLevel.compare(e0, e1) != 0) {
        e0 = s0.next()
        if (e0 == CollationElementIterator.NULLORDER) return false
      }
      val start: Int = s0.getOffset
      if (collationStartsWith(s0, s1)) {
        if (matchAtEnd) {
          e0 = s0.next()
          if (e0 == CollationElementIterator.NULLORDER) return true
        } else {
          if (offsets != null) {
            offsets(0) = start - 1
            offsets(1) = s0.getOffset
          }
          return true
        }
      }
      s0.setOffset(start)
      if (s0.getOffset != start) {
        s0.next()
      }
      s1.reset()
      e0 = -1
      e1 = s1.next()
    }
    false
  }

  private class Primary extends Strength {

    override def compare(ce1: Int, ce2: Int): Int =
      java.lang.Integer.compare(CollationElementIterator.primaryOrder(ce1),
        CollationElementIterator.primaryOrder(ce2))

  }

  private class Secondary extends Strength {

    override def compare(ce1: Int, ce2: Int): Int = {
      val c1: Int = java.lang.Integer.compare(
        CollationElementIterator.primaryOrder(ce1),
        CollationElementIterator.primaryOrder(ce2))
      if (c1 == 0) {
        java.lang.Integer.compare(
          CollationElementIterator.secondaryOrder(ce1).toInt,
          CollationElementIterator.secondaryOrder(ce2).toInt)
      } else {
        c1
      }
    }

  }

  private class Tertiary extends Strength {

    override def compare(ce1: Int, ce2: Int): Int = {
      val c1: Int = java.lang.Integer.compare(
        CollationElementIterator.primaryOrder(ce1),
        CollationElementIterator.primaryOrder(ce2))
      if (c1 == 0) {
        val c2: Int = java.lang.Integer.compare(
          CollationElementIterator.secondaryOrder(ce1).toInt,
          CollationElementIterator.secondaryOrder(ce2).toInt)
        if (c2 == 0) {
          java.lang.Integer.compare(
            CollationElementIterator.tertiaryOrder(ce1).toInt,
            CollationElementIterator.tertiaryOrder(ce2).toInt)
        } else {
          c2
        }
      } else {
        c1
      }
    }

  }

  private class Identical extends Strength {

    override def compare(ce1: Int, ce2: Int): Int =
      java.lang.Integer.compare(ce1, ce2)

  }

}
