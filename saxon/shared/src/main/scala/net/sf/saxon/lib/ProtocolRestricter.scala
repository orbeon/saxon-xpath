////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
package net.sf.saxon.lib

import java.net.URI
import java.util.{ArrayList, List, Objects}
import java.util.function.Predicate

object ProtocolRestricter {

  def make(value: String): ProtocolRestricter = {
    Objects.requireNonNull(value)
    var valueVar: String = value
    valueVar = value.trim()
    if (valueVar.equals("all")) {
      var protocolRestricter: Predicate[URI] = new Predicate[URI] {
        override def test(t: URI): Boolean = true
      }
      protocolRestricter.asInstanceOf[ProtocolRestricter]
    } else {
      new ProtocolRestricter(value)
    }
  }

}

class ProtocolRestricter private(value: String) extends Predicate[URI] {

  private var rule: String = value

  private var permitted: List[Predicate[URI]] = new ArrayList()

  val tokens: Array[String] = value.split(",\\s*")

  for (token <- tokens) {
    if (token.startsWith("jar:") && token.length > 4) {
      val subScheme: String = token.substring(4).toLowerCase()
      permitted.add(
        new Predicate[URI] {
          override def test(uri: URI): Boolean = uri.getScheme.equals("jar") &&
            uri.getSchemeSpecificPart.toLowerCase().startsWith(subScheme)
        })
    } else {
      permitted.add(new Predicate[URI] {
        override def test(uri: URI): Boolean = uri.getScheme == token
      })
    }
  }

  import scala.jdk.CollectionConverters._

  override def test(uri: URI): Boolean =
    permitted.asScala.find(_.test(uri)).map(_ => true).getOrElse(false)

  override def toString(): String = rule

}
