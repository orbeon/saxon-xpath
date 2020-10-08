package javax.xml.transform

trait URIResolver {
  @throws[TransformerException]
  def resolve(href: String, base: String): Source
}
