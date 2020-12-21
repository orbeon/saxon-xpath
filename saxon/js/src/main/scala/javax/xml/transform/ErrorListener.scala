package javax.xml.transform

trait ErrorListener {
  def warning(exception: TransformerException): Unit
  def error(exception: TransformerException): Unit
  def fatalError(exception: TransformerException): Unit
}
