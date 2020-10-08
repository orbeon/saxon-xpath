package javax.xml.transform

trait SourceLocator {
  def getPublicId: String
  def getSystemId: String
  def getLineNumber: Int
  def getColumnNumber: Int
}
