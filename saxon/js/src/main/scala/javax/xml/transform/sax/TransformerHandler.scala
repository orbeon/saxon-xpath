package javax.xml.transform.sax

import javax.xml.transform.{Result, Transformer}
import org.xml.sax.{ContentHandler, DTDHandler}
import org.xml.sax.ext.LexicalHandler


trait TransformerHandler extends ContentHandler with LexicalHandler with DTDHandler {
  def setResult(result: Result): Unit
  def setSystemId(systemID: String): Unit
  def getSystemId: String
  def getTransformer: Transformer
}
