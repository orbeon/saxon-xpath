package java.nio.charset


class IllegalCharsetNameException(var charsetName: String)
  extends IllegalArgumentException(String.valueOf(charsetName)) {
  def getCharsetName: String = charsetName
}
