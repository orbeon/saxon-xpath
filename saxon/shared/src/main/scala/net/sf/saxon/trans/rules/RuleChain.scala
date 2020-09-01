package net.sf.saxon.trans.rules

//remove if not needed
// import scala.collection.JavaConversions._

class RuleChain {

  var head: Rule = null

  var optimizationData: AnyRef = _

  def this(head: Rule) = {
    this()
    this.head = head
  }

  def setHead(head: Rule): Unit = {
    this.head = head
  }

  def getLength: Int = {
    var i: Int = 0
    var r: Rule = head
    while (r != null) {
      i += 1
      r = r.getNext
    }
    i
  }

}