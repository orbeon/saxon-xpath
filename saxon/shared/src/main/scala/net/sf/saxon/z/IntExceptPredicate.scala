package net.sf.saxon.z

class IntExceptPredicate(private var p1: java.util.function.IntPredicate,
                         private var p2: java.util.function.IntPredicate)
  extends java.util.function.IntPredicate {

  def test(value: Int): Boolean = p1.test(value) && !p2.test(value)

  def getOperands: Array[java.util.function.IntPredicate] = Array(p1, p2)

}
