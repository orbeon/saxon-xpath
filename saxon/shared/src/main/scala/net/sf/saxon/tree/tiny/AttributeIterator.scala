package net.sf.saxon.tree.tiny

import net.sf.saxon.model.SchemaType
import net.sf.saxon.model.Type
import net.sf.saxon.om._
import net.sf.saxon.pattern.NameTest
import net.sf.saxon.pattern.NodeTest
import net.sf.saxon.trans.XPathException
import net.sf.saxon.tree.iter.AxisIterator
import java.util.EnumSet

import net.sf.saxon.om.SequenceIterator.Property
import net.sf.saxon.om.SequenceIterator.Property.Property


class AttributeIterator(private val tree: TinyTree,
                        private val element: Int,
                        private val nodeTest: NodeTest)
  extends AxisIterator
    with AtomizedValueIterator {

  private var index: Int = tree.alpha(element)

  private var currentNodeNr: Int = -1

  private def moveToNext(): Boolean = {
    while (true) {
      if (index >= tree.numberOfAttributes || tree.attParent(index) != element) {
        index = java.lang.Integer.MAX_VALUE
        currentNodeNr = -1
        return false
      }
      val typeCode: SchemaType = tree.getAttributeType(index)
      if (nodeTest.matches(Type.ATTRIBUTE,
        new CodedName(tree.attCode(index) & NamePool.FP_MASK,
          "",
          tree.getNamePool),
        typeCode)) {
        currentNodeNr = { index += 1; index - 1 }
        if (nodeTest.isInstanceOf[NameTest]) {
          index = java.lang.Integer.MAX_VALUE
        }
        return true
      }
      index = index + 1;
    }
    false
  }

  def next(): NodeInfo =
    if (moveToNext()) {
      tree.getAttributeNode(currentNodeNr)
    } else {
      null
    }

  def nextAtomizedValue(): AtomicSequence =
    if (moveToNext()) {
      tree.getTypedValueOfAttribute(null, currentNodeNr)
    } else {
      null
    }

  override def getProperties(): Set[Property] =
    Set(Property.ATOMIZING)

}