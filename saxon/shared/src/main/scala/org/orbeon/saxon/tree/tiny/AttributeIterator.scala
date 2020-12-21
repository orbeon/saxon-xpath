package org.orbeon.saxon.tree.tiny

import org.orbeon.saxon.model.SchemaType
import org.orbeon.saxon.model.Type
import org.orbeon.saxon.om._
import org.orbeon.saxon.pattern.NameTest
import org.orbeon.saxon.pattern.NodeTest
import org.orbeon.saxon.trans.XPathException
import org.orbeon.saxon.tree.iter.AxisIterator
import java.util.EnumSet

import org.orbeon.saxon.om.SequenceIterator.Property
import org.orbeon.saxon.om.SequenceIterator.Property.Property


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

  override def getProperties: Set[Property] =
    Set(Property.ATOMIZING)

}