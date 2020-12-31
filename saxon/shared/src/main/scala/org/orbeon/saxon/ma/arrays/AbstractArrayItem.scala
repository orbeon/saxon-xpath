////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// Copyright (c) 2018-2020 Saxonica Limited
// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is "Incompatible With Secondary Licenses", as defined by the Mozilla Public License, v. 2.0.
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

package org.orbeon.saxon.ma.arrays

import java.util.{ArrayList, List}

import org.orbeon.saxon.expr._
import org.orbeon.saxon.expr.sort.AtomicComparer
import org.orbeon.saxon.functions.DeepEqual
import org.orbeon.saxon.model._
import org.orbeon.saxon.om._
import org.orbeon.saxon.trace.ExpressionPresenter
import org.orbeon.saxon.trans.XPathException
import org.orbeon.saxon.tree.util.FastStringBuffer
import org.orbeon.saxon.value.{AtomicValue, Cardinality, IntegerValue, SequenceType}
import org.orbeon.saxon.query.AnnotationList

import scala.jdk.CollectionConverters._


/**
 * An abstract implementation of XDM array items, containing methods that can be implemented generically.
 */
abstract class AbstractArrayItem extends ArrayItem {

  // computed on demand
  private var memberType: SequenceType = null

  /**
   * Get the roles of the arguments, for the purposes of streaming
   *
   * @return an array of OperandRole objects, one for each argument
   */
  def getOperandRoles: Array[OperandRole] = Array(OperandRole.SINGLE_ATOMIC)

  /**
   * Ask whether this function item is an array
   *
   * @return true (it is an array)
   */
  def isArray: Boolean = true

  /**
   * Ask whether this function item is a map
   *
   * @return false (it is not a map)
   */
  def isMap: Boolean = false

  /**
   * Atomize the item.
   *
   * @return the result of atomization
   * @throws XPathException if atomization is not allowed for this kind of item
   */
  def atomize(): AtomicSequence = {
    val list: List[AtomicValue] = new ArrayList[AtomicValue](arrayLength())
    for (seq <- members) {
      seq
        .iterate()
        .forEachOrFail((item: Item) => {
          val atoms: AtomicSequence = item.atomize()
          for (atom: AtomicValue <- atoms.asScala) {
            list.add(atom)
          }
        })
    }
    new AtomicArray(list)
  }

  override def getAnnotations: AnnotationList = AnnotationList.EMPTY

  def getFunctionItemType: FunctionItemType = ArrayItemType.ANY_ARRAY_TYPE

  def getFunctionName: StructuredQName = null

  /**
   * Get a description of this function for use in error messages. For named functions, the description
   * is the function name (as a lexical QName). For others, it might be, for example, "inline function",
   * or "partially-applied ends-with function".
   *
   * @return a description of the function for use in error messages
   */
  def getDescription: String = "array"

  def getArity: Int = 1

  /**
   * Prepare an XPathContext object for evaluating the function
   *
   * @param callingContext the XPathContext of the function calling expression
   * @param originator
   * @return a suitable context for evaluating the function (which may or may
   *         not be the same as the caller's context)
   */
  def makeNewContext(callingContext: XPathContext,
                     originator: ContextOriginator): XPathContext =
    callingContext

  def call(context: XPathContext, args: Array[Sequence]): GroundedValue = {
    val subscript: IntegerValue = args(0).head.asInstanceOf[IntegerValue]
    get(
      ArrayFunctionSet.checkSubscript(subscript, arrayLength()) -
        1)
  }

  def deepEquals(other: Function,
                 context: XPathContext,
                 comparer: AtomicComparer,
                 flags: Int): Boolean =
    if (other.isInstanceOf[ArrayItem]) {
      val that: ArrayItem = other.asInstanceOf[ArrayItem]
      if (this.arrayLength() != that.arrayLength()) {
        return false
      }
      for (i <- 0 until this.arrayLength()
           if !DeepEqual.deepEqual(this.get(i).iterate(),
             that.get(i).iterate(),
             comparer,
             context,
             flags)) {
        return false
      }
      true
    } else {
      false
    }

  override def effectiveBooleanValue: Boolean =
    throw new XPathException(
      "Effective boolean value is not defined for arrays",
      "FORG0006")

  def getStringValue: String =
    throw new UnsupportedOperationException(
      "An array does not have a string value")

  def getStringValueCS: CharSequence =
    throw new UnsupportedOperationException(
      "An array does not have a string value")

  /**
   * Output information about this function item to the diagnostic explain() output
   */
  def export(out: ExpressionPresenter): Unit = {
    out.startElement("array")
    out.emitAttribute("size", s"${arrayLength()}")
    for (mem <- members) {
      Literal.exportValue(mem, out)
    }
    out.endElement()
  }

  def isTrustedResultType: Boolean = false

  override def toString: String = {
    val buffer: FastStringBuffer = new FastStringBuffer(256)
    buffer.append("[")
    for (seq <- members) {
      if (buffer.length > 1) {
        buffer.append(", ")
      }
      buffer.append(seq.toString)
    }
    buffer.append("]")
    buffer.toString
  }

  def getMemberType(th: TypeHierarchy): SequenceType = {
    //try {
    if (memberType == null) {
      if (isEmpty) {
        memberType = SequenceType.makeSequenceType(ErrorType,
          StaticProperty.EXACTLY_ONE)
      } else {
        var contentType: ItemType = null
        var contentCard: Int = StaticProperty.EXACTLY_ONE
        for (s <- members) {
          if (contentType == null) {
            contentType = SequenceTool.getItemType(s, th)
            contentCard = SequenceTool.getCardinality(s)
          } else {
            contentType = Type.getCommonSuperType(
              contentType,
              SequenceTool.getItemType(s, th))
            contentCard =
              Cardinality.union(contentCard, SequenceTool.getCardinality(s))
          }
        }
        memberType = SequenceType.makeSequenceType(contentType, contentCard)
      }
    }
    memberType
  }

  //        } catch (XPathException e) {
  //            return SequenceType.ANY_SEQUENCE;
  //        }
  //        } catch (XPathException e) {
  //            return SequenceType.ANY_SEQUENCE;
  //        }

}
