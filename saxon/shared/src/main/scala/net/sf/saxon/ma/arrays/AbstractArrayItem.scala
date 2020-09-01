////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
package net.sf.saxon.ma.arrays

import net.sf.saxon.expr._
import net.sf.saxon.expr.sort.AtomicComparer
import net.sf.saxon.functions.DeepEqual
import net.sf.saxon.model._
import net.sf.saxon.om._
import net.sf.saxon.trace.ExpressionPresenter
import net.sf.saxon.trans.XPathException
import net.sf.saxon.tree.util.FastStringBuffer
import net.sf.saxon.value.AtomicValue
import net.sf.saxon.value.Cardinality
import net.sf.saxon.value.IntegerValue
import net.sf.saxon.value.SequenceType
import java.util.ArrayList
import java.util.List
import scala.jdk.CollectionConverters._
import net.sf.saxon.query.AnnotationList


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
  def getOperandRoles(): Array[OperandRole] = Array(OperandRole.SINGLE_ATOMIC)

  /**
   * Ask whether this function item is an array
   *
   * @return true (it is an array)
   */
  def isArray(): Boolean = true

  /**
   * Ask whether this function item is a map
   *
   * @return false (it is not a map)
   */
  def isMap(): Boolean = false

  /**
   * Atomize the item.
   *
   * @return the result of atomization
   * @throws XPathException if atomization is not allowed for this kind of item
   */
  def atomize(): AtomicSequence = {
    var list: List[AtomicValue] = new ArrayList[AtomicValue](arrayLength())
    for (seq <- members()) {
      seq
        .iterate()
        .forEachOrFail((item: Item) => {
          val atoms: AtomicSequence = item.atomize()
          for (atom: AtomicValue <- atoms.asScala) {
            list.add(atom)
          }
        })
    }
    return new AtomicArray(list)
  }

  override def getAnnotations(): AnnotationList = AnnotationList.EMPTY

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
        false
      }
      true
    } else {
      false
    }

  override def effectiveBooleanValue(): Boolean =
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
    for (mem <- members()) {
      Literal.exportValue(mem, out)
    }
    out.endElement()
  }

  def isTrustedResultType(): Boolean = false

  override def toString: String = {
    val buffer: FastStringBuffer = new FastStringBuffer(256)
    buffer.append("[")
    for (seq <- members()) {
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
        for (s <- members()) {
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

// Copyright (c) 2018-2020 Saxonica Limited
// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is "Incompatible With Secondary Licenses", as defined by the Mozilla Public License, v. 2.0.
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
