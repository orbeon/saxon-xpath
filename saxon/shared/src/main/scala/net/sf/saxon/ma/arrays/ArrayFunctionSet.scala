package net.sf.saxon.ma.arrays

import java.util.{ArrayList, List}

import net.sf.saxon.expr.XPathContext
import net.sf.saxon.functions.SystemFunction._
import net.sf.saxon.functions.registry.BuiltInFunctionSet
import net.sf.saxon.functions.registry.BuiltInFunctionSet._
import net.sf.saxon.functions.{Fold, FoldingFunction, SystemFunction}
import net.sf.saxon.lib.NamespaceConstant
import net.sf.saxon.ma.arrays.ArrayFunctionSet.ArrayToSequence._
import net.sf.saxon.ma.arrays.ArrayFunctionSet._
import net.sf.saxon.model.{AnyItemType, BuiltInAtomicType, ItemType, SpecificFunctionType}
import net.sf.saxon.om._
import net.sf.saxon.trans.XPathException
import net.sf.saxon.value._
import net.sf.saxon.z.{IntHashSet, IntSet}

object ArrayFunctionSet {

  var THE_INSTANCE: ArrayFunctionSet = new ArrayFunctionSet()

  def getInstance(): ArrayFunctionSet = THE_INSTANCE

  def checkSubscript(subscript: IntegerValue, limit: Int): Int = {
    val index: Int = subscript.asSubscript()
    if (index <= 0) {
      throw new XPathException(
        "Array subscript " + subscript.getStringValue + " is out of range",
        "FOAY0001")
    }
    if (index > limit) {
      throw new XPathException(
        "Array subscript " + subscript.getStringValue + " exceeds limit (" +
          limit +
          ")",
        "FOAY0001")
    }
    index
  }

  object ArrayAppend {

    def append(array: ArrayItem, member: Sequence): ArrayItem = {
      val list: List[GroundedValue] = new ArrayList[GroundedValue](1)
      list.add(member.materialize())
      val otherArray: SimpleArrayItem = new SimpleArrayItem(list)
      array.concat(otherArray)
    }

  }

  /**
    * Implementation of the function array:append(array, item()*) =&gt; array
    */
  class ArrayAppend extends SystemFunction {

    def call(context: XPathContext, arguments: Array[Sequence]): ArrayItem = {
      val array: ArrayItem = arguments(0).head().asInstanceOf[ArrayItem]
      assert(array != null)
      ArrayAppend.append(array, arguments(1))
    }

  }

  /**
    * Implementation of the function array:filter(array, function) =&gt; array
    */
  class ArrayFilter extends SystemFunction {

    override def call(context: XPathContext,
                      arguments: Array[Sequence]): ArrayItem = {
      val array: ArrayItem = arguments(0).head().asInstanceOf[ArrayItem]
      assert(array != null)
      val fn: Function = arguments(1).head().asInstanceOf[Function]
      val list: List[GroundedValue] = new ArrayList[GroundedValue](1)
      var i: Int = 0
      i = 0
      while (i < array.arrayLength()) {
        if (SystemFunction.dynamicCall(fn, context, Array(array.get(i)))
          .head()
          .asInstanceOf[BooleanValue]
          .getBooleanValue) {
          list.add(array.get(i))
        }
        { i += 1; i - 1 }
      }
      new SimpleArrayItem(list)
    }

  }

  /**
    * Implementation of the function array:flatten =&gt; item()*
    */
  class ArrayFlatten extends SystemFunction {

    private def flatten(arg: Sequence, out: List[Item]): Unit = {
      arg
        .iterate()
        .forEachOrFail((item) =>
          if (item.isInstanceOf[ArrayItem]) {
            for (member <- item.asInstanceOf[ArrayItem].members()) {
              flatten(member, out)
            }
          } else {
            out.add(item)
          })
    }

    def call(context: XPathContext, arguments: Array[Sequence]): Sequence = {
      val out: List[Item] = new ArrayList[Item]()
      flatten(arguments(0), out)
      SequenceExtent.makeSequenceExtent(out)
    }

  }

  /**
    * Implementation of the function array:fold-left(array, item()*, function) =&gt; array
    */
  class ArrayFoldLeft extends SystemFunction {

    override def call(context: XPathContext,
                      arguments: Array[Sequence]): Sequence = {
      val array: ArrayItem = arguments(0).head().asInstanceOf[ArrayItem]
      assert(array != null)
      val arraySize: Int = array.arrayLength()
      var zero: Sequence = arguments(1).head()
      val fn: Function = arguments(2).head().asInstanceOf[Function]
      var i: Int = 0
      i = 0
      while (i < arraySize) {
        zero = dynamicCall(fn, context, Array(zero, array.get(i)))
          i += 1
      }
      zero
    }

  }

  /**
    * Implementation of the function array:fold-left(array, item()*, function) =&gt; array
    */
  class ArrayFoldRight extends SystemFunction {

    override def call(context: XPathContext,
                      arguments: Array[Sequence]): Sequence = {
      val array: ArrayItem = arguments(0).head().asInstanceOf[ArrayItem]
      assert(array != null)
      var zero: Sequence = arguments(1).head()
      val fn: Function = arguments(2).head().asInstanceOf[Function]
      var i: Int = 0
      i = array.arrayLength() - 1
      while (i >= 0) {
        zero = dynamicCall(fn, context, Array(array.get(i), zero))
          i -= 1
      }
      zero
    }

  }

  /**
    * Implementation of the function array:for-each(array, function) =&gt; array
    */
  class ArrayForEach extends SystemFunction {

    override def call(context: XPathContext,
                      arguments: Array[Sequence]): ArrayItem = {
      val array: ArrayItem = arguments(0).head().asInstanceOf[ArrayItem]
      assert(array != null)
      val fn: Function = arguments(1).head().asInstanceOf[Function]
      val list: List[GroundedValue] = new ArrayList[GroundedValue](1)
      var i: Int = 0
      i = 0
      while (i < array.arrayLength()) {
        list.add(dynamicCall(fn, context, Array(array.get(i))).materialize())
          i =i+ 1

      }
      new SimpleArrayItem(list)
    }

  }

  /**
    * Implementation of the function array:for-each-pair(array, array, function) array
    */
  class ArrayForEachPair extends SystemFunction {

    override def call(context: XPathContext,
                      arguments: Array[Sequence]): ArrayItem = {
      val array1: ArrayItem = arguments(0).head().asInstanceOf[ArrayItem]
      assert(array1 != null)
      val array2: ArrayItem = arguments(1).head().asInstanceOf[ArrayItem]
      assert(array2 != null)
      val fn: Function = arguments(2).head().asInstanceOf[Function]
      val list: List[GroundedValue] = new ArrayList[GroundedValue](1)
      var i: Int = 0
      i = 0
      while (i < array1.arrayLength() && i < array2.arrayLength()) {
        list.add(
          dynamicCall(fn, context, Array(array1.get(i), array2.get(i)))
            .materialize())
        i =i+ 1
      }
      new SimpleArrayItem(list)
    }

  }

  /**
    * Implementation of the function array:get(array, xs:integer) =&gt; item()*
    */
  class ArrayGet extends SystemFunction {

    def call(context: XPathContext, arguments: Array[Sequence]): Sequence = {
      val array: ArrayItem = arguments(0).head().asInstanceOf[ArrayItem]
      val index: IntegerValue = arguments(1).head().asInstanceOf[IntegerValue]
      array.get(checkSubscript(index, array.arrayLength()) - 1)
    }

  }

  /**
    * Implementation of the function array:head(array) =&gt; item()*
    */
  class ArrayHead extends SystemFunction {

    def call(context: XPathContext, arguments: Array[Sequence]): Sequence = {
      val array: ArrayItem = arguments(0).head().asInstanceOf[ArrayItem]
      assert(array != null)
      if (array.arrayLength() == 0) {
        throw new XPathException("Argument to array:head is an empty array",
          "FOAY0001")
      }
      array.get(0)
    }

  }

  /**
    * Implementation of the function array:insert-before(array, xs:integer, item()*) =&gt; array
    */
  class ArrayInsertBefore extends SystemFunction {

    def call(context: XPathContext, arguments: Array[Sequence]): ArrayItem = {
      val array: ArrayItem = arguments(0).head().asInstanceOf[ArrayItem]
      assert(array != null)
      val index: Int = checkSubscript(
        arguments(1).head().asInstanceOf[IntegerValue],
        array.arrayLength() + 1) -
        1
      if (index < 0 || index > array.arrayLength()) {
        throw new XPathException("Specified position is not in range",
          "FOAY0001")
      }
      val newMember: Sequence = arguments(2)
      array.insert(index, newMember.materialize())
    }

  }

  /**
    * Implementation of the function array:join(arrays) =&gt; array
    */
  class ArrayJoin extends SystemFunction {

    override def call(context: XPathContext,
                      arguments: Array[Sequence]): ArrayItem = {
      val iterator: SequenceIterator = arguments(0).iterate()
      var array: ArrayItem = SimpleArrayItem.EMPTY_ARRAY
      var nextArray: ArrayItem = null
      while (({
        nextArray = iterator.next().asInstanceOf[ArrayItem]
        nextArray
      }) !=
        null) array = array.concat(nextArray)
      array
    }

  }

  /**
    * Implementation of the function array:put(arrays, index, newValue) =&gt; array
    */
  class ArrayPut extends SystemFunction {

    override def call(context: XPathContext,
                      arguments: Array[Sequence]): ArrayItem = {
      val array: ArrayItem = arguments(0).head().asInstanceOf[ArrayItem]
      val index: Int = checkSubscript(
        arguments(1).head().asInstanceOf[IntegerValue],
        array.arrayLength()) -
        1
      val newVal: GroundedValue = arguments(2).materialize()
      array.put(index, newVal)
    }

  }

  /**
    * Implementation of the function array:remove(array, xs:integer) =&gt; array
    */
  class ArrayRemove extends SystemFunction {

    def call(context: XPathContext, arguments: Array[Sequence]): ArrayItem = {
      val array: ArrayItem = arguments(0).head().asInstanceOf[ArrayItem]
      if (arguments(1).isInstanceOf[IntegerValue]) {
        val index: Int = checkSubscript(
          arguments(1).asInstanceOf[IntegerValue],
          array.arrayLength()) -
          1
        array.remove(index)
      }
      val positions: IntSet = new IntHashSet()
      val arg1: SequenceIterator = arguments(1).iterate()
      arg1.forEachOrFail((pos) => {
        val index: Int = checkSubscript(pos.asInstanceOf[IntegerValue],
          array.arrayLength()) -
          1
        positions.add(index)
      })
      array.removeSeveral(positions)
    }

  }

  /**
    * Implementation of the function array:reverse(array, xs:integer, xs:integer) =&gt; array
    */
  class ArrayReverse extends SystemFunction {

    def call(context: XPathContext, arguments: Array[Sequence]): ArrayItem = {
      val array: ArrayItem = arguments(0).head().asInstanceOf[ArrayItem]
      assert(array != null)
      val list: List[GroundedValue] = new ArrayList[GroundedValue](1)
      var i: Int = 0
      i = 0
      while (i < array.arrayLength()) {
        list.add(array.get(array.arrayLength() - i - 1))
        i =i+ 1
      }
      new SimpleArrayItem(list)
    }

  }

  /**
    * Implementation of the function array:size(array) =&gt; integer
    */
  class ArraySize extends SystemFunction {

    def call(context: XPathContext, arguments: Array[Sequence]): IntegerValue = {
      val array: ArrayItem = arguments(0).head().asInstanceOf[ArrayItem]
      assert(array != null)
      new Int64Value(array.arrayLength())
    }

  }

  /**
    * Implementation of the function array:subarray(array, xs:integer, xs:integer) =&gt; array
    */
  class ArraySubarray extends SystemFunction {

    def call(context: XPathContext, arguments: Array[Sequence]): ArrayItem = {
      val array: ArrayItem = arguments(0).head().asInstanceOf[ArrayItem]
      assert(array != null)
      val start: Int = checkSubscript(
        arguments(1).head().asInstanceOf[IntegerValue],
        array.arrayLength() + 1)
      var length: Int = 0
      if (arguments.length == 3) {
        val len: IntegerValue = arguments(2).head().asInstanceOf[IntegerValue]
        val signum: Int = len.signum()
        if (signum < 0) {
          throw new XPathException(
            "Specified length of subarray is less than zero",
            "FOAY0002")
        }
        length =
          if (signum == 0) 0 else checkSubscript(len, array.arrayLength())
      } else {
        length = array.arrayLength() - start + 1
      }
      if (start < 1) {
        throw new XPathException("Start position is less than one", "FOAY0001")
      }
      if (start > array.arrayLength() + 1) {
        throw new XPathException("Start position is out of bounds", "FOAY0001")
      }
      if (start + length > array.arrayLength() + 1) {
        throw new XPathException(
          "Specified length of subarray is too great for start position given",
          "FOAY0001")
      }
      array.subArray(start - 1, start + length - 1)
    }

  }

  /**
    * Implementation of the function array:tail(array) =&gt; item()*
    */
  class ArrayTail extends SystemFunction {

    def call(context: XPathContext, arguments: Array[Sequence]): ArrayItem = {
      val array: ArrayItem = arguments(0).head().asInstanceOf[ArrayItem]
      assert(array != null)
      if (array.arrayLength() < 1) {
        throw new XPathException("Argument to array:tail is an empty array",
          "FOAY0001")
      }
      array.remove(0)
    }

  }

  object ArrayToSequence {

    def toSequence(array: ArrayItem): Sequence = {
      val results: List[GroundedValue] = new ArrayList[GroundedValue]()
      for (seq <- array.members()) {
        results.add(seq.materialize())
      }
      new Chain(results)
    }

  }

  class ArrayToSequence extends SystemFunction {

    def call(context: XPathContext, arguments: Array[Sequence]): Sequence = {
      val array: ArrayItem = arguments(0).head().asInstanceOf[ArrayItem]
      toSequence(array)
    }

  }

  class ArrayFromSequence extends FoldingFunction {

    override def call(context: XPathContext, arguments: Array[Sequence]): ArrayItem =
      SimpleArrayItem.makeSimpleArrayItem(
        arguments(0).iterate())

    /**
      * Create the Fold object which is used to perform a streamed evaluation
      *
      * @param context             the dynamic evaluation context
      * @param additionalArguments the values of all arguments other than the first.
      * @return the Fold object used to compute the function
      */
    override def getFold(context: XPathContext,
                         additionalArguments: Sequence*): Fold = new Fold() {
      var members: List[GroundedValue] = new ArrayList()

      /**
        * Process one item in the input sequence, returning a new copy of the working data
        *
        * @param item the item to be processed from the input sequence
        */
      override def processItem(item: Item): Unit = {
        members.add(item)
      }

      /**
        * Ask whether the computation has completed. A function that can deliver its final
        * result without reading the whole input should return true; this will be followed
        * by a call on result() to deliver the final result.
        *
        * @return true if the result of the function is now available even though not all
        * items in the sequence have been processed
        */
      override def isFinished(): Boolean = false

      /**
        * Compute the final result of the function, when all the input has been processed
        *
        * @return the result of the function
        */
      override def result(): ArrayItem = new SimpleArrayItem(members)
    }

  }

}

class ArrayFunctionSet extends BuiltInFunctionSet {

  init()

  private def init(): Unit = {
    register("append",
      2,
      classOf[ArrayAppend],
      ArrayItemType.ANY_ARRAY_TYPE,
      ONE,
      0)
      .arg(0, ArrayItemType.ANY_ARRAY_TYPE, ONE | INS, null)
      .arg(1, AnyItemType, STAR | NAV, null)
    val filterFunctionType: ItemType = new SpecificFunctionType(
      Array(SequenceType.ANY_SEQUENCE),
      SequenceType.SINGLE_BOOLEAN)
    register("filter",
      2,
      classOf[ArrayFilter],
      ArrayItemType.ANY_ARRAY_TYPE,
      ONE,
      0)
      .arg(0, ArrayItemType.ANY_ARRAY_TYPE, ONE | INS, null)
      .arg(1, filterFunctionType, ONE | INS, null)
    register("flatten",
      1,
      classOf[ArrayFlatten],
      AnyItemType,
      STAR,
      0).arg(0, AnyItemType, STAR | ABS, null)
    val foldFunctionType: ItemType = new SpecificFunctionType(
      Array(SequenceType.ANY_SEQUENCE, SequenceType.ANY_SEQUENCE),
      SequenceType.ANY_SEQUENCE)
    register("fold-left",
      3,
      classOf[ArrayFoldLeft],
      AnyItemType,
      STAR,
      0)
      .arg(0, ArrayItemType.ANY_ARRAY_TYPE, ONE | INS, null)
      .arg(1, AnyItemType, STAR | NAV, null)
      .arg(2, foldFunctionType, ONE | INS, null)
    register("fold-right",
      3,
      classOf[ArrayFoldRight],
      AnyItemType,
      STAR,
      0)
      .arg(0, ArrayItemType.ANY_ARRAY_TYPE, ONE | INS, null)
      .arg(1, AnyItemType, STAR | NAV, null)
      .arg(2, foldFunctionType, ONE | INS, null)
    val forEachFunctionType: ItemType = new SpecificFunctionType(
      Array(SequenceType.ANY_SEQUENCE),
      SequenceType.ANY_SEQUENCE)
    register("for-each",
      2,
      classOf[ArrayForEach],
      AnyItemType,
      STAR,
      0)
      .arg(0, ArrayItemType.ANY_ARRAY_TYPE, ONE | INS, null)
      .arg(1, forEachFunctionType, ONE | INS, null)
    register("for-each-pair",
      3,
      classOf[ArrayForEachPair],
      AnyItemType,
      STAR,
      0)
      .arg(0, ArrayItemType.ANY_ARRAY_TYPE, ONE | INS, null)
      .arg(1, ArrayItemType.ANY_ARRAY_TYPE, ONE | INS, null)
      .arg(2, foldFunctionType, ONE | INS, null)
    register("get", 2, classOf[ArrayGet], AnyItemType, STAR, 0)
      .arg(0, ArrayItemType.ANY_ARRAY_TYPE, ONE | INS, null)
      .arg(1, BuiltInAtomicType.INTEGER, ONE | ABS, null)
    register("head", 1, classOf[ArrayHead], AnyItemType, STAR, 0)
      .arg(0, ArrayItemType.ANY_ARRAY_TYPE, ONE | INS, null)
    register("insert-before",
      3,
      classOf[ArrayInsertBefore],
      ArrayItemType.ANY_ARRAY_TYPE,
      ONE,
      0)
      .arg(0, ArrayItemType.ANY_ARRAY_TYPE, ONE | INS, null)
      .arg(1, BuiltInAtomicType.INTEGER, STAR | ABS, null)
      .arg(2, AnyItemType, STAR | NAV, null)
    register("join",
      1,
      classOf[ArrayJoin],
      ArrayItemType.ANY_ARRAY_TYPE,
      ONE,
      0).arg(0, ArrayItemType.ANY_ARRAY_TYPE, STAR | INS, null)
    register("put", 3, classOf[ArrayPut], ArrayItemType.ANY_ARRAY_TYPE, ONE, 0)
      .arg(0, ArrayItemType.ANY_ARRAY_TYPE, ONE | INS, null)
      .arg(1, BuiltInAtomicType.INTEGER, STAR | INS, null)
      .arg(2, AnyItemType, STAR | NAV, null)
    register("remove",
      2,
      classOf[ArrayRemove],
      ArrayItemType.ANY_ARRAY_TYPE,
      ONE,
      0)
      .arg(0, ArrayItemType.ANY_ARRAY_TYPE, ONE | INS, null)
      .arg(1, BuiltInAtomicType.INTEGER, STAR | ABS, null)
    register("reverse",
      1,
      classOf[ArrayReverse],
      ArrayItemType.ANY_ARRAY_TYPE,
      ONE,
      0).arg(0, ArrayItemType.ANY_ARRAY_TYPE, ONE | INS, null)
    register("size", 1, classOf[ArraySize], BuiltInAtomicType.INTEGER, ONE, 0)
      .arg(0, ArrayItemType.ANY_ARRAY_TYPE, ONE | INS, null)
    val sortFunctionType: ItemType = new SpecificFunctionType(
      Array(SequenceType.ANY_SEQUENCE),
      SequenceType.ATOMIC_SEQUENCE)
    register("sort",
      1,
      classOf[ArraySort],
      ArrayItemType.ANY_ARRAY_TYPE,
      ONE,
      0).arg(0, ArrayItemType.ANY_ARRAY_TYPE, ONE | INS, null)
    register("sort",
      2,
      classOf[ArraySort],
      ArrayItemType.ANY_ARRAY_TYPE,
      ONE,
      0)
      .arg(0, ArrayItemType.ANY_ARRAY_TYPE, ONE | INS, null)
      .arg(1, BuiltInAtomicType.STRING, OPT | ABS, null)
    register("sort",
      3,
      classOf[ArraySort],
      ArrayItemType.ANY_ARRAY_TYPE,
      ONE,
      0)
      .arg(0, ArrayItemType.ANY_ARRAY_TYPE, ONE | INS, null)
      .arg(1, BuiltInAtomicType.STRING, OPT | ABS, null)
      .arg(2, sortFunctionType, ONE | INS, null)
    register("subarray",
      2,
      classOf[ArraySubarray],
      ArrayItemType.ANY_ARRAY_TYPE,
      ONE,
      0)
      .arg(0, ArrayItemType.ANY_ARRAY_TYPE, ONE | INS, null)
      .arg(1, BuiltInAtomicType.INTEGER, ONE | ABS, null)
    register("subarray",
      3,
      classOf[ArraySubarray],
      ArrayItemType.ANY_ARRAY_TYPE,
      ONE,
      0)
      .arg(0, ArrayItemType.ANY_ARRAY_TYPE, ONE | INS, null)
      .arg(1, BuiltInAtomicType.INTEGER, ONE | ABS, null)
      .arg(2, BuiltInAtomicType.INTEGER, ONE | ABS, null)
    register("tail",
      1,
      classOf[ArrayTail],
      ArrayItemType.ANY_ARRAY_TYPE,
      ONE,
      0).arg(0, ArrayItemType.ANY_ARRAY_TYPE, ONE | INS, null)
    register("_to-sequence",
      1,
      classOf[ArrayToSequence],
      AnyItemType,
      STAR,
      0).arg(0, ArrayItemType.ANY_ARRAY_TYPE, ONE | INS, null)
    register("_from-sequence",
      1,
      classOf[ArrayFromSequence],
      ArrayItemType.ANY_ARRAY_TYPE,
      ONE,
      0).arg(0, AnyItemType, STAR | INS, null)
  }

  override def getNamespace(): String = NamespaceConstant.ARRAY_FUNCTIONS

  override def getConventionalPrefix(): String = "array"

}


