package net.sf.saxon.trans

import java.util

import net.sf.saxon.expr.Expression
import net.sf.saxon.expr.XPathContext
import net.sf.saxon.expr.XPathContextMajor
import scala.jdk.CollectionConverters._
import net.sf.saxon.expr.sort.AtomicMatchKey
import net.sf.saxon.expr.sort.LocalOrderComparer
import net.sf.saxon.lib.ConversionRules
import net.sf.saxon.lib.StringCollator
import net.sf.saxon.model.BuiltInAtomicType
import net.sf.saxon.model.StringConverter
import net.sf.saxon.model.UType
import net.sf.saxon.om.NodeInfo
import net.sf.saxon.om.SequenceIterator
import net.sf.saxon.om.StandardNames
import net.sf.saxon.om.TreeInfo
import net.sf.saxon.pattern.Pattern
import net.sf.saxon.regex.UnicodeString
import net.sf.saxon.tree.iter.EmptyIterator
import net.sf.saxon.tree.iter.ListIterator
import net.sf.saxon.tree.iter.ManualIterator
import net.sf.saxon.tree.iter.SingleNodeIterator
import net.sf.saxon.value.AtomicValue
import net.sf.saxon.value.UntypedAtomicValue

import scala.beans.{BeanProperty, BooleanBeanProperty}
import scala.collection.mutable
import scala.util.control.Breaks._

object KeyIndex {

  object Status extends Enumeration {

    val UNDER_CONSTRUCTION: Status = new Status()

    val BUILT: Status = new Status()

    val FAILED: Status = new Status()

    class Status extends Val

    implicit def convertValue(v: Value): Status = v.asInstanceOf[Status]

  }

  def getCollationKey(value: AtomicValue,
                      collation: StringCollator,
                      implicitTimezone: Int): AtomicMatchKey =
    if (UType.STRING_LIKE.subsumes(value.getUType)) {
      if (collation == null) {
        UnicodeString.makeUnicodeString(value.getStringValueCS)
      } else {
        collation.getCollationKey(value.getStringValue)
      }
    } else {
      value.getXPathComparable(ordered = false, collation, implicitTimezone)
    }

}

class KeyIndex(isRangeKey: Boolean) {

  import KeyIndex._

  var index: util.Map[AtomicMatchKey, Any] =
    if (isRangeKey) new util.TreeMap[AtomicMatchKey, Any]() else new util.HashMap[AtomicMatchKey, Any]()

  var keyTypesPresent: UType = UType.VOID

  var keyTypesConvertedFromUntyped: UType = UType.STRING_LIKE

  var untypedKeys: util.List[UntypedAtomicValue] = _

  var rules: ConversionRules = _

  var implicitTimezone: Int = _

  var collation: StringCollator = _

  var creatingThread: Long = Thread.currentThread().getId

  @BeanProperty
  var status: Status.Status = Status.UNDER_CONSTRUCTION

  def getUnderlyingMap: util.Map[AtomicMatchKey, Any] = index

  def isCreatedInThisThread: Boolean =
    creatingThread == Thread.currentThread().getId

  def buildIndex(/*keySet: KeyDefinitionSet, */
                 // need to create KeyDefinitionSet class
                 doc: TreeInfo,
                 context: XPathContext): Unit = {
    // val definitions: List[KeyDefinition] = keySet.getKeyDefinitions
    /* for (k <- 0 until definitions.size) {
       constructIndex(doc, definitions.get(k), context, k == 0)
     }*/
    this.rules = context.getConfiguration.getConversionRules
    this.implicitTimezone = context.getImplicitTimezone
    //this.collation = definitions.get(0).getCollation
  }

  def constructIndex(doc: TreeInfo,
                     /* keydef: KeyDefinition,*/
                     context: XPathContext,
                     isFirst: Boolean): Unit = {
    //val `match`: Pattern = keydef.getMatch
    val xc: XPathContextMajor = context.newContext()
    // xc.setOrigin(keydef)
    //xc.setCurrentComponent(keydef.getDeclaringComponent)
    xc.setTemporaryOutputState(StandardNames.XSL_KEY)
    //val map: SlotManager = keydef.getStackFrameMap

    /* `match`
       .selectNodes(doc, xc)
       .forEachOrFail((node) =>
         processNode(node.asInstanceOf[NodeInfo], keydef, xc, isFirst))*/
  }

  def processNode(node: NodeInfo,
                  /* keydef: KeyDefinition,*/
                  xc: XPathContext,
                  isFirst: Boolean): Unit = {
    val si: ManualIterator = new ManualIterator(node)
    xc.setCurrentIterator(si)
    //val collation: StringCollator = keydef.getCollation
    val implicitTimezone: Int = xc.getImplicitTimezone
    //val use: Expression = keydef.getUse
    //val useval: SequenceIterator = use.iterate(xc)
    /* if (keydef.isComposite) {
       val amks: List[AtomicMatchKey] = new ArrayList[AtomicMatchKey](4)
       useval.forEachOrFail(
         (keyVal) =>
           amks.add(
             getCollationKey(keyVal.asInstanceOf[AtomicValue],
               collation,
               implicitTimezone)))
       addEntry(new CompositeAtomicMatchKey(amks), node, isFirst)
     } else {*/
    var keyVal: AtomicValue = null
    /*while ((keyVal = useval.next().asInstanceOf[AtomicValue]) != null) {
      if (keyVal.isNaN) {
        //continue
      }*/
    val actualUType: UType = keyVal.getUType
    if (!keyTypesPresent.subsumes(actualUType)) {
      keyTypesPresent = keyTypesPresent.union(actualUType)
    }
    val amk: AtomicMatchKey =
      getCollationKey(keyVal, collation, implicitTimezone)
    /* if (actualUType == UType.UNTYPED_ATOMIC && keydef.isConvertUntypedToOther) {
       if (untypedKeys == null) {
         untypedKeys = new util.ArrayList[UntypedAtomicValue](20)
       }*/

    untypedKeys.add(keyVal.asInstanceOf[UntypedAtomicValue])
    addEntry(amk, node, isFirst)
  }


  def addEntry(`val`: AtomicMatchKey,
               curr: NodeInfo,
               isFirst: Boolean): Unit = {
    val value: AnyRef = index.get(`val`).asInstanceOf[AnyRef]
    if (value == null) {
      index.put(`val`, curr)
    } else {
      var nodes: util.List[NodeInfo] = null
      if (value.isInstanceOf[NodeInfo]) {
        nodes = new util.ArrayList[NodeInfo](4)
        nodes.add(value.asInstanceOf[NodeInfo])
        index.put(`val`, nodes)
      } else {
        nodes = value.asInstanceOf[util.List[NodeInfo]]
      }
      if (isFirst) {
        if (nodes.get(nodes.size - 1) != curr) {
          nodes.add(curr)
        }
      } else {
        val comparer: LocalOrderComparer = LocalOrderComparer.getInstance
        var found: Boolean = false
        var i: Int = nodes.size - 1
        breakable {   while (i >= 0) {
          val d: Int = comparer.compare(curr, nodes.get(i))
          if (d >= 0) {
            if (d == 0) {} else {
              nodes.add(i + 1, curr)
            }
            found = true
            break()
          }
          {
            i -= 1;
            i + 1
          }
        }
      }
        if (!found) {
          nodes.add(0, curr)
        }
      }
    }
  }

  def reindexUntypedValues(builtType: BuiltInAtomicType): Unit = {
    var builtAtomType = builtType
    val uType: UType = builtAtomType.getUType
    if (UType.STRING_LIKE.subsumes(uType)) {
      return
    }
    if (UType.NUMERIC.subsumes(uType)) {
      builtAtomType = BuiltInAtomicType.DOUBLE
    }
    val converter: StringConverter = builtAtomType.getStringConverter(rules)
    for (v <- untypedKeys.asScala) {
      val uk: AtomicMatchKey = getCollationKey(v, collation, implicitTimezone)
      val convertedValue: AtomicValue =
        converter.convertString(v.getStringValueCS).asAtomic()
      val amk: AtomicMatchKey =
        getCollationKey(convertedValue, collation, implicitTimezone)
      val value: AnyRef = index.get(uk).asInstanceOf[AnyRef]
      if (value.isInstanceOf[NodeInfo]) {
        addEntry(amk, value.asInstanceOf[NodeInfo], isFirst = false)
      } else {
        val nodes: List[NodeInfo] = value.asInstanceOf[List[NodeInfo]]
        for (node <- nodes) {
          addEntry(amk, node, isFirst = false)
        }
      }
    }
  }

  def isEmpty: Boolean = index.isEmpty

  def getNodes(soughtValue: AtomicValue): SequenceIterator = {
    if (untypedKeys != null &&
      !keyTypesConvertedFromUntyped.subsumes(soughtValue.getUType)) {
      reindexUntypedValues(soughtValue.getPrimitiveType)
    }
    val value: AnyRef =
      index.get(getCollationKey(soughtValue, collation, implicitTimezone)).asInstanceOf[AnyRef]
    entryIterator(value)
  }

  def entryIterator(value: AnyRef): SequenceIterator =
    if (value == null) {
      EmptyIterator.ofNodes()
    } else if (value.isInstanceOf[NodeInfo]) {
      SingleNodeIterator.makeIterator(value.asInstanceOf[NodeInfo])
    } else {
      val nodes: util.List[NodeInfo] = value.asInstanceOf[util.List[NodeInfo]]
      new ListIterator(nodes)
    }

  def getComposite(soughtValue: SequenceIterator): SequenceIterator = {
    val amks: util.List[AtomicMatchKey] = new util.ArrayList[AtomicMatchKey](4)
    soughtValue.forEachOrFail(
      (keyVal) =>
        amks.add(
          getCollationKey(keyVal.asInstanceOf[AtomicValue],
            collation,
            implicitTimezone)))
    val value: AnyRef = index.get(new CompositeAtomicMatchKey(amks.asScala.toList)).asInstanceOf[AnyRef]
    entryIterator(value)
  }

  class CompositeAtomicMatchKey(var keys: List[AtomicMatchKey])
    extends AtomicMatchKey {

    def asAtomic(): AtomicValue = throw new UnsupportedOperationException()

    override def equals(obj: Any): Boolean = {
      if (obj.isInstanceOf[CompositeAtomicMatchKey] &&
        obj.asInstanceOf[CompositeAtomicMatchKey].keys.size ==
          keys.size) {
        val keys2: util.List[AtomicMatchKey] =
          obj.asInstanceOf[CompositeAtomicMatchKey].keys.asJava
        for (i <- 0 until keys.size if keys.asJava.get(i) != keys2.get(i)) {
          false
        }
        return true
      }
      false
    }

    override def hashCode(): Int = {
      var h: Int = 0x8ab27cd6
      for (amk <- keys) {
        h ^= amk.hashCode
        h = h << 1
      }
      h
    }

  }

}
