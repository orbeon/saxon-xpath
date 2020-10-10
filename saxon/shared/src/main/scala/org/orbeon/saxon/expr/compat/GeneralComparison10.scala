package org.orbeon.saxon.expr.compat

import org.orbeon.saxon.utils.Configuration

import org.orbeon.saxon.expr._

import org.orbeon.saxon.expr.parser._

import org.orbeon.saxon.expr.sort.AtomicComparer

import org.orbeon.saxon.expr.sort.CodepointCollator

import org.orbeon.saxon.expr.sort.GenericAtomicComparer

import org.orbeon.saxon.functions.Number_1

import org.orbeon.saxon.lib.ConversionRules

import org.orbeon.saxon.lib.StringCollator

import org.orbeon.saxon.model._

import org.orbeon.saxon.om.Item

import org.orbeon.saxon.om.Sequence

import org.orbeon.saxon.om.SequenceIterator

import org.orbeon.saxon.trace.ExpressionPresenter

import org.orbeon.saxon.trans.XPathException

import org.orbeon.saxon.tree.iter.EmptyIterator

import org.orbeon.saxon.tree.iter.PrependSequenceIterator

import org.orbeon.saxon.value.AtomicValue

import org.orbeon.saxon.value.BooleanValue

import org.orbeon.saxon.value.DoubleValue

import org.orbeon.saxon.value.StringValue

import java.util.ArrayList

import java.util.List

import GeneralComparison10._

//import scala.collection.compat._
import scala.jdk.CollectionConverters._

import scala.util.control.Breaks._

object GeneralComparison10 {

  private def compare(a0: AtomicValue,
                      op: Int,
                      a1: AtomicValue,
                      comparer: AtomicComparer,
                      context: XPathContext): Boolean = {
    var atomicVal0 = a0
    var atomicVal1 = a1
    var atomicCom = comparer
    atomicCom = atomicCom.provideContext(context)
    val rules: ConversionRules = context.getConfiguration.getConversionRules
    val t0: BuiltInAtomicType = atomicVal0.getPrimitiveType
    val t1: BuiltInAtomicType = atomicVal1.getPrimitiveType
    if (t0.isPrimitiveNumeric || t1.isPrimitiveNumeric) {
      val v0: DoubleValue = Number_1.convert(atomicVal0, context.getConfiguration)
      val v1: DoubleValue = Number_1.convert(atomicVal1, context.getConfiguration)
      ValueComparison.compare(v0, op, v1, atomicCom, checkTypes = false)
    }
    if (t0 == BuiltInAtomicType.STRING || t1 == BuiltInAtomicType.STRING ||
      (t0 == BuiltInAtomicType.UNTYPED_ATOMIC && t1 == BuiltInAtomicType.UNTYPED_ATOMIC)) {
      val s0: StringValue = StringValue.makeStringValue(atomicVal0.getStringValueCS)
      val s1: StringValue = StringValue.makeStringValue(atomicVal1.getStringValueCS)
      ValueComparison.compare(s0, op, s1, atomicCom, checkTypes = false)
    }
    if (t0 == BuiltInAtomicType.UNTYPED_ATOMIC) {
      atomicVal0 = t1
        .getStringConverter(rules)
        .convert(atomicVal0.asInstanceOf[StringValue])
        .asAtomic()
    }
    if (t1 == BuiltInAtomicType.UNTYPED_ATOMIC) {
      atomicVal1 = t0
        .getStringConverter(rules)
        .convert(atomicVal1.asInstanceOf[StringValue])
        .asAtomic()
    }
    ValueComparison.compare(atomicVal0, op, atomicVal1, atomicCom, checkTypes = false)
  }

}

class GeneralComparison10(p0: Expression, op: Int, p1: Expression)
  extends BinaryExpression(p0, op, p1)
    with Callable {

  var singletonop: Int =
    GeneralComparison.getCorrespondingSingletonOperator(op)

  var comparer: AtomicComparer = _

  private var atomize0: Boolean = true

  private var atomize1: Boolean = true

  private var maybeBoolean0: Boolean = true

  private var maybeBoolean1: Boolean = true

  override def computeCardinality(): Int = StaticProperty.EXACTLY_ONE

  override def typeCheck(visitor: ExpressionVisitor,
                         contextInfo: ContextItemStaticInfo): Expression = {
    getLhs.typeCheck(visitor, contextInfo)
    getRhs.typeCheck(visitor, contextInfo)
    val env: StaticContext = visitor.getStaticContext
    var comp: StringCollator =
      visitor.getConfiguration.getCollation(env.getDefaultCollationName)
    if (comp == null) {
      comp = CodepointCollator.getInstance
    }
    val context: XPathContext = env.makeEarlyEvaluationContext()
    comparer = new GenericAtomicComparer(comp, context)
    if ((getLhsExpression.isInstanceOf[Literal]) && (getRhsExpression
      .isInstanceOf[Literal])) {
      Literal.makeLiteral(evaluateItem(context), this)
    }
    this
  }

  def setAtomicComparer(comparer: AtomicComparer): Unit = {
    this.comparer = comparer
  }

  override def optimize(visitor: ExpressionVisitor,
                        contextInfo: ContextItemStaticInfo): Expression = {
    val config: Configuration = visitor.getConfiguration
    val env: StaticContext = visitor.getStaticContext
    getLhs.optimize(visitor, contextInfo)
    getRhs.optimize(visitor, contextInfo)
    this.setLhsExpression(getLhsExpression.unordered(retainAllNodes = false, forStreaming = false))
    this.setRhsExpression(getRhsExpression.unordered(retainAllNodes = false, forStreaming = false))
    if ((getLhsExpression.isInstanceOf[Literal]) && (getRhsExpression
      .isInstanceOf[Literal])) {
      Literal.makeLiteral(evaluateItem(env.makeEarlyEvaluationContext()), this)
    }
    val th = config.getTypeHierarchy
    var type0: ItemType = getLhsExpression.getItemType
    var type1: ItemType = getRhsExpression.getItemType
    if (type0.isPlainType) {
      atomize0 = false
    }
    if (type1.isPlainType) {
      atomize1 = false
    }
    if (th.relationship(type0, BuiltInAtomicType.BOOLEAN) == Affinity.DISJOINT) {
      maybeBoolean0 = false
    }
    if (th.relationship(type1, BuiltInAtomicType.BOOLEAN) == Affinity.DISJOINT) {
      maybeBoolean1 = false
    }
    if (!maybeBoolean0 && !maybeBoolean1) {
      if (!(type0.isInstanceOf[AtomicType])) {
        this.setLhsExpression(
          Atomizer.makeAtomizer(getLhsExpression, null).simplify())
        type0 = getLhsExpression.getItemType
      }
      if (!(type1.isInstanceOf[AtomicType])) {
        this.setRhsExpression(
          Atomizer.makeAtomizer(getRhsExpression, null).simplify())
        type1 = getRhsExpression.getItemType
      }
      val n0: Affinity.Affinity = th.relationship(type0, NumericType.getInstance)
      val n1: Affinity.Affinity = th.relationship(type1, NumericType.getInstance)
      val maybeNumeric0: Boolean = n0 != Affinity.DISJOINT
      val maybeNumeric1: Boolean = n1 != Affinity.DISJOINT
      val numeric0: Boolean = n0 == Affinity.SUBSUMED_BY || n0 == Affinity.SAME_TYPE
      val numeric1: Boolean = n1 == Affinity.SUBSUMED_BY || n1 == Affinity.SAME_TYPE
      if (op == Token.EQUALS || op == Token.NE) {
        if ((!maybeNumeric0 && !maybeNumeric1) || (numeric0 && numeric1)) {
          val gc: GeneralComparison = new GeneralComparison20(getLhsExpression,
            op,
            getRhsExpression)
          gc.setRetainedStaticContext(getRetainedStaticContext)
          gc.setAtomicComparer(comparer)
          val binExp: Expression = visitor
            .obtainOptimizer()
            .optimizeGeneralComparison(visitor, gc, backwardsCompatible = false, contextInfo)
          ExpressionTool.copyLocationInfo(this, binExp)
          binExp.typeCheck(visitor, contextInfo).optimize(visitor, contextInfo)
        }
      } else if (numeric0 && numeric1) {
        val gc: GeneralComparison =
          new GeneralComparison20(getLhsExpression, op, getRhsExpression)
        gc.setRetainedStaticContext(getRetainedStaticContext)
        val binExp: Expression = visitor
          .obtainOptimizer()
          .optimizeGeneralComparison(visitor, gc, backwardsCompatible = false, contextInfo)
        ExpressionTool.copyLocationInfo(this, binExp)
        binExp.typeCheck(visitor, contextInfo).optimize(visitor, contextInfo)
      }
    }
    this
  }

  override def evaluateItem(context: XPathContext): BooleanValue =
    BooleanValue.get(effectiveBooleanValue(context))

  def call(context: XPathContext, arguments: Array[Sequence]): BooleanValue =
    BooleanValue.get(
      effectiveBooleanValue(arguments(0).iterate(),
        arguments(1).iterate(),
        context))

  override def effectiveBooleanValue(context: XPathContext): Boolean =
    effectiveBooleanValue(getLhsExpression.iterate(context),
      getRhsExpression.iterate(context),
      context)

  private def effectiveBooleanValue(iter0: SequenceIterator,
                                    iter1: SequenceIterator,
                                    context: XPathContext): Boolean = {
    var seqItr0 = iter0
    var seqItr1 = iter1
    val iter1used: Boolean = false
    if (maybeBoolean0) {
      val i01: Item = seqItr0.next()
      val i02: Item = if (i01 == null) null else seqItr0.next()
      if (i01.isInstanceOf[BooleanValue] && i02 == null) {
        seqItr0.close()
        val b: Boolean = ExpressionTool.effectiveBooleanValue(seqItr1)
        compare(i01.asInstanceOf[BooleanValue],
          singletonop,
          BooleanValue.get(b),
          comparer,
          context)
      }
      if (i01 == null && !maybeBoolean1) {
        seqItr0.close()
        return false
      }
      if (i02 != null) {
        seqItr0 = new PrependSequenceIterator(i02, seqItr0)
      }
      if (i01 != null) {
        seqItr0 = new PrependSequenceIterator(i01, seqItr0)
      }
    }
    if (maybeBoolean1) {
      val i11: Item = seqItr1.next()
      val i12: Item = if (i11 == null) null else seqItr1.next()
      if (i11.isInstanceOf[BooleanValue] && i12 == null) {
        seqItr1.close()
        val b: Boolean = ExpressionTool.effectiveBooleanValue(seqItr0)
        compare(BooleanValue.get(b),
          singletonop,
          i11.asInstanceOf[BooleanValue],
          comparer,
          context)
      }
      if (i11 == null && !maybeBoolean0) {
        seqItr1.close()
        return false
      }
      if (i12 != null) {
        seqItr1 = new PrependSequenceIterator(i12, seqItr1)
      }
      if (i11 != null) {
        seqItr1 = new PrependSequenceIterator(i11, seqItr1)
      }
    }
    if (atomize0) {
      seqItr0 = Atomizer.getAtomizingIterator(seqItr0, oneToOne = false)
    }
    if (atomize1) {
      seqItr1 = Atomizer.getAtomizingIterator(seqItr1, oneToOne = false)
    }
    if (seqItr0.isInstanceOf[EmptyIterator] || seqItr1
      .isInstanceOf[EmptyIterator]) {
      return false
    }
    if (op == Token.LT || op == Token.LE || op == Token.GT ||
      op == Token.GE) {
      val config: Configuration = context.getConfiguration
      val map: ItemMappingFunction = new ItemMappingFunction() {
        def mapItem(item: Item): DoubleValue =
          Number_1.convert(item.asInstanceOf[AtomicValue], config)
      }
      seqItr0 = new ItemMappingIterator(seqItr0, map, true)
      seqItr1 = new ItemMappingIterator(seqItr1, map, true)
    }
    var seq1: List[AtomicValue] = null
    var item0: AtomicValue = null
    breakable {
      while (({
        item0 = seqItr0.next().asInstanceOf[AtomicValue]
        item0
      }) != null) if (seqItr1 != null) {

        while (true) {
          val item1: AtomicValue = seqItr1.next().asInstanceOf[AtomicValue]
          if (item1 == null) {
            seqItr1 = null
            if (seq1 == null) {
              return false
            }
            break()
          }
          try {
            if (compare(item0, singletonop, item1, comparer, context)) {
              return true
            }
            if (seq1 == null) {
              seq1 = new ArrayList[AtomicValue](40)
            }
            seq1.add(item1)
          } catch {
            case e: XPathException => {
              e.maybeSetLocation(getLocation)
              e.maybeSetContext(context)
              throw e
            }

          }
        }
      } else {
        for (item1 <- seq1.asScala
             if compare(item0, singletonop, item1, comparer, context)) {
          true
        }
      }
    }
    false
  }

  def copy(rebindings: RebindingMap): Expression = {
    val gc: GeneralComparison10 = new GeneralComparison10(
      getLhsExpression.copy(rebindings),
      op,
      getRhsExpression.copy(rebindings))
    ExpressionTool.copyLocationInfo(this, gc)
    gc.setRetainedStaticContext(getRetainedStaticContext)
    gc.comparer = comparer
    gc.atomize0 = atomize0
    gc.atomize1 = atomize1
    gc.maybeBoolean0 = maybeBoolean0
    gc.maybeBoolean1 = maybeBoolean1
    gc
  }

  def getItemType: ItemType = BuiltInAtomicType.BOOLEAN

  override def explainExtraAttributes(out: ExpressionPresenter): Unit = {
    out.emitAttribute("cardinality", "many-to-many (1.0)")
    out.emitAttribute("comp", comparer.save())
  }

  override def tag(): String = "gc10"

}
