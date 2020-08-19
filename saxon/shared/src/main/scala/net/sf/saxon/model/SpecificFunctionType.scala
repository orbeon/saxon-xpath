package net.sf.saxon.model

import net.sf.saxon.utils.Configuration
import net.sf.saxon.expr.Expression
import net.sf.saxon.expr.StaticProperty
import net.sf.saxon.expr.parser.RoleDiagnostic
import net.sf.saxon.functions.hof.FunctionSequenceCoercer
import net.sf.saxon.ma.arrays.ArrayItem
import net.sf.saxon.ma.arrays.ArrayItemType
import net.sf.saxon.ma.map.KeyValuePair
import net.sf.saxon.ma.map.MapItem
import net.sf.saxon.ma.map.MapType
import net.sf.saxon.om.Function
import net.sf.saxon.om.Item
import net.sf.saxon.trans.Err
import net.sf.saxon.trans.XPathException
import net.sf.saxon.tree.util.FastStringBuffer
import net.sf.saxon.value.SequenceType
import java.util.Objects
import java.util.Optional

import net.sf.saxon.query.AnnotationList

import scala.beans.{BeanProperty, BooleanBeanProperty}
import scala.jdk.CollectionConverters._

class SpecificFunctionType extends AnyFunctionType {

  private var argTypes: Array[SequenceType] = _
  private var resultType: SequenceType = _
  private var annotations: AnnotationList = AnnotationList.EMPTY
  private var config: Configuration = _

  def this(argTyps: Array[SequenceType], resultTyp: SequenceType) {
    this()
    argTypes = Objects.requireNonNull(argTyps)
    this.resultType = Objects.requireNonNull(resultTyp)
  }

  def this(argTyps: Array[SequenceType], resultTyp: SequenceType, annotationList: AnnotationList) = {
    this()
    this.argTypes = Objects.requireNonNull(argTyps)
    this.resultType = Objects.requireNonNull(resultTyp)
    this.annotations = Objects.requireNonNull(annotationList)
  }

  def getArity(): Int = argTypes.length

  override def getArgumentTypes(): Array[SequenceType] = argTypes

  override def getAnnotationAssertions(): AnnotationList = annotations

  override def isAtomizable(th: TypeHierarchy): Boolean = {
    if (getArity != 1) {
      false
    }
    val argType: ItemType = getArgumentTypes()(0).getPrimaryType
    th.isSubType(BuiltInAtomicType.INTEGER, argType)
  }

  override def toString(): String = {
    val sb: FastStringBuffer = new FastStringBuffer(100)
    sb.append("(function(")
    for (i <- 0 until argTypes.length) {
      sb.append(argTypes(i).toString)
      if (i < argTypes.length - 1) {
        sb.append(", ")
      }
    }
    sb.append(") as ")
    sb.append(resultType.toString)
    sb.cat(')')
    sb.toString
  }

  override def toExportString(): String = {
    val sb: FastStringBuffer = new FastStringBuffer(100)
    sb.append("(function(")
    for (i <- 0 until argTypes.length) {
      sb.append(argTypes(i).toExportString())
      if (i < argTypes.length - 1) {
        sb.append(", ")
      }
    }
    sb.append(") as ")
    sb.append(resultType.toExportString())
    sb.cat(')')
    sb.toString
  }

  override def equals(other: Any): Boolean = {
    if (other.isInstanceOf[SpecificFunctionType]) {
      val f2: SpecificFunctionType = other.asInstanceOf[SpecificFunctionType]
      if (resultType != f2.resultType) {
        false
      }
      if (argTypes.length != f2.argTypes.length) {
        false
      }
      for (i <- 0 until argTypes.length if argTypes(i) != f2.argTypes(i)) {
        false
      }
      if (getAnnotationAssertions != f2.getAnnotationAssertions) {
        false
      }
      true
    }
    false
  }

  override def hashCode(): Int = {
    var h: Int = resultType.hashCode ^ argTypes.length
    for (argType <- argTypes) {
      h ^= argType.hashCode
    }
    h
  }
  import Affinity._

  override def relationship(other: FunctionItemType, th: TypeHierarchy): Affinity =
    if (other == AnyFunctionType.getInstance || other
      .isInstanceOf[AnyFunctionTypeWithAssertions]) {
      Affinity.SUBSUMED_BY
    } else if (equals(other)) {
      Affinity.SAME_TYPE
    } else if (other.isInstanceOf[ArrayItemType] || other
      .isInstanceOf[MapType]) {
      val rrel: Affinity = other.relationship(this, th)
      rrel match {
        case SUBSUMES => Affinity.SUBSUMED_BY
        case SUBSUMED_BY => Affinity.SUBSUMES
        case _ => rrel

      }
    } else {
      if (argTypes.length != other.getArgumentTypes.length) {
        Affinity.DISJOINT
      }
      var wider: Boolean = false
      var narrower: Boolean = false
      for (i <- 0 until argTypes.length) {
        val argRel: Affinity =
          th.sequenceTypeRelationship(argTypes(i), other.getArgumentTypes()(i))
        argRel match {
          case DISJOINT => Affinity.DISJOINT
          case SUBSUMES => narrower = true
          case SUBSUMED_BY => wider = true
          case OVERLAPS =>
            wider = true
            narrower = true
          case _ =>

        }
      }
      val resRel: Affinity =
        th.sequenceTypeRelationship(resultType, other.getResultType)
      resRel match {
        case DISJOINT => Affinity.DISJOINT
        case SUBSUMES => wider = true
        case SUBSUMED_BY => narrower = true
        case OVERLAPS =>
          wider = true
          narrower = true
        case _ =>

      }
      if (wider) {
        if (narrower) {
          Affinity.OVERLAPS
        } else {
          Affinity.SUBSUMES
        }
      } else {
        if (narrower) {
          Affinity.SUBSUMED_BY
        } else {
          Affinity.SAME_TYPE
        }
      }
    }

  override def getDefaultPriority(): Double = {
    var prio: Double = 1
    for (st <- getArgumentTypes) {
      prio *= st.getPrimaryType.getNormalizedDefaultPriority
    }
    prio
  }

  override def matches(item: Item, th: TypeHierarchy): Boolean = {
    if (!(item.isInstanceOf[Function])) {
      false
    }
    if (item.isInstanceOf[MapItem]) {
      if (getArity == 1 &&
        argTypes(0).getCardinality == StaticProperty.EXACTLY_ONE &&
        argTypes(0).getPrimaryType.isPlainType) {
        for (pair <- item.asInstanceOf[MapItem].keyValuePairs().asScala) {
          try if (!resultType.matches(pair.value, th)) {
            false
          } catch {
            case e: XPathException => false

          }
        }
        true
      } else {
        false
      }
    }
    if (item.isInstanceOf[ArrayItem]) {
      if (getArity == 1 &&
        argTypes(0).getCardinality == StaticProperty.EXACTLY_ONE &&
        argTypes(0).getPrimaryType.isPlainType) {
        val rel: Affinity = th.relationship(argTypes(0).getPrimaryType,
          BuiltInAtomicType.INTEGER)
        if (!(rel == Affinity.SAME_TYPE || rel == Affinity.SUBSUMED_BY)) {
          false
        }
        for (member <- item.asInstanceOf[ArrayItem].members()) {
          try if (!resultType.matches(member, th)) {
            false
          } catch {
            case e: XPathException => false

          }
        }
        true
      } else {
        false
      }
    }
    val rel: Affinity =
      th.relationship(item.asInstanceOf[Function].getFunctionItemType, this)
    rel == Affinity.SAME_TYPE || rel == Affinity.SUBSUMED_BY
  }

  override def explainMismatch(item: Item,
                               th: TypeHierarchy): Optional[String] = {
    if (!(item.isInstanceOf[Function])) {
      Optional.empty()
    }
    if (item.isInstanceOf[MapItem]) {
      if (getArity == 1) {
        if (argTypes(0).getCardinality == StaticProperty.EXACTLY_ONE &&
          argTypes(0).getPrimaryType.isPlainType) {
          for (pair <- item.asInstanceOf[MapItem].keyValuePairs().asScala) {
            try if (!resultType.matches(pair.value, th)) {
              var s: String = "The supplied map contains an entry with key (" + pair.key +
                ") whose corresponding value (" +
                Err.depictSequence(pair.value) +
                ") is not an instance of the return type in the function signature (" +
                resultType +
                ")"
              val more: Optional[String] =
                resultType.explainMismatch(pair.value, th)
              if (more.isPresent) {
                s = s + ". " + more.get
              }
              Optional.of(s)
            } catch {
              case e: XPathException => Optional.empty()

            }
          }
        } else {
          val s: String = "The function argument is of type " + argTypes(0) +
            "; a map can only be supplied for a function type whose argument type is atomic"
          Optional.of(s)
        }
      } else {
        val s: String = "The function arity is " + getArity +
          "; a map can only be supplied for a function type with arity 1"
        Optional.of(s)
      }
    }
    if (item.isInstanceOf[ArrayItem]) {
      if (getArity == 1) {
        if (argTypes(0).getCardinality == StaticProperty.EXACTLY_ONE &&
          argTypes(0).getPrimaryType.isPlainType) {
          val rel: Affinity = th.relationship(argTypes(0).getPrimaryType,
            BuiltInAtomicType.INTEGER)
          if (!(rel == Affinity.SAME_TYPE || rel == Affinity.SUBSUMED_BY)) {
            val s: String = "The function expects an argument of type " + argTypes(
              0) +
              "; an array can only be supplied for a function that expects an integer"
            Optional.of(s)
          } else {
            for (member <- item.asInstanceOf[ArrayItem].members()) {
              try if (!resultType.matches(member, th)) {
                var s: String = "The supplied array contains an entry (" + Err
                  .depictSequence(member) +
                  ") is not an instance of the return type in the function signature (" +
                  resultType +
                  ")"
                val more: Optional[String] =
                  resultType.explainMismatch(member, th)
                if (more.isPresent) {
                  s = s + ". " + more.get
                }
                Optional.of(s)
              } catch {
                case e: XPathException => Optional.empty()

              }
            }
          }
        } else {
          val s: String = "The function argument is of type " + argTypes(0) +
            "; an array can only be supplied for a function type whose argument type is xs:integer"
          Optional.of(s)
        }
      } else {
        val s: String = "The function arity is " + getArity +
          "; an array can only be supplied for a function type with arity 1"
        Optional.of(s)
      }
    }
    val other: FunctionItemType =
      item.asInstanceOf[Function].getFunctionItemType
    if (getArity != item.asInstanceOf[Function].getArity) {
      val s: String = "The required function arity is " + getArity + "; the supplied function has arity " +
        item.asInstanceOf[Function].getArity
      Optional.of(s)
    }
    var rel: Affinity =
      th.sequenceTypeRelationship(resultType, other.getResultType)
    if (rel != Affinity.SAME_TYPE && rel != Affinity.SUBSUMES) {
      val s: String = "The return type of the required function is " + resultType +
        " but the return" +
        "type of the supplied function is " +
        other.getResultType
      Optional.of(s)
    }
    for (j <- 0 until getArity) {
      rel =
        th.sequenceTypeRelationship(argTypes(j), other.getArgumentTypes()(j))
      if (rel != Affinity.SAME_TYPE && rel != Affinity.SUBSUMED_BY) {
        val s: String = "The type of the " + RoleDiagnostic.ordinal(j + 1) + " argument of the required function is " +
          argTypes(j) +
          " but the declared" +
          "type of the corresponding argument of the supplied function is " +
          other.getArgumentTypes()(j)
        Optional.of(s)
      }
    }
    Optional.empty()
  }

  override def makeFunctionSequenceCoercer(exp: Expression,
                                           role: RoleDiagnostic): Expression =
    new FunctionSequenceCoercer(exp, this, role)

}
