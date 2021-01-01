package org.orbeon.saxon.model

import java.util.Objects

import org.orbeon.saxon.expr.parser.RoleDiagnostic
import org.orbeon.saxon.expr.{Expression, StaticProperty}
import org.orbeon.saxon.functions.hof.FunctionSequenceCoercer
import org.orbeon.saxon.ma.arrays.{ArrayItem, ArrayItemType}
import org.orbeon.saxon.ma.map.{MapItem, MapType}
import org.orbeon.saxon.om.{Function, Item}
import org.orbeon.saxon.query.AnnotationList
import org.orbeon.saxon.trans.{Err, XPathException}
import org.orbeon.saxon.tree.util.FastStringBuffer
import org.orbeon.saxon.utils.Configuration
import org.orbeon.saxon.value.SequenceType

//import scala.collection.compat._
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

  def getArity: Int = argTypes.length

  override def getArgumentTypes: Array[SequenceType] = argTypes

  override def getAnnotationAssertions: AnnotationList = annotations

  override def isAtomizable(th: TypeHierarchy): Boolean = {
    if (getArity != 1) {
      return false
    }
    val argType: ItemType = getArgumentTypes(0).getPrimaryType
    th.isSubType(BuiltInAtomicType.INTEGER, argType)
  }

  override def toString: String = {
    val sb = new FastStringBuffer(100)
    sb.append("(function(")
    for (i <- argTypes.indices) {
      sb.append(argTypes(i).toString)
      if (i < argTypes.length - 1)
        sb.append(", ")
    }
    sb.append(") as ")
    sb.append(resultType.toString)
    sb.cat(')')
    sb.toString
  }

  override def toExportString: String = {
    val sb = new FastStringBuffer(100)
    sb.append("(function(")
    for (i <- argTypes.indices) {
      sb.append(argTypes(i).toExportString)
      if (i < argTypes.length - 1)
        sb.append(", ")
    }
    sb.append(") as ")
    sb.append(resultType.toExportString)
    sb.cat(')')
    sb.toString
  }

  override def equals(other: Any): Boolean = {
    other match {
      case f2: SpecificFunctionType =>
        if (resultType != f2.resultType)
          return false
        if (argTypes.length != f2.argTypes.length)
          return false
        for (i <- argTypes.indices if argTypes(i) != f2.argTypes(i))
          return false
        if (getAnnotationAssertions != f2.getAnnotationAssertions)
          return false
        return true
      case _ =>
    }
    false
  }

  override def hashCode: Int = {
    var h = resultType.hashCode ^ argTypes.length
    for (argType <- argTypes)
      h ^= argType.hashCode
    h
  }
  import Affinity._

  override def relationship(other: FunctionItemType, th: TypeHierarchy): Affinity =
    if (other == AnyFunctionType || other
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
        return Affinity.DISJOINT
      }
      var wider: Boolean = false
      var narrower: Boolean = false
      for (i <- argTypes.indices) {
        val argRel = th.sequenceTypeRelationship(argTypes(i), other.getArgumentTypes(i))
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
      val resRel = th.sequenceTypeRelationship(resultType, other.getResultType)
      resRel match {
        case DISJOINT => return Affinity.DISJOINT
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

  override def getDefaultPriority: Double = {
    var prio: Double = 1
    for (st <- getArgumentTypes) {
      prio *= st.getPrimaryType.getNormalizedDefaultPriority
    }
    prio
  }

  override def matches(item: Item, th: TypeHierarchy): Boolean = {
    if (!item.isInstanceOf[Function]) {
      return false
    }
    item match {
      case mapItem: MapItem =>
        if (getArity == 1 &&
          argTypes(0).getCardinality == StaticProperty.EXACTLY_ONE &&
          argTypes(0).getPrimaryType.isPlainType) {
          for (pair <- mapItem.keyValuePairs.asScala) {
            try if (!resultType.matches(pair.value, th)) {
              return false
            } catch {
              case _: XPathException => return false
            }
          }
          return true
        } else {
          false
        }
      case arrayItem: ArrayItem =>
        if (getArity == 1 &&
          argTypes(0).getCardinality == StaticProperty.EXACTLY_ONE &&
          argTypes(0).getPrimaryType.isPlainType) {
          val rel: Affinity = th.relationship(argTypes(0).getPrimaryType,
            BuiltInAtomicType.INTEGER)
          if (!(rel == Affinity.SAME_TYPE || rel == Affinity.SUBSUMED_BY)) {
            return false
          }
          for (member <- arrayItem.members) {
            try if (!resultType.matches(member, th)) {
              return false
            } catch {
              case _: XPathException => return false
            }
          }
          return true
        } else {
          false
        }
      case _ =>
        val rel = th.relationship(item.asInstanceOf[Function].getFunctionItemType, this)
        rel == Affinity.SAME_TYPE || rel == Affinity.SUBSUMED_BY
    }
  }

  override def explainMismatch(item: Item, th: TypeHierarchy): Option[String] = {

    if (! item.isInstanceOf[Function])
      return None

    item match {
      case mapItem: MapItem =>
        if (getArity == 1) {
          if (argTypes(0).getCardinality == StaticProperty.EXACTLY_ONE && argTypes(0).getPrimaryType.isPlainType) {
            for (pair <- mapItem.keyValuePairs.asScala) {
              try
                if (!resultType.matches(pair.value, th)) {
                  var s: String = "The supplied map contains an entry with key (" + pair.key +
                    ") whose corresponding value (" +
                    Err.depictSequence(pair.value) +
                    ") is not an instance of the return type in the function signature (" +
                    resultType +
                    ")"
                  val more = resultType.explainMismatch(pair.value, th)
                  if (more.isDefined)
                    s = s + ". " + more.get
                  return Some(s)
                } catch {
                  case _: XPathException =>
                    return None
                }
              }

          } else {
            val s = "The function argument is of type " + argTypes(0) +
              "; a map can only be supplied for a function type whose argument type is atomic"
            return Some(s)
          }
        } else {
          val s = "The function arity is " + getArity +
            "; a map can only be supplied for a function type with arity 1"
          return Some(s)
        }
      case _ =>
    }

    item match {
      case arrayItem: ArrayItem =>
        if (getArity == 1) {
          if (argTypes(0).getCardinality == StaticProperty.EXACTLY_ONE &&
            argTypes(0).getPrimaryType.isPlainType) {
            val rel: Affinity = th.relationship(argTypes(0).getPrimaryType,
              BuiltInAtomicType.INTEGER)
            if (!(rel == Affinity.SAME_TYPE || rel == Affinity.SUBSUMED_BY)) {
              val s = "The function expects an argument of type " + argTypes(
                0) +
                "; an array can only be supplied for a function that expects an integer"
              return Some(s)
            } else {
              for (member <- arrayItem.members) {
                try if (!resultType.matches(member, th)) {
                  var s: String = "The supplied array contains an entry (" + Err
                    .depictSequence(member) +
                    ") is not an instance of the return type in the function signature (" +
                    resultType +
                    ")"
                  val more: Option[String] =
                    resultType.explainMismatch(member, th)
                  if (more.isDefined) {
                    s = s + ". " + more.get
                  }
                  return Some(s)
                } catch {
                  case _: XPathException =>
                    return None
                }
              }
            }
          } else {
            val s = "The function argument is of type " + argTypes(0) +
              "; an array can only be supplied for a function type whose argument type is xs:integer"
            return Some(s)
          }
        } else {
          val s = "The function arity is " + getArity +
            "; an array can only be supplied for a function type with arity 1"
          return Some(s)
        }
      case _ =>
    }
    val other = item.asInstanceOf[Function].getFunctionItemType
    if (getArity != item.asInstanceOf[Function].getArity) {
      val s = "The required function arity is " + getArity + "; the supplied function has arity " +
        item.asInstanceOf[Function].getArity
      return Some(s)
    }
    var rel = th.sequenceTypeRelationship(resultType, other.getResultType)
    if (rel != Affinity.SAME_TYPE && rel != Affinity.SUBSUMES) {
      val s = "The return type of the required function is " + resultType +
        " but the return" +
        "type of the supplied function is " +
        other.getResultType
      return Some(s)
    }
    for (j <- 0 until getArity) {
      rel = th.sequenceTypeRelationship(argTypes(j), other.getArgumentTypes(j))
      if (rel != Affinity.SAME_TYPE && rel != Affinity.SUBSUMED_BY) {
        val s = "The type of the " + RoleDiagnostic.ordinal(j + 1) + " argument of the required function is " +
          argTypes(j) +
          " but the declared" +
          "type of the corresponding argument of the supplied function is " +
          other.getArgumentTypes(j)
        return Some(s)
      }
    }
    None
  }

  override def makeFunctionSequenceCoercer(exp: Expression,
                                           role: RoleDiagnostic): Expression =
    new FunctionSequenceCoercer(exp, this, role)

}
