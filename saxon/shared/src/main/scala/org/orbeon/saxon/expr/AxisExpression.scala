package org.orbeon.saxon.expr

import java.util
import java.util._

import org.orbeon.saxon.expr.AxisExpression._
import org.orbeon.saxon.expr.parser._
import org.orbeon.saxon.model._
import org.orbeon.saxon.om._
import org.orbeon.saxon.pattern._
import org.orbeon.saxon.trace.ExpressionPresenter
import org.orbeon.saxon.trans.XPathException
import org.orbeon.saxon.tree.iter.AxisIterator
import org.orbeon.saxon.tree.util.FastStringBuffer
import org.orbeon.saxon.utils.Configuration
import org.orbeon.saxon.value.Cardinality
import org.orbeon.saxon.z.{IntHashSet, IntIterator, IntSet}

import scala.beans.BeanProperty
import scala.util.control.Breaks._

object AxisExpression {

  private def getDiagnosticName(name: StructuredQName, env: StaticContext): String = {
    val uri: String = name.getURI
    if (uri.==("")) {
      name.getLocalPart
    } else {
      val resolver: NamespaceResolver = env.getNamespaceResolver
      val it = resolver.iteratePrefixes
      while (it.hasNext) {
        val prefix = it.next()
        if (uri == resolver.getURIForPrefix(prefix, useDefault = true)) {
          if (prefix.isEmpty)
            "Q{" + uri + "}" + name.getLocalPart
          else
            prefix + ":" + name.getLocalPart
        }
      }
    }
    "Q{" + uri + "}" + name.getLocalPart
  }

  private def getStartingNodeDescription(`type`: SchemaType): String = {
    val s = `type`.getDescription
    if (s.startsWith("of element")) {
      "a valid element named" + s.substring("of element".length)
    } else if (s.startsWith("of attribute")) {
      "a valid attribute named" + s.substring("of attribute".length)
    } else {
      "a node with " + (if (`type`.isSimpleType) "simple" else "complex") +
        " type " +
        s
    }
  }

  private def isPeerNodeTest(test: NodeTest): Boolean = {
    if (test == null)
      return false
    val uType = test.getUType
    if (uType.overlaps(UType.ELEMENT)) {
      false
    } else if (uType.overlaps(UType.DOCUMENT)) {
      uType == UType.DOCUMENT
    } else {
      true
    }
  }
}

class AxisExpression(@BeanProperty var axis: Int, nodeTest: NodeTest)
  extends Expression {

  private var test: NodeTest = nodeTest
  private var itemType: ItemType = null
  private var staticInfo: ContextItemStaticInfo = ContextItemStaticInfo.DEFAULT
  private var doneTypeCheck: Boolean = false
  private var doneOptimize: Boolean = false

  override def getExpressionName: String = "axisStep"

  override def simplify(): Expression = {
    val e2 = super.simplify()
    if (e2 != this)
      return e2
    if ((test == null || test == AnyNodeTest.getInstance) && (axis == AxisInfo.PARENT || axis == AxisInfo.ANCESTOR))
      test = MultipleNodeKindTest.PARENT_NODE
    this
  }

  override def typeCheck(visitor: ExpressionVisitor,
                         contextInfo: ContextItemStaticInfo): Expression = {
    val contextItemType: ItemType = contextInfo.getItemType
    val noWarnings: Boolean = doneOptimize || (doneTypeCheck && this.staticInfo.getItemType == contextItemType)
    doneTypeCheck = true
    if (contextItemType == ErrorType) {
      val err = new XPathException(
        "Axis step " + this + " cannot be used here: the context item is absent")
      err.setErrorCode("XPDY0002")
      err.setLocation(getLocation)
      throw err
    } else {
      staticInfo = contextInfo
    }
    val config: Configuration = visitor.getConfiguration
    if (contextItemType.getGenre != Genre.NODE) {
      val th: TypeHierarchy = config.getTypeHierarchy
      val relation: Affinity.Affinity =
        th.relationship(contextItemType, AnyNodeTest.getInstance)
      if (relation == Affinity.DISJOINT) {
        val err = new XPathException(
          "Axis step " + this +
            " cannot be used here: the context item is not a node")
        err.setIsTypeError(true)
        err.setErrorCode("XPTY0020")
        err.setLocation(getLocation)
        throw err
      } else if (relation == Affinity.OVERLAPS || relation == Affinity.SUBSUMES) {
        val thisExp: Expression =
          checkPlausibility(visitor, contextInfo, !noWarnings)
        if (Literal.isEmptySequence(thisExp)) {
          return thisExp
        }
        val exp: ContextItemExpression = new ContextItemExpression()
        ExpressionTool.copyLocationInfo(this, exp)
        val role: RoleDiagnostic =
          new RoleDiagnostic(RoleDiagnostic.AXIS_STEP, "", axis)
        role.setErrorCode("XPTY0020")
        val checker: ItemChecker =
          new ItemChecker(exp, AnyNodeTest.getInstance, role)
        ExpressionTool.copyLocationInfo(this, checker)
        val step: SimpleStepExpression =
          new SimpleStepExpression(checker, thisExp)
        ExpressionTool.copyLocationInfo(this, step)
        return step
      }
    }
    if (visitor.getStaticContext.getOptimizerOptions.isSet(
      OptimizerOptions.VOID_EXPRESSIONS)) {
      checkPlausibility(visitor, contextInfo, !noWarnings)
    } else {
      this
    }
  }

  private def checkPlausibility(visitor: ExpressionVisitor,
                                contextInfo: ContextItemStaticInfo,
                                warnings: Boolean): Expression = {
    val env: StaticContext = visitor.getStaticContext
    val config: Configuration = env.getConfiguration
    var contextType: ItemType = contextInfo.getItemType
    if (! contextType.isInstanceOf[NodeTest])
      contextType = AnyNodeTest.getInstance
    if (test != null &&
      !AxisInfo
        .getTargetUType(UType.ANY_NODE, axis)
        .overlaps(test.getUType)) {
      if (warnings) {
        visitor.issueWarning(
          "The " + AxisInfo.axisName(axis) + " axis will never select " +
            test.getUType.toStringWithIndefiniteArticle,
          getLocation)
      }
      Literal.makeEmptySequence
    }
    test match {
      case test1: NameTest if !test1.getNamespaceURI.isEmpty && axis == AxisInfo.NAMESPACE =>
        if (warnings) {
          visitor.issueWarning(
            "The names of namespace nodes are never prefixed, so this axis step will never select anything",
            getLocation)
        }
        Literal.makeEmptySequence
      case _ =>
    }
    val originUType: UType = contextType.getUType
    var targetUType: UType = AxisInfo.getTargetUType(originUType, axis)
    val testUType: UType = if (test == null) UType.ANY_NODE else test.getUType
    if (targetUType == UType.VOID) {
      if (warnings) {
        visitor.issueWarning(
          "The " + AxisInfo.axisName(axis) + " axis starting at " +
            originUType.toStringWithIndefiniteArticle +
            " will never select anything",
          getLocation)
      }
      Literal.makeEmptySequence
    }
    if (contextInfo.isParentless &&
      (axis == AxisInfo.PARENT || axis == AxisInfo.ANCESTOR)) {
      if (warnings) {
        visitor.issueWarning(
          "The " + AxisInfo.axisName(axis) +
            " axis will never select anything because the context item is parentless",
          getLocation)
      }
      Literal.makeEmptySequence
    }
    if (!targetUType.overlaps(testUType)) {
      if (warnings) {
        visitor.issueWarning(
          "The " + AxisInfo.axisName(axis) + " axis starting at " +
            originUType.toStringWithIndefiniteArticle +
            " will never select " +
            test.getUType.toStringWithIndefiniteArticle,
          getLocation
        )
      }
      Literal.makeEmptySequence
    }
    val nonSelf: Int = AxisInfo.excludeSelfAxis(axis)
    val kind: UType = if (test == null) UType.ANY_NODE else test.getUType
    if (axis != nonSelf) {
      val nonSelfTarget: UType = AxisInfo.getTargetUType(originUType, nonSelf)
      if (!nonSelfTarget.overlaps(testUType)) {
        axis = AxisInfo.SELF
        targetUType = AxisInfo.getTargetUType(originUType, axis)
      }
    }
    val target: ItemType = targetUType.toItemType
    itemType =
      if (test == null || test.isInstanceOf[AnyNodeTest]) target
      else if (target.isInstanceOf[AnyNodeTest] || targetUType.subsumes(
        test.getUType)) test
      else
        new CombinedNodeTest(target.asInstanceOf[NodeTest],
          Token.INTERSECT,
          test)
    val origin: Int = contextType.getPrimitiveType
    if (test != null) {
      contextType match {
        case test1: DocumentNodeTest if kind == UType.ELEMENT =>
          val elementTest: NodeTest =
            test1.getElementTest
          val outermostElementNames = elementTest.getRequiredNodeNames
          if (outermostElementNames.isDefined) {
            val selectedElementNames = test.getRequiredNodeNames
            if (selectedElementNames.isDefined) {
              if (axis == AxisInfo.CHILD) {
                if (selectedElementNames.get
                  .intersect(outermostElementNames.get)
                  .isEmpty) {
                  if (warnings) {
                    visitor.issueWarning(
                      "Starting at a document node, the step is selecting an element whose name " +
                        "is not among the names of child elements permitted for this document node type",
                      getLocation
                    )
                  }
                  Literal.makeEmptySequence
                }
                if (env.getPackageData.isSchemaAware && elementTest
                  .isInstanceOf[SchemaNodeTest] &&
                  outermostElementNames.get.size == 1) {
                  val oeni: IntIterator = outermostElementNames.get.iterator
                  val outermostElementName: Int =
                    if (oeni.hasNext) oeni.next() else -1
                  val decl: SchemaDeclaration =
                    config.getElementDeclaration(outermostElementName)
                  if (decl == null) {
                    if (warnings) {
                      visitor.issueWarning("Element " + config.getNamePool
                        .getEQName(outermostElementName) +
                        " is not declared in the schema",
                        getLocation)
                    }
                    itemType = elementTest
                  } else {
                    val contentType: SchemaType = decl.getType
                    itemType = new CombinedNodeTest(
                      elementTest,
                      Token.INTERSECT,
                      new ContentTypeTest(Type.ELEMENT,
                        contentType,
                        config,
                        true))
                  }
                } else {
                  itemType = elementTest
                }
                return this
              } else if (axis == AxisInfo.DESCENDANT) {
                val canMatchOutermost: Boolean = !selectedElementNames.get
                  .intersect(outermostElementNames.get)
                  .isEmpty
                if (!canMatchOutermost) {
                  val path: Expression = ExpressionTool.makePathExpression(
                    new AxisExpression(AxisInfo.CHILD, elementTest),
                    new AxisExpression(AxisInfo.DESCENDANT, test))
                  ExpressionTool.copyLocationInfo(this, path)
                  path.typeCheck(visitor, contextInfo)
                }
              }
            }
          }
        case _ =>
      }
      val contentType: SchemaType =
        contextType.asInstanceOf[NodeTest].getContentType
      if (contentType == AnyType.getInstance) {
        return this
      }
      if (!env.getPackageData.isSchemaAware) {
        val ct: SchemaType = test.getContentType
        if (!(ct == AnyType.getInstance || ct == Untyped.getInstance ||
          ct == AnySimpleType ||
          ct == BuiltInAtomicType.ANY_ATOMIC ||
          ct == BuiltInAtomicType.UNTYPED_ATOMIC ||
          ct == BuiltInAtomicType.STRING)) {
          if (warnings) {
            visitor.issueWarning(
              "The " + AxisInfo
                .axisName(axis) + " axis will never select any typed nodes, " +
                "because the expression is being compiled in an environment that is not schema-aware",
              getLocation
            )
          }
          Literal.makeEmptySequence
        }
      }
      val targetfp: Int = test.getFingerprint
      val targetName: StructuredQName = test.getMatchingNodeName
      if (contentType.isSimpleType) {
        if (warnings) {
          if ((axis == AxisInfo.CHILD || axis == AxisInfo.DESCENDANT ||
            axis == AxisInfo.DESCENDANT_OR_SELF) &&
            UType.PARENT_NODE_KINDS.union(UType.ATTRIBUTE).subsumes(kind)) {
            visitor.issueWarning(
              "The " + AxisInfo
                .axisName(axis) + " axis will never select any " +
                kind +
                " nodes when starting at " +
                (if (origin == Type.ATTRIBUTE) "an attribute node"
                else getStartingNodeDescription(contentType)),
              getLocation
            )
          } else if (axis == AxisInfo.CHILD && kind == UType.TEXT && getParentExpression.isInstanceOf[Atomizer]) {
            visitor.issueWarning(
              "Selecting the text nodes of an element with simple content may give the " +
                "wrong answer in the presence of comments or processing instructions. It is usually " +
                "better to omit the '/text()' step",
              getLocation
            )
          } else if (axis == AxisInfo.ATTRIBUTE) {
            val extensions: util.Iterator[_ <: SchemaType] = config.getExtensionsOfType(contentType)
            var found: Boolean = false
            if (targetfp == -1) {
              breakable {
                while (extensions.hasNext) {
                  val extension: ComplexType =
                    extensions.next().asInstanceOf[ComplexType]
                  if (extension.allowsAttributes()) {
                    found = true
                    break()
                  }
                }
              }
            } else {
              breakable {
                while (extensions.hasNext) {
                  val extension: ComplexType =
                    extensions.next().asInstanceOf[ComplexType]
                  try if (extension.getAttributeUseType(targetName) != null) {
                    found = true
                    break()
                  } catch {
                    case _: SchemaException =>
                  }
                }
              }
            }
            if (!found) {
              visitor.issueWarning(
                "The " + AxisInfo.axisName(axis) + " axis will never select " +
                  (if (targetName == null) "any attribute nodes"
                  else
                    "an attribute node named " + getDiagnosticName(targetName,
                      env)) +
                  " when starting at " +
                  getStartingNodeDescription(contentType),
                getLocation
              )
            }
          }
        }
      } else if (contentType.asInstanceOf[ComplexType].isSimpleContent &&
        (axis == AxisInfo.CHILD || axis == AxisInfo.DESCENDANT ||
          axis == AxisInfo.DESCENDANT_OR_SELF) &&
        UType.PARENT_NODE_KINDS.subsumes(kind)) {
        if (warnings) {
          visitor.issueWarning(
            "The " + AxisInfo.axisName(axis) + " axis will never select any " +
              kind +
              " nodes when starting at " +
              getStartingNodeDescription(contentType) +
              ", as this type requires simple content",
            getLocation
          )
        }
        Literal.makeEmptySequence
      } else if (contentType.asInstanceOf[ComplexType].isEmptyContent &&
        (axis == AxisInfo.CHILD || axis == AxisInfo.DESCENDANT ||
          axis == AxisInfo.DESCENDANT_OR_SELF)) {
        val iter: util.Iterator[_ <: SchemaType] = config.getExtensionsOfType(contentType)
        while (iter.hasNext) {
          val extension: ComplexType = iter.next().asInstanceOf[ComplexType]
          if (!extension.isEmptyContent) {
            this
          }
        }
        if (warnings) {
          visitor.issueWarning(
            "The " + AxisInfo.axisName(axis) + " axis will never select any" +
              " nodes when starting at " +
              getStartingNodeDescription(contentType) +
              ", as this type requires empty content",
            getLocation
          )
        }
        Literal.makeEmptySequence
      } else if (axis == AxisInfo.ATTRIBUTE) {
        if (targetfp == -1) {
          if (warnings) {
            if (!contentType.asInstanceOf[ComplexType].allowsAttributes()) {
              visitor.issueWarning(
                "The complex type " + contentType.getDescription +
                  " allows no attributes other than the standard attributes in the xsi namespace",
                getLocation)
            }
          }
        } else {
          try {
            var schemaType: SchemaType = null
            schemaType =
              if (targetfp == StandardNames.XSI_TYPE) BuiltInAtomicType.QNAME
              else if (targetfp == StandardNames.XSI_SCHEMA_LOCATION)
                BuiltInListType.ANY_URIS
              else if (targetfp == StandardNames.XSI_NO_NAMESPACE_SCHEMA_LOCATION)
                BuiltInAtomicType.ANY_URI
              else if (targetfp == StandardNames.XSI_NIL)
                BuiltInAtomicType.BOOLEAN
              else
                contentType
                  .asInstanceOf[ComplexType]
                  .getAttributeUseType(targetName)
            if (schemaType == null) {
              if (warnings) {
                visitor.issueWarning(
                  "The complex type " + contentType.getDescription + " does not allow an attribute named " +
                    getDiagnosticName(targetName, env),
                  getLocation)
                Literal.makeEmptySequence
              }
            } else {
              itemType = new CombinedNodeTest(
                test,
                Token.INTERSECT,
                new ContentTypeTest(Type.ATTRIBUTE, schemaType, config, false))
            }
          } catch {
            case _: SchemaException =>

          }
        }
      } else if (axis == AxisInfo.CHILD && kind == UType.ELEMENT) {
        try {
          var childfp: Int = targetfp
          if (targetName == null) {
            if (contentType
              .asInstanceOf[ComplexType]
              .containsElementWildcard()) {
              return this
            }
            val children: IntHashSet = new IntHashSet()
            contentType
              .asInstanceOf[ComplexType]
              .gatherAllPermittedChildren(children, ignoreWildcards = false)
            if (children.isEmpty) {
              if (warnings) {
                visitor.issueWarning(
                  "The complex type " + contentType.getDescription + " does not allow children",
                  getLocation)
              }
              Literal.makeEmptySequence
            }
            if (children.size == 1) {
              val iter: IntIterator = children.iterator
              if (iter.hasNext) {
                childfp = iter.next()
              }
            } else {
              return this
            }
          }
          val schemaType: SchemaType = contentType
            .asInstanceOf[ComplexType]
            .getElementParticleType(childfp, considerExtensions = true)
          if (schemaType == null) {
            if (warnings) {
              val childElement: StructuredQName =
                getConfiguration.getNamePool.getStructuredQName(childfp)
              var message: String = "The complex type " + contentType.getDescription + " does not allow a child element named " +
                getDiagnosticName(childElement, env)
              val permitted: IntHashSet = new IntHashSet()
              contentType
                .asInstanceOf[ComplexType]
                .gatherAllPermittedChildren(permitted, ignoreWildcards = false)
              if (!permitted.contains(-1)) {
                val kids: IntIterator = permitted.iterator
                breakable {
                  while (kids.hasNext) {
                    val kid: Int = kids.next()
                    val sq: StructuredQName =
                      getConfiguration.getNamePool.getStructuredQName(kid)
                    if (sq.getLocalPart == childElement.getLocalPart && kid != childfp) {
                      message += ". Perhaps the namespace is " +
                        (if (childElement.hasURI("")) "missing" else "wrong") +
                        ", and " +
                        sq.getEQName +
                        " was intended?"
                      break()
                    }
                  }
                }
              }
              visitor.issueWarning(message, getLocation)
            }
            Literal.makeEmptySequence
          } else {
            itemType = new CombinedNodeTest(
              test,
              Token.INTERSECT,
              new ContentTypeTest(Type.ELEMENT, schemaType, config, true))
            val computedCardinality: Int = contentType
              .asInstanceOf[ComplexType]
              .getElementParticleCardinality(childfp, considerExtensions = true)
            ExpressionTool.resetStaticProperties(this)
            if (computedCardinality == StaticProperty.ALLOWS_ZERO) {
              val childElement: StructuredQName =
                getConfiguration.getNamePool.getStructuredQName(childfp)
              visitor.issueWarning(
                "The complex type " + contentType.getDescription + " appears not to allow a child element named " +
                  getDiagnosticName(childElement, env),
                getLocation)
              Literal.makeEmptySequence
            }
            if (!Cardinality.allowsMany(computedCardinality) &&
              !getParentExpression.isInstanceOf[FirstItemExpression] &&
              !visitor.isOptimizeForPatternMatching) {
              FirstItemExpression.makeFirstItemExpression(this)
            }
          }
        } catch {
          case _: SchemaException =>

        }
      } else if (axis == AxisInfo.DESCENDANT && kind == UType.ELEMENT &&
        targetfp != -1) {
        val descendants: IntHashSet = new IntHashSet()
        contentType
          .asInstanceOf[ComplexType]
          .gatherAllPermittedDescendants(descendants)
        if (descendants.contains(-1)) {
          return this
        }
        if (descendants.contains(targetfp)) {
          val children: IntHashSet = new IntHashSet()
          contentType
            .asInstanceOf[ComplexType]
            .gatherAllPermittedChildren(children, ignoreWildcards = false)
          val usefulChildren: IntHashSet = new IntHashSet()
          var considerSelf: Boolean = false
          var considerDescendants: Boolean = false
          val kids: IntIterator = children.iterator
          while (kids.hasNext) {
            val c: Int = kids.next()
            if (c == targetfp) {
              usefulChildren.add(c)
              considerSelf = true
            }
            val st: SchemaType = contentType
              .asInstanceOf[ComplexType]
              .getElementParticleType(c, considerExtensions = true)
            if (st == null) {
              throw new AssertionError(
                "Can't find type for child element " + c)
            }
            st match {
              case complexType: ComplexType =>
                val subDescendants: IntHashSet = new IntHashSet()
                complexType.gatherAllPermittedDescendants(subDescendants)
                if (subDescendants.contains(targetfp)) {
                  usefulChildren.add(c)
                  considerDescendants = true
                }
              case _ =>
            }
          }
          itemType = test
          if (considerDescendants) {
            val st: SchemaType = contentType
              .asInstanceOf[ComplexType]
              .getDescendantElementType(targetfp)
            if (st != AnyType.getInstance) {
              itemType = new CombinedNodeTest(
                test,
                Token.INTERSECT,
                new ContentTypeTest(Type.ELEMENT, st, config, true))
            }
          }
          if (usefulChildren.size < children.size) {
            val childTest: NodeTest =
              makeUnionNodeTest(usefulChildren, config.getNamePool)
            val first: AxisExpression =
              new AxisExpression(AxisInfo.CHILD, childTest)
            ExpressionTool.copyLocationInfo(this, first)
            var nextAxis: Int = 0
            nextAxis =
              if (considerSelf)
                if (considerDescendants) AxisInfo.DESCENDANT_OR_SELF
                else AxisInfo.SELF
              else AxisInfo.DESCENDANT
            val next: AxisExpression =
              new AxisExpression(nextAxis, itemType.asInstanceOf[NodeTest])
            ExpressionTool.copyLocationInfo(this, next)
            val path: Expression =
              ExpressionTool.makePathExpression(first, next)
            ExpressionTool.copyLocationInfo(this, path)
            path.typeCheck(visitor, contextInfo)
          }
        } else {
          if (warnings) {
            visitor.issueWarning(
              "The complex type " + contentType.getDescription + " does not allow a descendant element named " +
                getDiagnosticName(targetName, env),
              getLocation)
          }
        }
      }
    }
    this
  }

  private def makeUnionNodeTest(elements: IntHashSet,
                                pool: NamePool): NodeTest = {
    var test: NodeTest = null
    val iter: IntIterator = elements.iterator
    while (iter.hasNext) {
      val fp: Int = iter.next()
      val nextTest: NodeTest = new NameTest(Type.ELEMENT, fp, pool)
      test =
        if (test == null) nextTest
        else new CombinedNodeTest(test, Token.UNION, nextTest)
    }
    test
  }

  def getContextItemType: ItemType = staticInfo.getItemType

  override def optimize(visitor: ExpressionVisitor,
                        contextInfo: ContextItemStaticInfo): Expression = {
    doneOptimize = true
    staticInfo = contextInfo
    this
  }

  override def getCost: Double = axis match {
    case AxisInfo.SELF | AxisInfo.PARENT | AxisInfo.ATTRIBUTE => 1
    case AxisInfo.CHILD | AxisInfo.FOLLOWING_SIBLING |
         AxisInfo.PRECEDING_SIBLING | AxisInfo.ANCESTOR |
         AxisInfo.ANCESTOR_OR_SELF =>
      5
    case _ => 20

  }

  override def equals(other: Any): Boolean =
    other.isInstanceOf[AxisExpression] && axis == other
      .asInstanceOf[AxisExpression]
      .axis &&
      Objects.equals(test, other.asInstanceOf[AxisExpression].test)

  override def computeHashCode(): Int = {
    var h: Int = 9375162 + axis << 20
    if (test != null) {
      h ^= test.getPrimitiveType << 16
      h ^= test.getFingerprint
    }
    h
  }

  def copy(rebindings: RebindingMap): Expression = {
    val a2: AxisExpression = new AxisExpression(axis, test)
    a2.itemType = itemType
    a2.staticInfo = staticInfo
    a2.doneTypeCheck = doneTypeCheck
    a2.doneOptimize = doneOptimize
    ExpressionTool.copyLocationInfo(this, a2)
    a2
  }

  override def computeSpecialProperties(): Int =
    StaticProperty.CONTEXT_DOCUMENT_NODESET | StaticProperty.SINGLE_DOCUMENT_NODESET |
      StaticProperty.NO_NODES_NEWLY_CREATED |
      (if (AxisInfo.isForwards(axis)) StaticProperty.ORDERED_NODESET
      else StaticProperty.REVERSE_DOCUMENT_ORDER) |
      (if (AxisInfo.isPeerAxis(axis) || isPeerNodeTest(test))
        StaticProperty.PEER_NODESET
      else 0) |
      (if (AxisInfo.isSubtreeAxis(axis)) StaticProperty.SUBTREE_NODESET else 0) |
      (if (axis == AxisInfo.ATTRIBUTE || axis == AxisInfo.NAMESPACE)
        StaticProperty.ATTRIBUTE_NS_NODESET
      else 0)

  def getItemType: ItemType = {
    if (itemType != null) {
      return itemType
    }
    val p: Int = AxisInfo.principalNodeType(axis)
    p match {
      case Type.ATTRIBUTE | Type.NAMESPACE => NodeKindTest.makeNodeKindTest(p)
      case _ =>
        if (test == null) {
          AnyNodeTest.getInstance
        } else {
          test
        }

    }
  }

  override def getStaticUType(contextItemType: UType): UType = {
    val reachable: UType = AxisInfo.getTargetUType(contextItemType, axis)
    if (test == null) {
      reachable
    } else {
      reachable.intersection(test.getUType)
    }
  }

  override def getIntrinsicDependencies: Int =
    StaticProperty.DEPENDS_ON_CONTEXT_ITEM

  def computeCardinality(): Int = {
    var originNodeType: NodeTest = null
    val nodeTest: NodeTest = test
    val contextItemType: ItemType = staticInfo.getItemType
    contextItemType match {
      case test1: NodeTest =>
        originNodeType = test1
      case _ =>
        if (contextItemType eq AnyItemType)
          originNodeType = AnyNodeTest.getInstance
        else
          StaticProperty.ALLOWS_ZERO_OR_MORE
    }
    if (axis == AxisInfo.ATTRIBUTE && nodeTest.isInstanceOf[NameTest]) {
      val contentType: SchemaType = originNodeType.getContentType
      contentType match {
        case complexType: ComplexType =>
          try
           complexType.getAttributeUseCardinality(nodeTest.getMatchingNodeName)
          catch {
            case _: SchemaException =>
              StaticProperty.ALLOWS_ZERO_OR_ONE
          }
        case _: SimpleType =>
          StaticProperty.EMPTY
        case _ =>
      }
      StaticProperty.ALLOWS_ZERO_OR_ONE
    } else if (axis == AxisInfo.DESCENDANT && nodeTest
      .isInstanceOf[NameTest] &&
      nodeTest.getPrimitiveType == Type.ELEMENT) {
      val contentType: SchemaType = originNodeType.getContentType
      contentType match {
        case complexType: ComplexType =>
          try complexType.getDescendantElementCardinality(nodeTest.getFingerprint)
          catch {
            case _: SchemaException => StaticProperty.ALLOWS_ZERO_OR_MORE
          }
        case _ =>
          StaticProperty.EMPTY
      }
    } else if (axis == AxisInfo.SELF) {
      StaticProperty.ALLOWS_ZERO_OR_ONE
    } else {
      StaticProperty.ALLOWS_ZERO_OR_MORE
    }
  }

  override def isSubtreeExpression: Boolean = AxisInfo.isSubtreeAxis(axis)

  def getNodeTest: NodeTest = test

  override def addToPathMap(pathMap: PathMap,
                             pathMapNodeSet: PathMap.PathMapNodeSet): PathMap.PathMapNodeSet = {
    var pmnSet = pathMapNodeSet
    if (pmnSet == null) {
      val cie: ContextItemExpression = new ContextItemExpression()
      pmnSet = new PathMap.PathMapNodeSet(pathMap.makeNewRoot(cie))
    }
    pmnSet.createArc(
      axis,
      if (test == null) AnyNodeTest.getInstance else test)
  }

  def isContextPossiblyUndefined: Boolean = staticInfo.isPossiblyAbsent

  def getContextItemStaticInfo: ContextItemStaticInfo = staticInfo

  override def toPattern(config: Configuration): Pattern = {
    var test: NodeTest = getNodeTest
    var pat: Pattern = null
    if (test == null) {
      test = AnyNodeTest.getInstance
    }
    if (test.isInstanceOf[AnyNodeTest] &&
      (axis == AxisInfo.CHILD || axis == AxisInfo.DESCENDANT ||
        axis == AxisInfo.SELF)) {
      test = MultipleNodeKindTest.CHILD_NODE
    }
    val kind: Int = test.getPrimitiveType
    if (axis == AxisInfo.SELF) {
      pat = new NodeTestPattern(test)
    } else if (axis == AxisInfo.ATTRIBUTE) {
      pat =
        if (kind == Type.NODE) new NodeTestPattern(NodeKindTest.ATTRIBUTE)
        else if (!AxisInfo.containsNodeKind(axis, kind))
          new NodeTestPattern(ErrorType)
        else new NodeTestPattern(test)
    } else if (axis == AxisInfo.CHILD || axis == AxisInfo.DESCENDANT ||
      axis == AxisInfo.DESCENDANT_OR_SELF) {
      pat =
        if (kind != Type.NODE && !AxisInfo.containsNodeKind(axis, kind))
          new NodeTestPattern(ErrorType)
        else new NodeTestPattern(test)
    } else if (axis == AxisInfo.NAMESPACE) {
      pat =
        if (kind == Type.NODE) new NodeTestPattern(NodeKindTest.NAMESPACE)
        else if (!AxisInfo.containsNodeKind(axis, kind))
          new NodeTestPattern(ErrorType)
        else new NodeTestPattern(test)
    } else {
      throw new XPathException("Only downwards axes are allowed in a pattern",
        "XTSE0340")
    }
    ExpressionTool.copyLocationInfo(this, pat)
    pat
  }

  override def getImplementationMethod: Int = Expression.ITERATE_METHOD

  override def iterate(context: XPathContext): SequenceIterator = {
    val item: Item = context.getContextItem
    if (item == null) {
      val err = new XPathException(
        "The context item for axis step " + this + " is absent")
      err.setErrorCode("XPDY0002")
      err.setXPathContext(context)
      err.setLocation(getLocation)
      err.setIsTypeError(true)
      throw err
    }
    try if (test == null) {
      item.asInstanceOf[NodeInfo].iterateAxis(axis)
    } else {
      item.asInstanceOf[NodeInfo].iterateAxis(axis, test)
    } catch {
      case _: ClassCastException =>
        val err = new XPathException(
          "The context item for axis step " + this + " is not a node")
        err.setErrorCode("XPTY0020")
        err.setXPathContext(context)
        err.setLocation(getLocation)
        err.setIsTypeError(true)
        throw err

      case err: UnsupportedOperationException =>
        err.getCause match {
          case ec: XPathException =>
            ec.maybeSetLocation(getLocation)
            ec.maybeSetContext(context)
            throw ec
          case _ =>
            dynamicError(err.getMessage, "XPST0010", context)
            null
        }

    }
  }

  def iterate(origin: NodeInfo): AxisIterator =
    if (test == null) {
      origin.iterateAxis(axis)
    } else {
      origin.iterateAxis(axis, test)
    }

  def export(destination: ExpressionPresenter): Unit = {
    destination.startElement("axis", this)
    destination.emitAttribute("name", AxisInfo.axisName(axis))
    destination.emitAttribute(
      "nodeTest",
      AlphaCode.fromItemType(
        if (test == null) AnyNodeTest.getInstance else test))
    destination.endElement()
  }

  override def toString: String = {
    val fsb: FastStringBuffer = new FastStringBuffer(FastStringBuffer.C16)
    fsb.append(AxisInfo.axisName(axis))
    fsb.append("::")
    fsb.append(if (test == null) "node()" else test.toString)
    fsb.toString
  }

  override def toShortString: String = {
    val fsb: FastStringBuffer = new FastStringBuffer(FastStringBuffer.C16)
    if (axis == AxisInfo.CHILD) {
      // no action
    } else if (axis == AxisInfo.ATTRIBUTE) {
      fsb.append("@")
    } else {
      fsb.append(AxisInfo.axisName(axis))
      fsb.append("::")
    }
    if (test == null) {
      fsb.append("node()")
    } else test match {
      case test1: NameTest =>
        if (test1.getNodeKind != AxisInfo.principalNodeType(axis))
          fsb.append(test.toString)
        else
          fsb.append(test.getMatchingNodeName.getDisplayName)
      case _ =>
        fsb.append(test.toString)
    }
    fsb.toString
  }

  override def getStreamerName: String = "AxisExpression"

  def getPreconditions: Set[Expression] = {
    val pre: HashSet[Expression] = new HashSet[Expression](1)
    val a: Expression = this.copy(new RebindingMap())
    a.setRetainedStaticContext(getRetainedStaticContext)
    pre.add(a)
    pre
  }
}
