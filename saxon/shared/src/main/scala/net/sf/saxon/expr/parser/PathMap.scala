package net.sf.saxon.expr.parser

import net.sf.saxon.expr._

import net.sf.saxon.functions.Doc

import net.sf.saxon.functions.DocumentFn

import net.sf.saxon.functions.ResolveURI

import net.sf.saxon.lib.Logger

import net.sf.saxon.om.AxisInfo

import net.sf.saxon.pattern.AnyNodeTest

import net.sf.saxon.pattern.NodeKindTest

import net.sf.saxon.pattern.NodeTest

import net.sf.saxon.trans.XPathException

import java.net.URI

import java.net.URISyntaxException

import java.util._

import PathMap._

import scala.beans.{BeanProperty, BooleanBeanProperty}

import scala.jdk.CollectionConverters._
import scala.util.control.Breaks._

object PathMap {

  class PathMapNode {

    var arcs: List[PathMapArc] = new ArrayList[PathMapArc]()

    @BooleanBeanProperty
    var returnable: Boolean = _

    @BooleanBeanProperty
    var atomized: Boolean = _

    var hasUnknownDependencies: Boolean = _

    def createArc(axis: Int, test: NodeTest): PathMapNode = {
      for (a <- arcs.asScala if a.getAxis == axis && a.getNodeTest == test) {
        a.getTarget
      }
      val target: PathMapNode = new PathMapNode()
      val arc: PathMapArc = new PathMapArc(axis, test, target)
      arcs.add(arc)
      target
    }

    def createArc(axis: Int, test: NodeTest, target: PathMapNode): Unit = {
      for (a <- arcs.asScala
           if a.getAxis == axis && a.getNodeTest == test && a.getTarget == target) {
        a.getTarget.setReturnable(
          a.getTarget.isReturnable || target.isReturnable)
        if (target.isAtomized) {
          a.getTarget.setAtomized()
        }
        return
      }
      val arc: PathMapArc = new PathMapArc(axis, test, target)
      arcs.add(arc)
    }

    def getArcs: Array[PathMapArc] =
      arcs.toArray(Array.ofDim[PathMapArc](arcs.size))

    def hasReachableReturnables: Boolean = {
      if (isReturnable)
        return true
      for (arc <- arcs.asScala if arc.getTarget.hasReachableReturnables) {
        true
      }
      false
    }

    def setAtomized(): Unit = {
      this.atomized = true
    }

    def setHasUnknownDependencies(): Unit = {
      hasUnknownDependencies = true
    }

    def allPathsAreWithinStreamableSnapshot(): Boolean = {
      if (hasUnknownDependencies || isReturnable || isAtomized)
        return false
      for (arc <- arcs.asScala) {
        val axis: Int = arc.getAxis
        if (axis == AxisInfo.ATTRIBUTE) {
          val next: PathMapNode = arc.getTarget
          if (next.isReturnable) {
            return false
          }
          if (next.getArcs.length != 0 && !next
            .allPathsAreWithinStreamableSnapshot())
            return false
        } else if (axis == AxisInfo.SELF || axis == AxisInfo.ANCESTOR || axis == AxisInfo.ANCESTOR_OR_SELF ||
          axis == AxisInfo.PARENT) {
          val next: PathMapNode = arc.getTarget
          if (next.isAtomized) {
            return false
          }
          if (!next.allPathsAreWithinStreamableSnapshot())
            return false
        } else {
          return false
        }
      }
      return true
    }
  }

  class PathMapRoot(root: Expression) extends PathMapNode {

    @BeanProperty
    var rootExpression: Expression = root

    var isDownwardsOnly: Boolean = _

  }

  class PathMapArc(@BeanProperty var axis: Int,
                   var test: NodeTest,
                   @BeanProperty var target: PathMapNode) {

    def getNodeTest: NodeTest = test

  }

  class PathMapNodeSet extends HashSet[PathMapNode] {

    def this(singleton: PathMapNode) = {
      this()
      add(singleton)
    }

    def createArc(axis: Int, test: NodeTest): PathMapNodeSet = {
      val targetSet: PathMapNodeSet = new PathMapNodeSet()
      for (node <- this.asScala) {
        targetSet.add(node.createArc(axis, test))
      }
      targetSet
    }

    def addNodeSet(nodes: PathMapNodeSet): Unit = {
      if (nodes != null) {
        for (node <- nodes.asScala) {
          add(node)
        }
      }
    }

    def setAtomized(): Unit = {
      for (node <- this.asScala) {
        node.setAtomized()
      }
    }

    def setReturnable(isReturned: Boolean): Unit = {
      for (node <- this.asScala) {
        node.setReturnable(isReturned)
      }
    }

    def hasReachableReturnables: Boolean =
      this.asScala.find(_.hasReachableReturnables).map(_ => true).getOrElse(false)

    def allPathsAreWithinStreamableSnapshot(): Boolean =
      this.asScala
        .find(!_.allPathsAreWithinStreamableSnapshot())
        .map(_ => false)
        .getOrElse(true)

    def addDescendants(): Unit = {
      for (node <- this.asScala) {
        node.createArc(AxisInfo.DESCENDANT, AnyNodeTest.getInstance)
      }
    }

    def setHasUnknownDependencies(): Unit = {
      for (node <- this.asScala) {
        node.setHasUnknownDependencies()
      }
    }

  }

}

class PathMap(exp: Expression) {

  private var pathMapRoots: List[PathMapRoot] = new ArrayList[PathMapRoot]()

  private var pathsForVariables: HashMap[Binding, PathMapNodeSet] =
    new HashMap[Binding, PathMapNodeSet]()

  val finalNodes: PathMapNodeSet = exp.addToPathMap(this, null)

  if (finalNodes != null) {
    for (node <- finalNodes.asScala) {
      node.setReturnable(true)
    }
  }

  def makeNewRoot(exp: Expression): PathMapRoot = {
    for (r <- pathMapRoots.asScala if exp.isEqual(r.getRootExpression)) {
      r
    }
    val root: PathMapRoot = new PathMapRoot(exp)
    pathMapRoots.add(root)
    root
  }

  def getPathMapRoots: Array[PathMapRoot] =
    pathMapRoots.toArray(Array.ofDim[PathMapRoot](pathMapRoots.size))

  def registerPathForVariable(binding: Binding,
                              nodeset: PathMapNodeSet): Unit = {
    pathsForVariables.put(binding, nodeset)
  }

  def getPathForVariable(binding: Binding): PathMapNodeSet =
    pathsForVariables.get(binding)

  def getContextDocumentRoot: PathMapRoot = {
    val roots: Array[PathMap.PathMapRoot] = getPathMapRoots
    var contextRoot: PathMapRoot = null
    for (root <- roots) {
      val newRoot: PathMapRoot = reduceToDownwardsAxes(root)
      if (newRoot.getRootExpression.isInstanceOf[RootExpression]) {
        if (contextRoot != null) {
          throw new IllegalStateException(
            "More than one context document root found in path map")
        } else {
          contextRoot = newRoot
        }
      }
    }
    contextRoot
  }

  def getContextItemRoot: PathMapRoot = {
    val roots: Array[PathMap.PathMapRoot] = getPathMapRoots
    var contextRoot: PathMapRoot = null
    for (root <- roots
         if root.getRootExpression.isInstanceOf[ContextItemExpression]) {
      if (contextRoot != null) {
        throw new IllegalStateException(
          "More than one context document root found in path map")
      } else {
        contextRoot = root
      }
    }
    contextRoot
  }

  def getRootForDocument(requiredUri: String): PathMapRoot = {
    val roots: Array[PathMap.PathMapRoot] = getPathMapRoots
    var requiredRoot: PathMapRoot = null
    for (root <- roots) {
      val newRoot: PathMapRoot = reduceToDownwardsAxes(root)
      val exp: Expression = newRoot.getRootExpression
      var baseUri: String = null
      if (exp.isCallOn(classOf[Doc])) {
        baseUri = exp.getStaticBaseURIString
      } else if (exp.isCallOn(classOf[DocumentFn])) {
        baseUri = exp.getStaticBaseURIString
      } else {
        //continue
      }
      val arg: Expression = exp.asInstanceOf[SystemFunctionCall].getArg(0)
      var suppliedUri: String = null
      if (arg.isInstanceOf[Literal]) {
        try {
          val argValue: String =
            arg.asInstanceOf[Literal].getValue.getStringValue
          suppliedUri =
            if (baseUri == null)
              if (new URI(argValue).isAbsolute) argValue else null
            else ResolveURI.makeAbsolute(argValue, baseUri).toString
        } catch {
          case err: URISyntaxException => suppliedUri = null

          case err: XPathException => suppliedUri = null

        }
      }
      if (requiredUri == suppliedUri) {
        if (requiredRoot != null) {
          throw new IllegalStateException(
            "More than one document root found in path map for " +
              requiredUri)
        } else {
          requiredRoot = newRoot
        }
      }
    }
    requiredRoot
  }

  def reduceToDownwardsAxes(root: PathMap.PathMapRoot): PathMapRoot = {
    if (root.isDownwardsOnly)
      return root
    var newRoot: PathMapRoot = root
    if (root.getRootExpression.isInstanceOf[ContextItemExpression]) {
      val slash: RootExpression = new RootExpression()
      newRoot = makeNewRoot(slash)
      var i: Int = root.arcs.size - 1
      while (i >= 0) {
        val arc: PathMapArc = root.arcs.get(i)
        val axis: Int = arc.getAxis
        axis match {
          case AxisInfo.ATTRIBUTE | AxisInfo.NAMESPACE => {
            val newTarget: PathMapNode = new PathMapNode()
            newTarget.arcs.add(arc)
            newRoot.createArc(AxisInfo.DESCENDANT,
              NodeKindTest.ELEMENT,
              newTarget)
          }
          case _ => {
            newRoot.createArc(AxisInfo.DESCENDANT_OR_SELF,
              arc.getNodeTest,
              arc.getTarget)
          }

        }
        {
          i -= 1;
          i + 1
        }
      }
      breakable {
        for (i <- 0 until pathMapRoots.size if pathMapRoots.get(i) == root) {
          pathMapRoots.remove(i)
          break()
        }
      }
    }
    val nodeStack: Stack[PathMapNode] = new Stack[PathMapNode]()
    nodeStack.push(newRoot)
    reduceToDownwardsAxes(newRoot, nodeStack)
    newRoot.isDownwardsOnly = true
    newRoot
  }

  private def reduceToDownwardsAxes(root: PathMapRoot,
                                    nodeStack: Stack[PathMapNode]): Unit = {
    val node: PathMapNode = nodeStack.peek()
    if (node.hasUnknownDependencies) {
      root.setHasUnknownDependencies()
    }
    for (i <- 0 until node.arcs.size) {
      nodeStack.push((node.arcs.get(i)).getTarget)
      reduceToDownwardsAxes(root, nodeStack)
      nodeStack.pop()
    }
    var i: Int = node.arcs.size - 1
    while (i >= 0) {
      val thisArc: PathMapArc = node.arcs.get(i)
      val grandParent: PathMapNode =
        (if (nodeStack.size < 2) null else nodeStack.get(nodeStack.size - 2))
      var lastAxis: Int = -1
      if (grandParent != null) {
        for (arc1 <- grandParent.arcs.asScala) {
          val arc: PathMapArc = (arc1)
          if (arc.getTarget == node) {
            lastAxis = arc.getAxis
          }
        }
      }
      thisArc.getAxis match {
        case AxisInfo.ANCESTOR_OR_SELF | AxisInfo.DESCENDANT_OR_SELF =>
          if (thisArc.getNodeTest == NodeKindTest.DOCUMENT) {
            node.arcs.remove(i)
            for (arc <- thisArc.getTarget.arcs.asScala) {
              root.arcs.add(arc)
            }
          } else {}
        case AxisInfo.ANCESTOR | AxisInfo.FOLLOWING | AxisInfo.PRECEDING => {
          if (thisArc.getAxis != AxisInfo.DESCENDANT_OR_SELF) {
            root.createArc(AxisInfo.DESCENDANT_OR_SELF,
              thisArc.getNodeTest,
              thisArc.getTarget)
            node.arcs.remove(i)
          }
        }
        case AxisInfo.ATTRIBUTE | AxisInfo.CHILD | AxisInfo.DESCENDANT |
             AxisInfo.NAMESPACE =>
        case AxisInfo.FOLLOWING_SIBLING | AxisInfo.PRECEDING_SIBLING =>
          if (grandParent != null) {
            grandParent.createArc(lastAxis,
              thisArc.getNodeTest,
              thisArc.getTarget)
            node.arcs.remove(i)
          } else {
            root.createArc(AxisInfo.CHILD,
              thisArc.getNodeTest,
              thisArc.getTarget)
            node.arcs.remove(i)
          }
        case AxisInfo.PARENT => {
          if (lastAxis == AxisInfo.CHILD || lastAxis == AxisInfo.ATTRIBUTE ||
            lastAxis == AxisInfo.NAMESPACE) {
            if (node.isReturnable) {
              grandParent.setReturnable(true)
            }
            val target: PathMapNode = thisArc.getTarget
            for (a <- 0 until target.arcs.size) {
              grandParent.arcs.add(target.arcs.get(a))
            }
            node.arcs.remove(i)
          } else if (lastAxis == AxisInfo.DESCENDANT) {
            if (thisArc.getTarget.arcs.isEmpty) {
              grandParent.createArc(AxisInfo.DESCENDANT_OR_SELF,
                thisArc.getNodeTest)
            } else {
              grandParent.createArc(AxisInfo.DESCENDANT_OR_SELF,
                thisArc.getNodeTest,
                thisArc.getTarget)
            }
            node.arcs.remove(i)
          } else {
            if (thisArc.getTarget.arcs.isEmpty) {
              root.createArc(AxisInfo.DESCENDANT_OR_SELF, thisArc.getNodeTest)
            } else {
              root.createArc(AxisInfo.DESCENDANT_OR_SELF,
                thisArc.getNodeTest,
                thisArc.getTarget)
            }
            node.arcs.remove(i)
          }
        }
        case AxisInfo.SELF => {
          node.arcs.remove(i)
        }

      }
      {
        i -= 1;
        i + 1
      }
    }
  }

  def diagnosticDump(out: Logger): Unit = {
    for (i <- 0 until pathMapRoots.size) {
      out.info("\nROOT EXPRESSION " + i)
      val mapRoot: PathMapRoot = pathMapRoots.get(i)
      if (mapRoot.hasUnknownDependencies) {
        out.info("  -- has unknown dependencies --")
      }
      val exp: Expression = mapRoot.rootExpression
      exp.explain(out)
      out.info("\nTREE FOR EXPRESSION " + i)
      showArcs(out, mapRoot, 2)
    }
  }

  private def showArcs(out: Logger, node: PathMapNode, indent: Int): Unit = {
    val pad: String =
      "                                           ".substring(0, indent)
    val arcs: List[PathMapArc] = node.arcs
    for (arc <- arcs.asScala) {
      out.info(
        pad + AxisInfo.axisName(arc.axis) + "::" + arc.test.toString +
          (if (arc.target.isAtomized) " @" else "") +
          (if (arc.target.isReturnable) " #" else "") +
          (if (arc.target.hasUnknownDependencies) " ...??" else ""))
      showArcs(out, arc.target, indent + 2)
    }
  }

}
