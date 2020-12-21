package org.orbeon.saxon.tree.wrapper

import org.orbeon.saxon.utils.Configuration

import org.orbeon.saxon.event.Receiver

import org.orbeon.saxon.model.Type

import org.orbeon.saxon.om.AtomicSequence

import org.orbeon.saxon.om.AxisInfo

import org.orbeon.saxon.om.NodeInfo

import org.orbeon.saxon.pattern.AnyNodeTest

import org.orbeon.saxon.s9api.Location

import org.orbeon.saxon.trans.XPathException

import org.orbeon.saxon.tree.iter.AxisIterator

import org.orbeon.saxon.tree.util.Navigator

import org.orbeon.saxon.value.UntypedAtomicValue

import java.util.function.Predicate

object SnapshotNode {

  def makeSnapshot(original: NodeInfo): SnapshotNode = {
    val vc: SnapshotNode = new SnapshotNode(original, original)
    val config: Configuration = original.getConfiguration
    val doc: VirtualTreeInfo = new VirtualTreeInfo(config)
    val docNr: Long =
      config.getDocumentNumberAllocator.allocateDocumentNumber()
    doc.setDocumentNumber(docNr)
    doc.setCopyAccumulators(true)
    vc.tree = doc
    doc.setRootNode(vc.getRoot)
    vc
  }

}

class SnapshotNode(base: NodeInfo, var pivot: NodeInfo)
  extends VirtualCopy(base, pivot.getRoot)
    with NodeInfo {

  override def wrap(node: NodeInfo): SnapshotNode = {
    val vc: SnapshotNode = new SnapshotNode(node, pivot)
    vc.tree = tree
    vc
  }

  override def getStringValueCS: CharSequence =
    if (Navigator.isAncestorOrSelf(original, pivot)) {
      pivot.getStringValueCS
    } else {
      original.getStringValueCS
    }

  override def getParent: NodeInfo = {
    if (parent == null) {
      val basep: NodeInfo = original.getParent
      if (basep == null) {
        return null
      }
      parent = wrap(basep)
    }
    parent
  }

  override def getRoot: NodeInfo = super.getRoot

  override def copy(out: Receiver, copyOptions: Int, locationId: Location): Unit = {
    Navigator.copy(this, out, copyOptions, locationId)
  }

  override def atomize(): AtomicSequence = getNodeKind match {
    case Type.ATTRIBUTE | Type.TEXT | Type.COMMENT |
         Type.PROCESSING_INSTRUCTION | Type.NAMESPACE =>
      original.atomize()
    case _ =>
      if (Navigator.isAncestorOrSelf(pivot, original)) {
        original.atomize()
      } else {
        new UntypedAtomicValue(pivot.getStringValueCS)
      }

  }

  override def isId: Boolean = original.isId

  override def isIdref(): Boolean = original.isIdref

  override def isNilled(): Boolean = original.isNilled

  override def getPublicId: String =
    if (original != null) original.getPublicId else null

  override def iterateAxis(axisNumber: Int,
                           nodeTest: Predicate[_ >: NodeInfo]): AxisIterator =
    getNodeKind match {
      case Type.ATTRIBUTE | Type.NAMESPACE | Type.TEXT | Type.COMMENT |
           Type.PROCESSING_INSTRUCTION =>
        super.iterateAxis(axisNumber, nodeTest)
      case _ =>
        if (!original.isSameNodeInfo(pivot) && Navigator.isAncestorOrSelf(
          original,
          pivot)) {
          axisNumber match {
            case AxisInfo.CHILD =>
              Navigator.filteredSingleton(getChildOfAncestorNode, nodeTest)
            case AxisInfo.DESCENDANT | AxisInfo.DESCENDANT_OR_SELF =>
              var iter: AxisIterator = new Navigator.DescendantEnumeration(
                this,
                axisNumber == AxisInfo.DESCENDANT_OR_SELF,
                true)
              if (!(nodeTest.isInstanceOf[AnyNodeTest])) {
                iter = new Navigator.AxisFilter(iter, nodeTest)
              }
              iter
            case _ => super.iterateAxis(axisNumber, nodeTest)

          }
        } else {
          super.iterateAxis(axisNumber, nodeTest)
        }

    }

  private def getChildOfAncestorNode: NodeInfo = {
    val pivotKind: Int = pivot.getNodeKind
    var p: SnapshotNode = wrap(pivot)
    if ((pivotKind == Type.ATTRIBUTE || pivotKind == Type.NAMESPACE) &&
      p.getParent.isSameNodeInfo(this)) return null
    while (true) {
      val q: SnapshotNode = p.getParent.asInstanceOf[SnapshotNode]
      if (q == null) {
        throw new AssertionError()
      }
      if (q.isSameNodeInfo(this))
        return p
      p = q
    }
    null
  }

  override def isIncludedInCopy(sourceNode: NodeInfo): Boolean =
    sourceNode.getNodeKind match {
      case Type.ATTRIBUTE | Type.NAMESPACE =>
        isIncludedInCopy(sourceNode.getParent)
      case _ =>
        Navigator.isAncestorOrSelf(pivot, sourceNode) || Navigator
          .isAncestorOrSelf(sourceNode, pivot)

    }

}
