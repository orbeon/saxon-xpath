package net.sf.saxon.tree.wrapper

import net.sf.saxon.utils.Configuration

import net.sf.saxon.event.Receiver

import net.sf.saxon.model.BuiltInAtomicType

import net.sf.saxon.model.SchemaType

import net.sf.saxon.model.Type

import net.sf.saxon.model.Untyped

import net.sf.saxon.om.AtomicSequence

import net.sf.saxon.om.CopyOptions

import net.sf.saxon.om.NodeInfo

import net.sf.saxon.s9api.Location

import net.sf.saxon.trans.XPathException

import net.sf.saxon.value.UntypedAtomicValue

import VirtualUntypedCopy._


object VirtualUntypedCopy {

  def makeVirtualUntypedTree(original: NodeInfo, root: NodeInfo): VirtualCopy = {
    var lOriginal = original
    var lRoot = root
    var vc: VirtualCopy = null
    while (lOriginal
      .isInstanceOf[VirtualUntypedCopy] && lOriginal.getParent == null) {
      lOriginal = lOriginal.asInstanceOf[VirtualUntypedCopy].original
      lRoot = lRoot.asInstanceOf[VirtualUntypedCopy].original
    }
    vc = new VirtualUntypedCopy(lOriginal, lRoot)
    val config: Configuration = lOriginal.getConfiguration
    val doc: VirtualTreeInfo = new VirtualTreeInfo(config, vc)
    vc.tree = doc
    vc
  }

}

class VirtualUntypedCopy  (base: NodeInfo, root: NodeInfo)
  extends VirtualCopy(base, root) {

  override def getSchemaType: SchemaType = getNodeKind match {
    case Type.ELEMENT => Untyped.getInstance
    case Type.ATTRIBUTE => BuiltInAtomicType.UNTYPED_ATOMIC
    case _ => super.getSchemaType

  }

  override def atomize(): AtomicSequence = getNodeKind match {
    case Type.ELEMENT | Type.ATTRIBUTE =>
      new UntypedAtomicValue(getStringValueCS)
    case _ => super.atomize()

  }

  override def copy(out: Receiver, copyOptions: Int, locationId: Location): Unit = {
    super.copy(out, copyOptions & ~CopyOptions.TYPE_ANNOTATIONS, locationId)
  }

  override  def wrap(node: NodeInfo): VirtualCopy = {
    val vc: VirtualUntypedCopy = new VirtualUntypedCopy(node, root)
    vc.tree = tree
    vc
  }

  override def isNilled(): Boolean = false

}