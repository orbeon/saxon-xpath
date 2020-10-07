package org.orbeon.saxon.tree.wrapper

import org.orbeon.saxon.utils.Configuration

import org.orbeon.saxon.event.Receiver

import org.orbeon.saxon.model.BuiltInAtomicType

import org.orbeon.saxon.model.SchemaType

import org.orbeon.saxon.model.Type

import org.orbeon.saxon.model.Untyped

import org.orbeon.saxon.om.AtomicSequence

import org.orbeon.saxon.om.CopyOptions

import org.orbeon.saxon.om.NodeInfo

import org.orbeon.saxon.s9api.Location

import org.orbeon.saxon.trans.XPathException

import org.orbeon.saxon.value.UntypedAtomicValue

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