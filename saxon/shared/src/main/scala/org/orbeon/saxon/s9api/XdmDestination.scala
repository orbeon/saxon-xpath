package org.orbeon.saxon.s9api

import org.orbeon.saxon.event.Builder

import org.orbeon.saxon.event.PipelineConfiguration

import org.orbeon.saxon.event.Receiver

import org.orbeon.saxon.event.SequenceNormalizer

import org.orbeon.saxon.om.NodeInfo

import org.orbeon.saxon.om.TreeModel

import org.orbeon.saxon.serialize.SerializationProperties

import java.net.URI

class XdmDestination extends AbstractDestination {

  var treeModel: TreeModel = TreeModel.TINY_TREE

  var builder: Builder = _

  def setBaseURI(baseURI: URI): Unit = {
    if (!baseURI.isAbsolute) {
      throw new IllegalArgumentException("Supplied base URI must be absolute")
    }
    this.setDestinationBaseURI(baseURI)
  }

  def getBaseURI: URI = getDestinationBaseURI

  def setTreeModel(model: TreeModel): Unit = {
    this.treeModel = model
  }

  def getTreeModel: TreeModel = treeModel

  def getReceiver(pipe: PipelineConfiguration,
                  params: SerializationProperties): Receiver = {
    var model: TreeModel = treeModel
    if (model == null) {
      val m: Int = pipe.getParseOptions.getTreeModel
      if (m != Builder.UNSPECIFIED_TREE_MODEL) {
        model = TreeModel.getTreeModel(m)
      }
      if (model == null) {
        model = TreeModel.TINY_TREE
      }
    }
    builder = model.makeBuilder(pipe)
    val systemId: String =
      if (getBaseURI == null) null else getBaseURI.toASCIIString()
    if (systemId != null) {
      builder.setUseEventLocation(false)
      builder.setBaseURI(systemId)
    }
    val sn: SequenceNormalizer = params.makeSequenceNormalizer(builder)
    sn.setSystemId(systemId)
    sn.onClose(helper.getListeners)
    sn
  }

  def close(): Unit = ()

  def getXdmNode: XdmNode = {
    if (builder == null) {
      throw new IllegalStateException("The document has not yet been built")
    }
    val node: NodeInfo = builder.getCurrentRoot
    if (node == null) null else XdmValue.wrap(node).asInstanceOf[XdmNode]
  }

  def reset(): Unit = {
    builder = null
  }

}
