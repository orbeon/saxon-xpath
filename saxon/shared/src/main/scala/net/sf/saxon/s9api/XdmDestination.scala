package net.sf.saxon.s9api

import net.sf.saxon.event.Builder

import net.sf.saxon.event.PipelineConfiguration

import net.sf.saxon.event.Receiver

import net.sf.saxon.event.SequenceNormalizer

import net.sf.saxon.om.NodeInfo

import net.sf.saxon.om.TreeModel

import net.sf.saxon.serialize.SerializationProperties

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

  def getBaseURI(): URI = getDestinationBaseURI

  def setTreeModel(model: TreeModel): Unit = {
    this.treeModel = model
  }

  def getTreeModel(): TreeModel = treeModel

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

  def getXdmNode(): XdmNode = {
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
