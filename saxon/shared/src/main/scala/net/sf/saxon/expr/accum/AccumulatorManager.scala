////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
package net.sf.saxon.expr.accum

import net.sf.saxon.expr.XPathContext

import net.sf.saxon.expr.XPathContextMajor

import net.sf.saxon.om.NodeInfo

import net.sf.saxon.om.TreeInfo

import net.sf.saxon.trans.XPathException

import net.sf.saxon.tree.tiny.TinyTree

import net.sf.saxon.tree.wrapper.VirtualCopy

import net.sf.saxon.tree.wrapper.VirtualTreeInfo

import java.util.HashMap

import java.util.Map

import java.util.Set

import java.util.WeakHashMap

import AccumulatorManager._




object AccumulatorManager {

  private var MARKER: AccumulatorData = new AccumulatorData(null)

}

/**
  * Manager for accumulator functions (XSLT 3.0).
  *
  * <p>In principle there is a dataset containing accumulator values for every accumulator/document pair.
  * In addition, if the same stylesheet is run concurrently in multiple transformations, and a document
  * is shared between these transformations, the data values for the accumulator can differ between the
  * two transformations, because the accumulator rules can depend on values of stylesheet parameters
  * or on context variables such as current-dateTime.</p>
  *
  * <p>It is important that the accumulator data for a tree does not cause the tree to be locked in
  * memory for the duration of a transformation. We therefore keep a weak reference to the tree. For
  * each tree, until it is garbage collected, there is a map from accumulator names to accumulator data.</p>
  */
class AccumulatorManager {

  @transient private var accumulatorDataIndex: WeakHashMap[
    TreeInfo,
    Map[Accumulator, IAccumulatorData]] =
    new WeakHashMap[TreeInfo, Map[Accumulator, IAccumulatorData]]()

  @transient private var applicableAccumulators: WeakHashMap[
    TreeInfo,
    Set[_ <: Accumulator]] = new WeakHashMap[TreeInfo, Set[_ <: Accumulator]]()

  def setApplicableAccumulators(tree: TreeInfo,
                                accumulators: Set[_ <: Accumulator]): Unit = {
    applicableAccumulators.put(tree, accumulators)
  }

  def isApplicable(tree: TreeInfo, accumulator: Accumulator): Boolean = {
    val accSet: Set[_ <: Accumulator] = applicableAccumulators.get(tree)
    accSet == null || accSet.contains(accumulator)
  }

  def getAccumulatorData(doc: TreeInfo,
                         acc: Accumulator,
                         context: XPathContext): IAccumulatorData =
    synchronized {
      var map: Map[Accumulator, IAccumulatorData] =
        accumulatorDataIndex.get(doc)
      if (map != null) {
        val data: IAccumulatorData = map.get(acc)
        if (data != null) {
          if (data == MARKER) {
            throw new XPathException(
              "Accumulator " + acc.getAccumulatorName.getDisplayName +
                " requires access to its own value",
              "XTDE3400")
          }
         return data
        }
      } else {
        map = new HashMap[Accumulator, IAccumulatorData]()
        map.put(acc, MARKER)
        accumulatorDataIndex.put(doc, map)
      }
      if (doc.isInstanceOf[VirtualTreeInfo] &&
          doc.asInstanceOf[VirtualTreeInfo].isCopyAccumulators) {
        val original: NodeInfo =
          doc.getRootNode.asInstanceOf[VirtualCopy].getOriginalNode
        val originalData: IAccumulatorData =
          getAccumulatorData(original.getTreeInfo, acc, context)
        val vad: VirtualAccumulatorData =
          new VirtualAccumulatorData(originalData)
        map.put(acc, vad)
        vad
      } else if (doc.isInstanceOf[TinyTree] && doc
                   .asInstanceOf[TinyTree]
                   .getCopiedFrom != null) {
        val original: IAccumulatorData = getAccumulatorData(
          doc.asInstanceOf[TinyTree].getCopiedFrom.getTreeInfo,
          acc,
          context)
        new PathMappedAccumulatorData(original,
                                      doc.asInstanceOf[TinyTree].getCopiedFrom)
      } else {
        val d: AccumulatorData = new AccumulatorData(acc)
        val c2: XPathContextMajor = context.newCleanContext()
        c2.setCurrentComponent(acc.getDeclaringComponent)
        try {
          d.buildIndex(doc.getRootNode, c2)
          map.put(acc, d)
          d
        } catch {
          case err: XPathException => {
            val failed: IAccumulatorData = new FailedAccumulatorData(acc, err)
            map.put(acc, failed)
            failed
          }

        }
      }
    }

  def addAccumulatorData(doc: TreeInfo,
                         acc: Accumulator,
                         accData: IAccumulatorData): Unit = {
    synchronized {
      var map: Map[Accumulator, IAccumulatorData] =
        accumulatorDataIndex.get(doc)
      if (map != null) {
        val data: IAccumulatorData = map.get(acc)
        if (data != null) {
          return
        }
      } else {
        map = new HashMap[Accumulator, IAccumulatorData]()
        accumulatorDataIndex.put(doc, map)
      }
      map.put(acc, accData)
    }
  }

}

// Copyright (c) 2018-2020 Saxonica Limited
// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is "Incompatible With Secondary Licenses", as defined by the Mozilla Public License, v. 2.0.
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
