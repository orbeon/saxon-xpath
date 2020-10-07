package org.orbeon.saxon.tree.util

import org.orbeon.saxon.model.Type

import org.orbeon.saxon.om.NodeInfo

import org.orbeon.saxon.pattern._

import org.orbeon.saxon.tree.iter.AxisIterator

import java.util.function.Predicate

object SteppingNavigator {

  def getFollowingNode[N <: SteppingNode[N]](start: N, anchor: N): N = {
    var nodei: N = start.getFirstChild
    if (nodei != null)
      return nodei
    if (start.isSameNodeInfo(anchor))
      return null.asInstanceOf[N]
    nodei = start
    var parenti: N = start.getParent
    do {
      nodei = nodei.getNextSibling
      if (nodei != null)
        return nodei
      else if (parenti.isSameNodeInfo(anchor))
        return null.asInstanceOf[N]
      nodei = parenti
      parenti = parenti.getParent
    } while (parenti != null)
    null.asInstanceOf[N]
  }

  private trait Stepper[N <: SteppingNode[N]] {

    def step(node: N): N

  }

  private class FollowingNodeStepper[N <: SteppingNode[N]](var anchor: N)
    extends Stepper[N] {

    def step(node: N): N = getFollowingNode(node, anchor)

  }

  private class FollowingFilteredNodeStepper[N <: SteppingNode[N]](
                                                                    var anchor: N,
                                                                    var test: Predicate[_ >: NodeInfo])
    extends Stepper[N] {

    def step(node: N): N = {
      var nde = node
      do nde = getFollowingNode(nde, anchor) while (nde != null && !test.test(nde))
      nde
    }

  }

  private class FollowingElementStepper[N <: SteppingNode[N]](
                                                               var anchor: N,
                                                               var uri: String,
                                                               var local: String)
    extends Stepper[N] {

    def step(node: N): N = node.getSuccessorElement(anchor, uri, local)

  }

  private class FollowingFingerprintedElementStepper[N <: SteppingNode[N]](
                                                                            var anchor: N,
                                                                            var fingerprint: Int)
    extends Stepper[N] {

    def step(node: N): N = {
      var nde = node
      do nde = getFollowingNode(nde, anchor) while (nde != null && nde.getFingerprint != fingerprint);
      nde
    }

  }

  class DescendantAxisIterator[N <: SteppingNode[N]](
                                                      private var start: N,
                                                      includeSelf: Boolean,
                                                      test: Predicate[_ >: NodeInfo])
    extends AxisIterator {

    private var current: N = _

    private var stepper: Stepper[N] = _

    if (!(includeSelf && test.test(start))) {
      current = start
    }

    if (test == null || test == AnyNodeTest.getInstance) {
      stepper = new FollowingNodeStepper(start)
    } else if (test.isInstanceOf[NameTest]) {
      if (test.asInstanceOf[NameTest].getPrimitiveType == Type.ELEMENT) {
        val nt: NameTest = test.asInstanceOf[NameTest]
        stepper =
          if (start.hasFingerprint)
            new FollowingFingerprintedElementStepper(start, nt.getFingerprint)
          else
            new FollowingElementStepper(start,
              nt.getNamespaceURI,
              nt.getLocalPart)
      } else {
        stepper = new FollowingFilteredNodeStepper(start, test)
      }
    } else if (test.isInstanceOf[NodeKindTest]) {
      stepper =
        if (test.asInstanceOf[NodeKindTest].getPrimitiveType == Type.ELEMENT)
          new FollowingElementStepper(start, null, null)
        else new FollowingFilteredNodeStepper(start, test)
    } else if (test.isInstanceOf[LocalNameTest]) {
      if (test.asInstanceOf[LocalNameTest].getPrimitiveType == Type.ELEMENT) {
        val nt: LocalNameTest = test.asInstanceOf[LocalNameTest]
        stepper = new FollowingElementStepper(start, null, nt.getLocalName)
      } else {
        stepper = new FollowingFilteredNodeStepper(start, test)
      }
    } else if (test.isInstanceOf[NamespaceTest]) {
      if (test.asInstanceOf[NamespaceTest].getPrimitiveType == Type.ELEMENT) {
        val nt: NamespaceTest = test.asInstanceOf[NamespaceTest]
        stepper = new FollowingElementStepper(start, nt.getNamespaceURI, null)
      } else {
        stepper = new FollowingFilteredNodeStepper(start, test)
      }
    } else {
      stepper = new FollowingFilteredNodeStepper(start, test)
    }

    def next(): N = {
      if (current == null) {
        current = start
        return start
      }
      val curr: N = stepper.step(current)
      current = curr
      current
    }

  }

}
